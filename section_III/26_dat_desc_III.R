cat("26: Create data for Section III.: 
    Determine innovator origin and gender in Pharma industries using inventor names
    (MN 23.06.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("countrycode")
library("stringi")
library("OECD")

# packages for gender classification -------------------------------------------
library("keras")
library("tensorflow")
library("reticulate")

# packages for prism-name API: -------------------------------------------------
library("jsonlite")
library("httr")

# Version of OECD-data ---------------------------------------------------------
# vers <- c("201907_")
vers <- c("202001_")

# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}


#######################################
############ Load data sets ###########
#######################################

## Load data on gender shares of university graduates --------------------------
grad_dat <- read.csv(paste0(getwd(), "/section_III/oecd_graduates.csv"))
names(grad_dat)[1] <- "COUNTRY"
total_year <- grad_dat %>% filter(SEX == "T") %>% 
  select(COUNTRY, FIELD, Field, ISC11_LEVEL, YEAR, Value) %>%
  rename(total_graduates = Value)
grad_dat <- grad_dat %>% filter(SEX == "F") %>% 
  select(COUNTRY, SEX, FIELD, ISC11_LEVEL, YEAR, Value)
grad_dat <- merge(grad_dat, total_year, 
                  by = c("COUNTRY", "FIELD", "ISC11_LEVEL", "YEAR"),
                  all.x = TRUE)
total_year <- NULL
grad_dat <- mutate(grad_dat, female_share_graduates = Value / total_graduates)
grad_dat <- grad_dat[is.nan(grad_dat$female_share_graduates) == FALSE &
                       is.na(grad_dat$female_share_graduates) == FALSE, ]
print("Data on university graduates loaded")

## Load names and regions of pharma patents' inventors -------------------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg_16.rds")) 
print("Names of patent inventors loaded")

## add data on gender for USPTO patents inventors ------------------------------
gender_path <- paste0(mainDir1,"/raw data/inventor_gender.tsv")
gender <- read.table(gender_path, sep = "\t", header = TRUE, quote = "")
gender$male <- as.character(gender$male)
gender <- gender %>%
  select(disamb_inventor_id_20191231, male) %>%
  distinct(disamb_inventor_id_20191231, .keep_all = TRUE) %>%
  rename(inventor_id = disamb_inventor_id_20191231,
         gender = male)
gender <- subset(gender, gender %in% c("0", "1")) # subset to clearly defined gender
paste("number of inventors with gender information:", nrow(gender))

## merge gender to names
inv_reg <- left_join(x = inv_reg, y = gender, by = "inventor_id")
print("All necessary data is loaded")

##############################################
## Assign gender to inventors based on name ##
##############################################

# subset to those inventors without gender information
gender <- subset(inv_reg, !gender %in% c("1", "0")) %>% select(name, gender)
gender_idx <- rownames(gender) # keep index positions from "inv_reg"
print(paste0(nrow(gender)," inventors' gender has to be predicted based on name"))

## data processing -------------------------------------------------------------
first_names <- stri_extract_first_words(gender$name) # first names only
first_names <- tolower(first_names)

# remove punctuation and numbers
first_names <- gsub("[[:punct:]]", "", first_names)
first_names <- gsub("[0-9]", "", first_names)

# search for special characters and replace them from 'first_names'
special_chars <- stri_extract_all(str = first_names, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)[-1]
special_chars

# create a replacement vector and replace special_chars
repl_vec <- c("e", "ue", "o", "oe", "a", "o", "i", "e", "i", "a", "c", "a", "ae",
              "u", "o", "o", "i", "e", "a", "n", "a", "e", "o", "a", "u", 
              "ae", "u", "d", "l", "i", "b")
#repl_vec <- c("oe", "e")
data.frame(spec = special_chars, rep = repl_vec)

# drop NA's from sample
NA_obs <- which(is.na(first_names) == TRUE)
if(length(NA_obs) > 0){
  first_names <- first_names[-NA_obs]
  gender <- gender[-NA_obs, ]
  gender_idx <- rownames(gender)
}
if(length(first_names[is.na(first_names) == TRUE]) != 0){
  warning("Some first names in the sample are NA")}else{paste("No first names are NA")}

## clean special characters from "first_names"
first_names <- unlist(lapply(
  first_names, function(x) stri_replace_all_fixed(str = x, 
                                               pattern = special_chars,
                                               replacement = repl_vec,
                                               vectorize_all = FALSE)))

print("first names now only consist of 26 lowercase latin letters")

## construct vocabulary and sequence length ------------------------------------
char_dict <- c(letters, "END") # => this is the vocab that was used for training
n_chars <- length(char_dict)
max_char <- 19 # the model was trained on a maximum length 19 characters

## transform inventors' first names into one-hot-encoded tensors ---------------
source(paste0(getwd(), "/section_III/names_encoding_function.R")) # load encoding function
first_names_encoded <- encode_chars(names = first_names, 
                                    seq_max = max_char, 
                                    char_dict = char_dict,
                                    n_chars = n_chars)
dim(first_names_encoded)
print("First names encoded as 3D-tensors.")

## load the trained LSTM model -------------------------------------------------
gender_model <- load_model_hdf5(
  paste0(mainDir1, "/created_models/gender_classification_LSTM_model.h5")
  ) # if these throws an error, reload keras, tensorflow and reticulate and retry

## predict the inventors' gender ----- -----------------------------------------
gender$gender <- gender_model %>% predict_classes(first_names_encoded)
inv_reg[gender_idx, "gender"] <- gender$gender
print("Gender information added to inventor data")

# ## Robustness tests:
# dat <- inv_reg[!rownames(inv_reg) %in% gender_idx, ]
# set.seed(22072020)
# dat <- dat[sample(nrow(dat), 1000), c("name", "gender")]
# first_names <- stri_extract_first_words(dat$name)
# # re-run the cleaning and encoding code above
# dat$pred_gender <- as.numeric(gender_model %>% predict_classes(first_names_encoded))
# dat$correct <- dat$gender == dat$pred_gender
# dat[dat$correct == FALSE, ]



####################################################
## Analysis: Gender shares among patent inventors ##
####################################################

## clean the data --------------------------------------------------------------
inv_reg <- inv_reg[is.na(inv_reg$gender) == FALSE, ] # drop NA's
inv_reg$gender <- as.numeric(inv_reg$gender)
inv_reg$p_year <- as.numeric(inv_reg$p_year)
dat <- inv_reg

## For robustness checks: subset to inventors with observed gender status
dat <- inv_reg[!rownames(inv_reg) %in% gender_idx, ]

## overall gender shares among patent inventors: --------------------------------
table(dat$gender)/nrow(dat) # => slightly lower gender share with USPTO inventors only

## share of female inventors per country and p_year ----------------------------
female_inv_shares <- dat %>% 
  group_by(Ctry_code, p_year) %>%
  summarise(total_inventors = n(),
            female_inventor_share = 1 - sum(gender, na.rm = TRUE) / total_inventors)
female_inv_shares[318, ] # example
cat("In 2005, 834 Swiss residents contributed to a patent. 
Among those 20.5% were female.")

# Plot shares per country over time:
plot_data <- female_inv_shares %>%
  filter(Ctry_code %in% c("CH", 
                          "US", "JP",
                          "DE", "AT", "GB", "NL", "BE"),
                          # "NO", "SE", "DK", "FI"),
                          # "FR", "IT", "ES", "PT",
                          # "CZ", "PL", "SK", "HU"),
         p_year >= 1990,
         total_inventors > 30)
ggplot(plot_data, aes(x = p_year, y = female_inventor_share, color = Ctry_code))+
  geom_line()+ylim(c(0, 0.7))

## share of female biology graduates between 2010-2017 -------------------------
female_grad_shares <- filter(grad_dat, FIELD == "F051")
female_grad_shares %>% filter(ISC11_LEVEL == paste0("L", 8)) %>%
  group_by(COUNTRY, Field) %>%
  summarise(total_graduates = sum(total_graduates, na.rm = TRUE),
            female_share = sum(Value, na.rm = TRUE)/sum(total_graduates, na.rm = TRUE),
            n_year = n()) %>%
  filter(n_year > 5) %>%
  arrange(desc(female_share))

# plot female shares over time
plot_data <- female_grad_shares %>%
  filter(COUNTRY %in% c("CHE",
                        "USA", "JPN",
                        # "DEU", "AUT", "GBR", "NLD", "BEL"),
                        # "NOR", "SWE", "DNK", "FIN"),
                        "FRA", "ITA", "ESP", "PRT"),
                        # "CZE", "POL", "SVK", "HUN"),
         ISC11_LEVEL == "L8",
         YEAR >= 1990,
         total_graduates > 50)
plot_data$Field <- paste(plot_data$FIELD, plot_data$Field)
ggplot(plot_data, aes(x = YEAR, y = female_share_graduates, color = COUNTRY))+
  geom_line()+geom_point()+
  ylim(c(0.3, 0.8))+
  ggtitle(paste0("Female share of graduates in ", plot_data$Field[1], 
                 " (ISC-Level ", plot_data$ISC11_LEVEL[1],")"))

## bring graduates and inventors together
grad_dat$COUNTRY <- countrycode(grad_dat$COUNTRY, "iso3c", "iso2c")
grad_dat <- rename(grad_dat, Ctry_code = COUNTRY, p_year = YEAR, 
                   N_female_graduates = Value)
grad_dat <- select(grad_dat, - SEX)
gender_dat <- merge(female_inv_shares, grad_dat, 
                        by = c("Ctry_code", "p_year"))
gender_dat <- mutate(gender_dat,
                     inventor_graduate_ratio = female_inventor_share / female_share_graduates)

# plot ratios over time
plot_data <- gender_dat %>%
  filter(Ctry_code %in% c("CH", 
                          "US",
                          "DE", "AT", "GB", "NL", "BE"),
                          # "NO", "SE", "DK", "FI",
                          # "FR", "IT", "ES", "PT"),
                          # "CZ", "PL", "SK", "HU"),
         ISC11_LEVEL == "L8",
         FIELD == "F051",
         p_year >= 1990,
         total_graduates > 50,
         total_inventors > 30)
plot_data$Field <- paste(plot_data$FIELD, plot_data$Field)
ggplot(plot_data, aes(x = p_year, y = inventor_graduate_ratio, color = Ctry_code))+
  geom_line()+geom_point()+
  ylim(c(0, 1.5))

## save the dataset for report -------------------------------------------------


#########################################################
## Assign ethnic groups and nationalities to inventors ##
#########################################################

print("Number of unique inventors:")
dat %>%
  distinct(name, .keep_all = TRUE) %>%
  nrow()


#### 'name-prism' API -------------------------------------
# http://www.name-prism.com/api
# https://github.com/cdcrabtree/nomine/tree/master/R
# API_nameprism <- "..."
# 
# ## specify the names to scrap and convert to percent-encoding: ---------------
# query_names <- c("Matthias Niggli", "Christian Rutzer", "Dragan Filimonovic")
# query_names <- gsub(" ", "%20", x = query_names)
# 
# ## define function to get information for a series of names
# name_to_nat <- function(df, query_names, pred_type = "nat", 
#                         response_format = "json"){
#   
#   df <- select(df, ) # keep p_key and name for left_join later
#   new_var <- paste("inv_", pred_type, sep = "")
#   df[, new_var] <- rep(NA, nrow(df))
#   
#   # define function to get information for a single name
#   inv_name_fun <- function(inv_name){
#     api_url <- paste("http://www.name-prism.com/", API_nameprism, "/",
#                      pred_type, "/", response_format, "/", 
#                      inv_name, sep = "")
#     tmp <- fromJSON(txt = api_url)
#     tmp <- httr::GET(url = api_url)
#     tmp <- read_html(url = api_url)
#     
#     return(tmp)
#   }
#   
#   # get information for a all the names from API
#   tmp <- lapply(query_names, function(x){
#     tryCatch(inv_name_fun(inv_name = x),
#              error = function(e) data.frame(rep(NA, )))
#     Sys.sleep(1) # wait 1 second for the next access
#     })
#   
#   tmp <- bind_rows(tmp)
#   
#   df[, new_var] <- tmp
#   
#   return(df)
#  }
# 
# 
# cat("choose from the following parameters:
#     \n(1) type: ", paste(c("nat", "eth"), collapse = ", "),
#     "\n(2) response format: ", paste(c("json", "csv"), collapse = ", ")
# )


#### 'namsor' API -------------------------------------
# https://v2.namsor.com/NamSorAPIv2/index.html

api_namsor <- "1f0a964700cbe94d6052afbab80fd8d7"
test_df <- dat[sample(nrow(dat), 10), ]

#query_names <- c("Matthias Niggli", "Christian Rutzer", "Dragan Filimonovic")
query_names <- test_df$name
first_names <- stri_extract_first_words(query_names)
last_names <- stri_extract_last_words(query_names)

i <- 1
#for(inv_name in 1:length(query_names)){

url <- paste("https://v2.namsor.com/NamSorAPIv2/api2/json/origin/",
             first_names[i], "/", last_names[i],
             sep = "")

res <- GET(url, add_headers("X-API-KEY" = api_namsor))
res <- fromJSON(content(res, as = "text"))
res
#}





