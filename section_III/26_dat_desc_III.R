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
grad_dat$COUNTRY <- countrycode(grad_dat$COUNTRY, "iso3c", "iso2c")
grad_dat <- rename(grad_dat, Ctry_code = COUNTRY, p_year = YEAR, 
                   N_female_graduates = Value)
grad_dat <- select(grad_dat, - SEX)
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

################################################################
##### Assign gender to inventors based on their first name #####
################################################################

# subset to those inventors without gender information
gender <- subset(inv_reg, !gender %in% c("1", "0")) %>% select(name, gender)
gender_idx <- rownames(gender) # keep index positions from "inv_reg"
print(paste0(nrow(gender)," inventors' gender has to be predicted based on first name"))

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
print(paste("First names encoded as 3D-tensor of shape:", 
            paste(dim(first_names_encoded), collapse = ", ")
            )
      )

## load the trained LSTM model -------------------------------------------------
gender_model <- load_model_hdf5(
  paste0(mainDir1, "/created_models/gender_classification_LSTM_model.h5")
  ) # if this throws an error, reload keras, tensorflow and reticulate and reload

## predict the inventors' gender ----- -----------------------------------------
gender$gender <- gender_model %>% predict_classes(first_names_encoded) # takes around 5min
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

################################################################################
######## create datasets for regions/countries for subsequent analysis #########
################################################################################

## clean the data --------------------------------------------------------------
inv_reg <- inv_reg[is.na(inv_reg$gender) == FALSE, ] # drop NA's
inv_reg$gender <- as.numeric(inv_reg$gender)
inv_reg$p_year <- as.numeric(inv_reg$p_year)
dat <- inv_reg # to keep original inv_reg dataframe

## For robustness checks: subset to inventors with observed gender status
# dat <- inv_reg[!rownames(inv_reg) %in% gender_idx, ]

## overall gender shares among patent inventors --------------------------------
table(inv_reg$gender)/nrow(inv_reg) 
# => slightly lower gender share when considering USPTO inventors only

## calculate shares of female inventors per region and p_year
female_inv_shares_reg <- dat %>% 
  group_by(Up_reg_code, Ctry_code, p_year) %>%
  summarise(total_inventors_reg = n(),
            female_inventors_reg = sum(gender == 0, na.rm = TRUE),
            female_inventor_share_reg = female_inventors_reg / total_inventors_reg)%>%
  filter(total_inventors_reg > 30)
female_inv_shares_reg <- merge(female_inv_shares_reg, 
                               inv_reg[!duplicated(inv_reg$Up_reg_code), c("Up_reg_code", "Up_reg_label")],
                               by = "Up_reg_code", all.x = TRUE)
# => relatively many observations have NA as regional code
# => use this for a table

## calculate shares of female inventors per country and p_year:
female_inv_shares_ctry <- dat %>% 
  group_by(Ctry_code, p_year) %>%
  summarise(total_inventors_ctry = n(),
            female_inventors_ctry = sum(gender == 0, na.rm = TRUE),
            female_inventor_share_ctry = female_inventors_ctry / total_inventors_ctry)%>%
  filter(total_inventors_ctry > 30)

## merge female graduate shares and female inventor shares together ------------
female_inv_shares_ctry <- merge(female_inv_shares_ctry, grad_dat, 
                    by = c("Ctry_code", "p_year"), all = TRUE)
female_inv_shares_ctry$Ctry_label <- countrycode(female_inv_shares_ctry$Ctry_code,
                                                 "iso2c", "country.name")

# overall female shares among graduates (2010-2017)
tmp <- female_inv_shares_ctry %>%
  group_by(Ctry_code, FIELD, ISC11_LEVEL) %>%
  summarise(N_female_graduates_overall = sum(N_female_graduates, na.rm = TRUE),
            total_graduates_overall = sum(total_graduates, na.rm = TRUE),
            female_share_graduates_overall = N_female_graduates_overall / total_graduates_overall,
            female_inventor_share_ctry_overall = sum(female_inventors_ctry, na.rm = TRUE) /
              sum(total_inventors_ctry, na.rm = TRUE))
female_inv_shares_ctry <- merge(female_inv_shares_ctry, tmp, 
                                by = c("Ctry_code", "FIELD", "ISC11_LEVEL"),
                                all.x =TRUE)
tmp <- NULL
female_inv_shares_ctry <- mutate(female_inv_shares_ctry,
       inventor_graduate_ratio = female_inventor_share_ctry / female_share_graduates_overall)
gender_dat <- list(female_inv_shares_reg, female_inv_shares_ctry)
names(gender_dat) <- c("region", "country")

## save the dataset for report -------------------------------------------------
# dat <- list(dat)
#saveRDS(dat, paste0(getwd(),"/report/gender_nationalities.rds"))

## Premliminary Analysis -------------------------------------------------------

## Plot female inventor shares per country over time:
selected_countries <- c("CH", "US", 
                        "CN", "KR", "JP", "TW", "SG", "HK")
plot_data <- gender_dat[["country"]] %>%
  filter(Ctry_code %in% selected_countries,
         p_year >= 1990, p_year < 2017,
         total_inventors_ctry > 30)
plot_data$isoyear <- paste0(plot_data$Ctry_code, plot_data$p_year)
plot_data <- plot_data[!duplicated(plot_data$isoyear), ]
ggplot(plot_data, aes(x = p_year, y = female_inventor_share_ctry, color = Ctry_code))+
  geom_line()+geom_point(aes(shape = Ctry_code))+ylim(c(0, 0.7))

## overall share of female biology graduates between 2010-2017
plot_data <- filter(gender_dat[["country"]] , FIELD == "F051", ISC11_LEVEL == "L7")
plot_data <- plot_data[!duplicated(plot_data$Ctry_code), ]
plot_data %>% select(Ctry_code, female_share_graduates_overall) %>% 
  arrange(desc(female_share_graduates_overall))

## plot shares of female graduates over time
selected_countries <- c("CH", "US", "DE", "DK", "NL", "AT")
plot_data <- gender_dat[["country"]] %>%
  filter(Ctry_code %in% selected_countries,
         FIELD == "F051",
         ISC11_LEVEL == "L7",
         p_year >= 1990,
         total_graduates > 50) %>%
  distinct(Ctry_code, .keep_all = TRUE)
ggplot(plot_data, aes(y = female_share_graduates_overall, x= Ctry_code, fill = Ctry_code))+
  geom_col()+ylim(0,1)+
  ggtitle(paste0("Female share of graduates in ", plot_data$Field[1], 
                 " (ISC-Level ", plot_data$ISC11_LEVEL[1],")"))

## plot ratios over time
# selected_countries <- c("CH", "US", "DE", "DK", "NL", "AT")
# plot_data <- gender_dat[["country"]] %>%
#   filter(Ctry_code %in% selected_countries,
#          ISC11_LEVEL == "L7",
#          FIELD == "F051",
#          p_year >= 1990,
#          total_graduates > 50,
#          total_inventors_ctry > 30)
# ggplot(plot_data, aes(x = p_year, y = inventor_graduate_ratio, color = Ctry_code))+
#   geom_line()+geom_point(aes(shape = Ctry_code))+
#   ylim(c(0.2, 1))
# => not very interesting because it is rather stable

## correlation plot graduates vs. inventors
selected_countries <- c("CH", "US", "DE", "DK", "NL", "AT")
plot_data <- gender_dat[["country"]] %>%
  filter(#Ctry_code %in% selected_countries,
         FIELD == "F051", ISC11_LEVEL == "L7",
         p_year >= 2010,
         total_graduates > 50,
         total_inventors_ctry > 30) %>%
  distinct(Ctry_code, .keep_all = TRUE)
ggplot(plot_data, aes(x = female_share_graduates_overall, 
                      y = female_inventor_share_ctry_overall))+
  geom_point(aes(size = total_graduates), alpha = 0.5, color = "steelblue")+
  geom_text(aes(label = Ctry_code), size = 3, nudge_y = 0.01, nudge_x = -0.01)+
  xlim(0.4, 0.85)+ylim(0.2, 0.6)+theme(legend.position = "none")+
  geom_vline(xintercept = mean(plot_data$female_share_graduates_overall), 
             linetype = "dotted")+
  geom_hline(yintercept = mean(plot_data$female_inventor_share_ctry_overall), 
             linetype = "dotted")
# => could also add here a color that indicates how familiy friendly an economy is.

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





