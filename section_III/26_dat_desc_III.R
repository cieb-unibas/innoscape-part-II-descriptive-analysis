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

# packages for gender classification -------------------------------------------
library("keras")
library("tensorflow")
library("reticulate")

# packages for prism-name API: -------------------------------------------------
library("jsonlite")
library("httr")

# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

# Version of OECD-data ---------------------------------------------------------
# vers <- c("201907_")
vers <- c("202001_")

print("Directories are set and packages are loaded")

#######################################
############ Load data sets ###########
#######################################

## Load data on technology field -----------------------------------------------
q_new <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds")) %>% 
  distinct(p_key, tech_field) %>% 
  mutate(p_key = as.character(p_key))

## Load regions of patents' inventors ------------------------------------------
# firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg.rds")) 
# firm_reg <- distinct(firm_reg, p_key, organization, country, .keep_all = T)
# firm_reg <- dplyr::rename(firm_reg, ctry_code = country)
# setDT(firm_reg)[, share_firm := 1/.N, .(p_key)] #Q: Should we divide an invention by the number of regions or should we attribute each region a value to 1?
# firm_reg <- mutate(firm_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"))
# firm_reg <- left_join(firm_reg, q_new, by = c("p_key"))

## Load names and regions of patents' inventors --------------------------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds")) 
inv_reg <- dplyr::rename(inv_reg, ctry_code = Ctry_code)
setDT(inv_reg)[, share_inv := 1/.N, .(p_key)] 
inv_reg <- mutate(inv_reg, 
                  conti = countrycode(ctry_code, origin = "eurostat", 
                                      destination = "continent"), 
                  ctry_name = countrycode(ctry_code, origin = "eurostat", 
                                          "country.name.en"))
inv_reg <- left_join(inv_reg, q_new, by = c("p_key"))
p_year <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/dat_p_year.rds")  %>% 
  mutate(p_key = as.character(p_key))  %>% 
  dplyr::distinct(p_key, p_year)

## add data on gender for USPTO patents inventors ------------------------------
gender_path <- paste0(mainDir1,"/raw data/inventor_gender.tsv")
gender <- read.table(gender_path, sep = "\t", header = TRUE, quote = "")
gender$male <- as.character(gender$male)
gender <- gender %>%
  select(disamb_inventor_id_20191231, male) %>%
  distinct(disamb_inventor_id_20191231, .keep_all = TRUE) %>%
  rename(inventor_id = disamb_inventor_id_20191231,
         gender = male)

# subset to inventors whose gender is clearly defined and merge to full dataset
gender <- subset(gender, gender %in% c("0", "1"))
inv_reg <- left_join(x = inv_reg, y = gender, by = "inventor_id")
paste("number of inventors with gender information:", nrow(gender))

# subset to unique inventors...
# USPTO have inventor_id but European dont have one
# check with CR if these are all unique inventors!!
# if not, check for multiple matches

print("All necessary data is loaded")

##############################################
## Assign gender to inventors based on name ##
##############################################

# subset to inventors without gender information
gender <- subset(inv_reg, !gender %in% c("1", "0")) %>% select(name, gender)
gender_idx <- rownames(gender) # keep index positions from "inv_reg"

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
rep_vec <- c("o", "e", "u", "a", "o", "i", "o", "c", "e", "i", "e", "n", "o", "a", "i", "a", "u", "a", "a",
             "o", "ae", "a", "u", "e", "o", "y", "u", "a", "i", "y", "d", "s", "s", "n", "l", "s", "ss",
             "d", "z", "l", "e", "z")
data.frame(spec = special_chars, rep = rep_vec)

# drop NA's from sample
NA_obs <- which(is.na(first_names) == TRUE)
first_names <- first_names[-NA_obs]
gender <- gender[-NA_obs, ]
gender_idx <- rownames(gender)
if(length(first_names[is.na(first_names) == TRUE]) != 0){
  warning("NA names in the sample")}else{paste("NA names cleared")}

## define a random sub-sample for analysis (save computing time)
keep_obs <- sample(nrow(gender), 100000)
first_names <- first_names[keep_obs]
gender <- gender[keep_obs, ]
gender_idx <- rownames(gender) # keep index positions from "inv_reg"

## clean special characters from "first_names"
first_names <- unlist(lapply(
  first_names, function(x) stri_replace_all_fixed(str = x, 
                                               pattern = special_chars,
                                               replacement = rep_vec,
                                               vectorize_all = FALSE)))

print("first_names now only consist of 26 lowercase latin letters")

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

## load the trained LSTM model -------------------------------------------------
gender_model <- load_model_hdf5(
  paste0(mainDir1, "/created_models/gender_classification_LSTM_model.h5")
  )

## predict the inventors' gender ----- -----------------------------------------

# gender_prob <- gender_model %>% predict_proba(first_names_encoded)
gender$gender <- gender_model %>% predict_classes(first_names_encoded)

print("Gender information has been added.")

#########################################################
## Analysis: Gender & patenting in different regions   ##
#########################################################

# predicted gender + existing gender
tmp <- inv_reg[gender_idx, ]
tmp$gender <- gender$gender
t <- inv_reg %>% subset(gender %in% c("1", "0")) %>% sample_n(500000)
tmp <- rbind(tmp, t)
t <- NULL
tmp <- subset(tmp, tech_field == 16)
table(tmp$gender)/nrow(tmp)

# female inventor shares over time per country
plot_data <- tmp %>% group_by(ctry_code, p_year) %>%
  summarise(female_share = 1 - sum(as.numeric(gender))/n(),
            total_count = n()) %>%
  select(ctry_code, p_year, total_count, female_share)
plot_data <- subset(plot_data, ctry_code %in% c("US", "JP", "DE", "FR", "UK") &
                    as.numeric(p_year) > 1990 & total_count > 10)
ggplot(plot_data, aes(x = as.numeric(p_year), y = female_share, color = ctry_code))+
  geom_line()

# how many patents do females invent? how many do males invent?
plot_data <- tmp %>% group_by(name) %>% summarize(n_patent = n())
plot_data <- left_join(plot_data, tmp[, c("name", "gender")], by = "name")
n_females <- nrow(plot_data[plot_data$gender == "0", ])
n_males <- nrow(plot_data) - n_females
plot_data <- plot_data %>% group_by(n_patent, gender)%>% summarize(share = n())
plot_data[plot_data$gender == 0, "share"]/n_females
plot_data[plot_data$gender == 1, "share"]/n_males
# => more females only have 1 patent

# is the share of female inventors higher among highly cited patents?
plot_data <- tmp %>% subset(num_cited == 2)
table(plot_data$gender) / nrow(plot_data) # => slightly higher

# distribution of female inventors by patent
plot_data <- tmp %>% group_by(p_key) %>% summarise(
  female_share = 1 - sum(as.numeric(gender))/n())
ggplot(plot_data, #[plot_data$female_share > 0, ], 
       aes(female_share))+
  geom_density(fill = "#420A68FF", colour = "#420A68FF", alpha = 0.3)
#  geom_bar(fill = "#420A68FF", colour = "#420A68FF", alpha = 0.3)

# share of patents with female inventors
plot_data$female <- ifelse(plot_data$female_share > 0, 1, 0)
table(plot_data$female)/nrow(plot_data)


#########################################################
## Assign ethnic groups and nationalities to inventors ##
#########################################################

print("Number of unique inventors:")
inv_reg %>%
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
test_df <- inv_reg[sample(nrow(inv_reg), 10), ]

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





