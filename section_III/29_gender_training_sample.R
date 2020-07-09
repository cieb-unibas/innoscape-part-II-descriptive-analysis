cat("29: Create sample for training a neural network that classifies gender using names as input.
    (MN 02.07.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("countrycode")

# set directories --------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}

#######################################
############ Load data sets ###########
#######################################

## Load data on technology field -----------------------------------------------
q_new <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds")) %>% 
  distinct(p_key, tech_field) %>% 
  mutate(p_key = as.character(p_key))

## Load data on names of patents' inventors ------------------------------------
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

## Load data on gender of USPTO patent inventors ------------------------------
gender_path <- paste0(mainDir1,"/raw data/inventor_gender.tsv")
gender <- read.table(gender_path, sep = "\t", header = TRUE, quote = "")
gender$male <- as.character(gender$male)
gender <- gender %>%
  select(disamb_inventor_id_20191231, male) %>%
  distinct(disamb_inventor_id_20191231, .keep_all = TRUE) %>%
  rename(inventor_id = disamb_inventor_id_20191231,
         gender = male)
gender <- subset(gender, gender %in% c("0", "1")) # only keep those observations that are clearly defined

## Summarize and merge the data
paste("number of inventors with gender information:", nrow(gender))
paste("share of male inventors:", 
      nrow(gender[gender$gender == "1", ])/nrow(gender))
paste("share of female inventors:", 
      nrow(gender[gender$gender == "0", ])/nrow(gender))
gender <- left_join(x = inv_reg, y = gender, by = "inventor_id")
gender <- select(gender,
                 p_key, inventor_id, name, conti, ctry_name, gender) # keep IDs in case we have to link it again
gender <- gender[complete.cases(gender), ]
print("All necessary datas is loaded")

#########################################################
### Create sample for training a classification model ###
#########################################################

## Balance origin and gender in the sample
prop.table(table(gender$gender))*100
# 1) The ratio between male and female names is strongly unbalanced:
# => create a 50-50 sample on men and women

prop.table(table(gender$conti))*100
prop.table(table(inv_reg$conti))*100 # compare to full inventor data
# 2) Origin ratios could be biased towards Americas, because of USPTO patents
# => Europe is a underrepresented. Africa does almost not exist in both datasets
# However, all African inventors are from South Africa
# and most of their names are English / European names. The same is true for Oceania
prop.table(table(subset(gender, 
                        conti == "Africa")$ctry_name))
subset(gender, conti == "Africa") %>% sample_n(15)
subset(gender, conti == "Oceania") %>% sample_n(15)

# Thus: 
# 1) upsample female names to 50%
# 2) upsample Europe to 27.5%, downsample Americas to 44%, fix Asia at 27.5%, Africa/Oceania at 1%

## Define a function to create the dataset

train_set_fun <- function(df, N = 100000, gender_ratio = 0.5,
                          frac_Americas = 0.44, frac_Europe = 0.275, 
                          frac_Asia = 0.275, frac_RoW = 0.01){
  
  # Americas female:
  df_out <- subset(df, conti == "Americas" & gender == "0") %>%
    sample_n(gender_ratio * frac_Americas * N)
  
  # Americas male:
  tmp <- subset(df, conti == "Americas" & gender == "1") %>%
    sample_n(gender_ratio * frac_Americas * N)
  df_out <- bind_rows(df_out, tmp)
  
  # Europe female:
  tmp <- subset(df, conti == "Europe" & gender == "0") %>%
    sample_n(gender_ratio * frac_Europe * N)
  df_out <- bind_rows(df_out, tmp)
  
  # Europe male:
  tmp <- subset(df, conti == "Europe" & gender == "1") %>%
    sample_n(gender_ratio * frac_Europe * N)
  df_out <- bind_rows(df_out, tmp)
  
  # Asia female:
  tmp <- subset(df, conti == "Asia" & gender == "0") %>%
    sample_n(gender_ratio * frac_Asia * N)
  df_out <- bind_rows(df_out, tmp)
  
  # Asia male:
  tmp <- subset(df, conti == "Asia" & gender == "1") %>%
    sample_n(gender_ratio * frac_Asia * N)
  df_out <- bind_rows(df_out, tmp)
  
  # RoW female:
  tmp <- subset(df, conti %in% c("Africa", "Oceania") & gender == "0") %>%
    sample_n(gender_ratio * frac_RoW * N)
  df_out <- bind_rows(df_out, tmp)
  
  # RoW male:
  tmp <- subset(df, conti %in% c("Africa", "Oceania") & gender == "1") %>%
    sample_n(gender_ratio * frac_RoW * N)
  df_out <- bind_rows(df_out, tmp)
  
  # TEST if N = 100'000 and throw an error if it does not match
  if(nrow(df_out) != N){stop("Size of the data.frame does not correspond to N")}
  
  # shuffle the data.frame
  df_out <- df_out[sample(nrow(df_out)), ]
  rownames(df_out) <- NULL 
  
  return(df_out)
}

set.seed(02072020)
gender_train <- train_set_fun(df = gender)

## summarize the created sample
prop.table(table(gender_train$gender))*100
prop.table(table(gender_train$conti))*100
paste("number of observations:", nrow(gender_train))

## save for training the network
# saveRDS(gender_train,
#         paste0(getwd(),"/section_III/gender_training_sample.rds"))



