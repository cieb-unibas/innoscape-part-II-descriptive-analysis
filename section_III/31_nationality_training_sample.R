cat("31: Create a (balanced) sample of names for accessing the name-prism API.
    (MN 20.07.2020)")

##################################################################
########### Load packages and set directories ####################
##################################################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("stringi")

# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}


#####################################################
#################### Load data ######################
#####################################################

#### load names and country of residence of patent inventors -------------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds"))
name_origin <- inv_reg %>% select(name, Ctry_code) %>% 
  distinct(name, .keep_all = TRUE) %>% na.omit()
paste("There are", nrow(name_origin), "unique inventors in the sample")

#########################################################
########### Country of residence distribution ###########
#########################################################

residence_frequencies <- table(name_origin$Ctry_code) / nrow(name_origin)
residence_frequencies <- as.data.frame(residence_frequencies) %>%
filter(Freq > 0.005) %>% rename(Country = Var1, Share = Freq)
residence_frequencies$Country <- as.character(residence_frequencies$Country)
residence_frequencies <- rbind(residence_frequencies, 
                               c("RoW", 1-sum(residence_frequencies$Share)))
residence_frequencies$Share <- round(as.numeric(residence_frequencies$Share), 
                                     digits = 3)

# illustrate:
ggplot(residence_frequencies, aes(x = Country, y = Share))+
  geom_col()
residence_frequencies <- NULL

##################################################################
############ Construct country of residence groups ###############
##################################################################

ctry_groups <- c("China", "India","French", rep("DACH", 3), "Japan", "Italy",
                "Korea", rep("Hispanic", 5), rep("Scandinavia", 4),
                rep("EastEurope", 4), rep("Balkans", 2), rep("Anglo-Saxon", 5), 
                rep("Portugese", 2), rep("Russian", 2))
ctry_codes <- c("CN", "IN", "FR", "DE", "AT", "CH", "JP", "IT", "KR", "ES", 
                "MX", "AR", "CO", "CL", "FI", "SE", "DK", "NO", "PL", "HU", 
                "SK", "CZ", "RS", "HR", "US" , "UK", "CA", "AU", "NZ", "PT", 
                "BR", "RU", "UA")
ctry_groups <- data.frame(ctry_groups = ctry_groups, ctry_codes = ctry_codes)

#### subset to countries of interest and assign the broader country-groups -----
name_origin_sub <- filter(name_origin, 
                          Ctry_code %in% ctry_codes)
name_origin_sub$ctry_group <- NA
name_origin_sub$idx <- as.numeric(rownames(name_origin_sub))

name_origin_sub <- lapply(ctry_codes, function(ctry){
  
  ctry_group <- as.character(ctry_groups[ctry_groups$ctry_codes == ctry, "ctry_groups"])
  tmp <- subset(name_origin_sub, Ctry_code == ctry)
  tmp$ctry_group <-ctry_group
  return(tmp)
})

name_origin_sub <- bind_rows(name_origin_sub)
name_origin_sub <- name_origin_sub[order(name_origin_sub$idx, 
                                         decreasing = FALSE), ]
rownames(name_origin_sub) <- NULL
name_origin_sub$idx <- NULL

#### check the number of observations per country-group
sample_info <- as.data.frame(table(name_origin_sub$ctry_group))
sample_info <- rename(sample_info, ctry_group = Var1, N = Freq)
sample_info

########################################################
################### create training data ###############
########################################################

# choose 100'000 inventors from the Anglo-Saxon countries
# for all other residence countries, choose the actual number of inventors or a threshold

n_thresh <- 25000 # set the threshold
sample_info <- mutate(sample_info,
              sample_N = ifelse(N > n_thresh, n_thresh, N))
sample_info[sample_info$ctry_group == "Anglo-Saxon", "sample_N"] <- 100000
sample_info$ctry_group <- as.character(sample_info$ctry_group)
paste("Total number of observations in the training data: ", sum(sample_info$sample_N))

#### sample from the countries of residence
train_set_fun <- function(df_in, ctry_list, samples_list, ctry_group){
  
  # find the number of observations to sample
  ctry_idx <- which(ctry_list == ctry_group)
  sample_N <- samples_list[ctry_idx]
  
  # subset to ctry_group and sample
  set.seed(20072020)
  df_out <- df_in[df_in$ctry_group == ctry_group, ]
  df_out <- sample_n(df_out, sample_N)

  return(df_out)
}

train_dat <- lapply(sample_info$ctry_group, function(x){
  train_set_fun(df_in = name_origin_sub,
                ctry_list = sample_info$ctry_group,
                samples_list = sample_info$sample_N,
                ctry_group = x)
  }
  )
train_dat <- bind_rows(train_dat)

# check if the distribution is correct
table(train_dat$ctry_group)
nrow(train_dat)

# clean
ctry_groups <- NULL
sample_info <- NULL
name_origin_sub <- NULL
ctry_groups <- NULL
ctry_codes <- NULL
n_thresh <- NULL

##############################################################
################ pre-process the training data ###############
##############################################################

#### remove punctuation
train_dat$name <- tolower(train_dat$name)
train_dat$name <- gsub("[[:punct:]]", "", train_dat$name)
train_dat$name <- gsub("[0-9]", "", train_dat$name)


#### identify all letters in the sample and replace special characters
special_chars <- gsub(" ", replacement = "", train_dat$name)
special_chars <- stri_extract_all(str = special_chars, regex = "[^a-z]")
special_chars <- unlist(special_chars)
special_chars <- unique(special_chars)
special_chars <- special_chars[is.na(special_chars) == FALSE]
special_chars

repl_vec <- c("c", "o", "e", "e", "ue", "oe", "i", "a", "n", "o", "e",
              "e", "ae", "a", "o", "ae", "i", "c", "ss", "o", "y", "i",
              "b", "a", "n", "u", "l", "e", "s", "a", "e", "o", "o", "z",
              "u", "u", "i", "z", "u", "a", "a", "l", "a")
data.frame(spec = special_chars, rep = repl_vec)

# clean special characters from "names"
train_dat$name <- unlist(lapply(
  train_dat$name, function(x) stri_replace_all_fixed(str = x, 
                                                  pattern = special_chars,
                                                  replacement = repl_vec,
                                                  vectorize_all = FALSE)))
train_dat <- rename(train_dat, full_name = name)
print("All special characters removed from names")

#### encode names with '%'
train_dat$full_name_encoded <- gsub(" ", "%20", x = train_dat$full_name)

#### add only first last name
train_dat$first_name <- stri_extract_first_words(train_dat$full_name)
train_dat$last_name <- stri_extract_last_words(train_dat$full_name)

first_last <- function(names){
  first_name <- stri_extract_first_words(names)
  last_name <- stri_extract_last_words(names)
  first_last <- paste(first_name, last_name)
  return(first_last)
}

train_dat$first_last <- first_last(train_dat$full_name)
train_dat$first_last_encoded <- gsub(" ", "%20", x = train_dat$first_last)

##############################################
################ save the data ###############
##############################################

saveRDS(train_dat,
        paste0(mainDir1,"/created data/nationality_training_sample.rds"))








