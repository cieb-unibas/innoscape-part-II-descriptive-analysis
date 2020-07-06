cat("30: Preporcess the data and train algorithms to classify the gender of inventors based on names.
    (MN 02.07.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")

# packages for training the network: -------------------------------------------
library("tensorflow")
library("keras")


# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}

#######################################
############ Load data sets ###########
#######################################

dat <- readRDS(paste0(getwd(),"/section_III/gender_training_sample.rds"))
dat_0 <- dat %>% select(name, gender) # only use names as inputs

#######################################
######### encoding characters #########
#######################################

## maximum length and distribution of the number of characters per name:
max_char <- max(nchar(dat_0$name))
hist(nchar(dat_0$name), main = "", 
     xlab = "Number of characters per name")
# => two possibilities:
# 1) use "max_char" and padding with an "END" token indicating that the name is over
# 2) truncate at e.g. 35 characters to save computing time

## make everything lowercase
dat_0$name <- tolower(dat_0$name)

## check if there are semicolons etc.
special_chars <- c(",", ";", ".", "/", "-", " ")
lapply(special_chars, function(x){
  tmp <- grepl(x, dat_0$name, fixed = TRUE)
  if(length(tmp[tmp == TRUE] > 0)){paste(x, "exists")}else{paste(x, "does not exist")}
  })

## define function for one-hot-encoding of all characters
char_dict <- c(letters, special_chars, "END")
n_chars <- length(char_dict)

encode_chars <- function(name, seq_max = max_char){
  
  tmp <- matrix(rep(0, n_chars * seq_max), 
               nrow = seq_max, ncol = n_chars)
  
  for (char in 1:nchar(name)) {
    idx_pos <- which(char_dict == substr(name, char, char))
    tmp[char, idx_pos] <- 1
  }
  
  # indicate the end of a name with padding "END"
  tmp[seq(nchar(name)+1, seq_max), which(char_dict == "END")] <- 1
  
  return(tmp)
}

# TEST
encode_chars("matthias niggli", seq_max = max_char)

# encode all 100'000 names and create a tensor with it 
