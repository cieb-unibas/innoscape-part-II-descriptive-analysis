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

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}

# reproducibility --------------------------------------------------------------
set.seed(06072020)

#######################################
############ Load datasets ############
#######################################

dat <- readRDS(paste0(getwd(),"/section_III/gender_training_sample.rds"))
dat <- dat %>% select(name, gender) # only use names as inputs
dat <- dat[sample(nrow(dat)), ] # shuffle the data
dat$name <- tolower(dat$name) # make everything lowercase

#######################################
########### gender encoding ###########
#######################################

y_dat <- as.numeric(dat$gender)

#######################################
######### character encoding ##########
#######################################

## maximum length and distribution of the number of characters per name:
max_char <- max(nchar(dat$name))
hist(nchar(dat$name), main = "", 
     xlab = "Number of characters per name")
# => two possibilities:
# 1) use "max_char" and padding with an "END" token indicating the end of the name
# 2) truncate at e.g. 35 characters to save computing time

## check if there are semicolons etc. and define character dictionary
special_chars <- c(",", ";", ".", "/", "-", " ")
lapply(special_chars, function(x){
  tmp <- grepl(x, dat$name, fixed = TRUE)
  if(length(tmp[tmp == TRUE] > 0)){paste(x, "exists")}
  else{paste(x, "does not exist")}
  })
char_dict <- c(letters, special_chars, "END")
n_chars <- length(char_dict)

## define function for one-hot-encoding all characters
encode_chars <- function(names, seq_max = max_char){
  
  N <- length(names)
  
  # Create 3D-Tensor with shape (No. of samples, max. name length, number of characters):
  tmp <- array(rep(0, N* n_chars * seq_max), 
               dim = c(N, seq_max, n_chars)
               ) 
  
  # iterate over all names
  for(i in 1:N){
    name <- names[[i]]
    
    # encode characters:
    for (char in 1:nchar(name)) {
      idx_pos <- which(char_dict == substr(name, char, char))
      tmp[i, char, idx_pos] <- 1
    }

    # padding:
    if(nchar(name) < seq_max){
    tmp[i, seq(nchar(name)+1, seq_max), which(char_dict == "END")] <- 1
    }
  }
  
  return(tmp)
}

# TEST
encode_chars(names = "bruce wayne")[1, , ]

# encode all names in the data and create a 3D-Tensor to train and evaluate the models
x_dat <- encode_chars(names = dat$name)

# summarize
paste("names are one-hot-encoded with shape: ", 
      paste0("(", paste(dim(x_dat), collapse = ", "), ")")
      )

################################################
######### split to train and test set ##########
################################################

train_frac <- 0.7 # fraction to train the model
N <- nrow(dat)
train_idx <- sample(seq(N), N * train_frac)

x_train <- x_dat[train_idx, , ]
y_train <- y_dat[train_idx]
x_val <- x_dat[-train_idx, , ]
y_val <- y_dat[-train_idx]

#########################################
############ build the model ############
#########################################

# start slow.. maybe 1 LSTM-Layer 
# if more neurons are included, use dropout to reduce overfitting
# one dense-layer at the end with softmax activation




















