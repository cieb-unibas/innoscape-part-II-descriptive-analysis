cat("30: Preporcess the data and train algorithms to classify the gender of inventors based on names.
    (MN 02.07.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("stringi")

# packages for training the network: -------------------------------------------
library("tensorflow")
library("keras")
library("reticulate")

# directories  -----------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
        print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
                print("Directories are set and packages are loaded")}

# reproducibility --------------------------------------------------------------
set.seed(06072020)

##################################################
############ Load and preprocess data ############
##################################################

dat <- readRDS(paste0(getwd(),"/section_III/gender_training_sample.rds"))
dat <- dat %>% select(name, gender) # only use names as inputs
dat <- dat[sample(nrow(dat)), ] # shuffle the data
dat$name <- tolower(dat$name) # reduce the number of features.

## remove all punctuation ------------------------------------------------------
dat$name <- gsub("[[:punct:]]", "", dat$name)
# check
special_chars <- c(",", ";", ".", "/", "-")
lapply(special_chars, function(x){
        tmp <- grepl(x, "", dat$name, fixed = TRUE)
        if(length(tmp[tmp == TRUE] > 0)){paste(x, "exists")}
        else{paste(x, "does not exist")}
})

## first names only ------------------------------------------------------------

# Option 1: only keep first word
dat$name <- stri_extract_first_words(dat$name)

# Option 2: keep everything except the last word (assumed to be the last name)
# remove_lastName <- function(name){
# 
#   name_split <- strsplit(name, " ")
# 
#   first_name <- name_split[[1]][-length(name_split[[1]])]
#   first_name <- paste(first_name, collapse = " ")
# 
#   return(first_name)
# }
# dat$name <- lapply(dat$name, remove_lastName)

#######################################
########### gender encoding ###########
#######################################

y_dat <- as.numeric(dat$gender)

#####################################################
######### tokenization: character encoding ##########
#####################################################

## maximum length and distribution of the number of characters per name: -------
max_char <- max(nchar(dat$name))
hist(nchar(dat$name), main = "", 
     xlab = "Number of characters per name")
paste("very few names have more than 20 characters. 
      Maximum number of characters is", max_char)

# two possibilities:
# 1) use "max_char" and padding with an "END" token indicating the end of the name
# 2) truncate at e.g. 20 characters and "END" token to save computing time
char_dict <- c(letters,
               #               " ", # remove when using the first word only
               "END")
n_chars <- length(char_dict)

# ## define/load function for one-hot-encoding all characters
source(paste0(getwd(), "/section_III/names_encoding_function.R"))
# encode_chars <- function(names, seq_max = max_char){
#         
#         N <- length(names)
#         
#         # Create 3D-Tensor with shape (No. of samples, max. name length, number of characters):
#         tmp <- array(rep(0, N * n_chars * seq_max), 
#                      dim = c(N, seq_max, n_chars)
#         ) 
#         
#         # iterate over all names
#         for(i in 1:N){
#                 name <- names[[i]]
#                 
#                 # truncate at seq_max:
#                 if(nchar(name) > seq_max){name <- substr(name, 1, seq_max)}
#                 
#                 # encode characters:
#                 for (char in 1:nchar(name)) {
#                         idx_pos <- which(char_dict == substr(name, char, char))
#                         tmp[i, char, idx_pos] <- 1
#                 }
#                 
#                 # padding:
#                 if(nchar(name) < seq_max){
#                         tmp[i, seq(nchar(name)+1, seq_max), which(char_dict == "END")] <- 1
#                 }
#         }
#         
#         return(tmp)
# }



# TEST
encode_chars(names = "asterix", seq_max = max_char)[1, , ]

# encode all names in the data and create a 3D-Tensor to train and evaluate the models
x_dat <- encode_chars(names = dat$name, seq_max = max_char) # truncate at max_char / 20 character length

# summarize
paste("names are one-hot-encoded with shape: ", 
      paste0("(", paste(dim(x_dat), collapse = ", "), ")")
)

################################################
######### split to train and test set ##########
################################################

train_frac <- 0.7 # fraction of data to train the model
N <- nrow(dat)
set.seed(08072020)
train_idx <- sample(seq(N), N * train_frac)

x_train <- x_dat[train_idx, , ]
x_val <- x_dat[-train_idx, , ]
y_train <- y_dat[train_idx]
y_val <- y_dat[-train_idx]

#########################################
############ train the network ##########
#########################################

## build and compile the model -------------------------------------------------
model <- keras_model_sequential()
model %>%
        layer_lstm(units = 512, return_sequences = TRUE, 
                   input_shape = c(dim(x_train)[2], dim(x_train)[3])) %>%
        layer_dropout(rate = 0.2) %>%
        layer_lstm(units = 128) %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = 2, activation = "softmax")

summary(model)

model %>% compile(
        optimizer =  "adam",
        loss = "sparse_categorical_crossentropy",
        metrics = c("accuracy"))

## fit the model ---------------------------------------------------------------
hist <- model %>% fit(
        x = x_train, y = y_train, 
        validation_data = list(x_val, y_val),
        epochs = 25, batch_size = 512, verbose = 2)

plot(hist)

## save the model (C) --------------------------------------------------------------
# model %>% save_model_hdf5("/scicore/home/weder/GROUP/Innovation/01_patent_data/created_models/gender_classification_LSTM_model.h5")

## evaluate the model ----------------------------------------------------------
tmp_pred <- encode_chars(c("matthias", "christian", "dragan",
                           "sascha", "nicola", "kim", "dominique", "robin"))
tmp_true <- c(1, 1, 1, 1, 1, 0, 0, 1)
model %>% predict_proba(tmp_pred)
tmp_true == model %>% predict_classes(tmp_pred)

##### findings: ----------------------------------------------------------------
# A) with first name, full sample, 2 512-LSTM layers, batch_size = 512 and 25 epochs:
#   (1) starts slightly overfitting at around 7 epochs
#   (2) validation accuracy reaches 97% after 16 epochs and then marginally increases to 97.5%

# B) with removing lastname, full sample, 512 & 128 LSTM layers, batch_size = 512 and 25 epochs:
#   (1) starts slightly overfitting at around 9 epochs
#   (2) validation accuracy reaches 96.6% at 16 epochs and then marginally increases to 97.2%
#   => seems to have clearly less overfitting
#   => BUT more likely to include a double lastname instead of a double first name

# C) with first name, full sample, 512 & 128-LSTM layers, batch_size = 512 and 25 epochs:
#   (1) starts slightly overfitting at around 12 epochs, but almost no overfitting
#   (2) validation accuracy reaches 96.9% at 16 epochs and then marginally increases to 97.4%

## similar work: -----------------------------------------------------------------

# https://maelfabien.github.io/machinelearning/NLP_7/# (French and American names)
# https://arxiv.org/pdf/1707.07129v2.pdf (Indonesian names)
# https://towardsdatascience.com/deep-learning-gender-from-name-lstm-recurrent-neural-networks-448d64553044 (Indian names)

# results from these analyses:
# Indonesian names: > 92% accuracy
# French & US names: 86 % accuracy
# Indian names : 86%
# => our model: 97% accuracy (probably because we have a balanced training sample)

