cat("30: Preporcess the data and train algorithms to classify gender based on names.
    (MN 02.07.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")


# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}

#######################################
############ Load data sets ###########
#######################################

dat <- readRDS(paste0(getwd(),"/section_III/gender_training_sample.rds"))

#######################################
######### preprocess the data #########
#######################################

