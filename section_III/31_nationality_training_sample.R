cat("31: Create a (balanced) sample of names for accessing the name-prism API.
    (MN 20.07.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")

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

#####################################################
#################### Distribution ###################
#####################################################

#### Country of residence distribution -----------------------------------------
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

####################################################################
################### create country groups the sample ###############
####################################################################

ctry_groups <- c("China", "India","French", rep("DACH", 3), "Japan", "Italy",
                "Korea", rep("Hispanic", 5), rep("Scandinavia", 4),
                rep("Visegrad", 4), rep("Balkans", 2), rep("Anglo-Saxon", 5), 
                rep("Portugese", 2))
ctry_codes <- c("CN", "IN", "FR", "DE", "AT", "CH", "JP", "IT", "KR", "ES", 
                "MX", "AR", "CO", "CL", "FI", "SE", "DK", "NO", "PL", "HU", 
                "SK", "CZ", "RS", "HR", "US" , "UK", "CA", "AU", "NZ", "PT", 
                "BR")
ctry_groups <- data_frame(ctry_groups = ctry_groups, ctry_codes = ctry_codes)

# subset to residence countries of interest and assign country-groups
name_origin_sub <- filter(name_origin, 
                          Ctry_code %in% ctry_codes)
name_origin_sub$ctry_group <- NA
name_origin_sub$idx <- as.numeric(rownames(name_origin_sub))

tmp <- lapply(ctry_groups$ctry_codes, function(ctry){
  
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

# check the number of observations per country-group
sample_info <- as.data.frame(table(name_origin_sub$ctry_group))
sample_info <- rename(sample_info, ctry_group = Var1, N = Freq)

########################################################
################### create training data ###############
########################################################

# max{25'000, N} observations per country group, except for the US (= 100'000)
sample_info <- mutate(sample_info,
              sample_N = ifelse(N > 25000, 25000, N))
sample_info[1, "sample_N"] <- 100000
sample_info$ctry_group <- as.character(sample_info$ctry_group)
paste("Total number of observations in the training data: ", sum(sample_info$sample_N))

train_set_fun <- function(df_in, sample_info, ctry_group){
  
  set.seed(20072020)
  sample_N <- sample_info[sample_info$ctry_group == ctry_group, "sample_N"]
  
  df_out <- df_in[df_in$ctry_group == ctry_group, ]
  df_out <- sample_n(df_out, sample_N)

  return(df_out)
}

train_dat <- lapply(sample_info$ctry_group, function(x)train_set_fun(df_in = name_origin_sub,
                                                               sample_info = sample_info,
                                                               ctry_group = x))
train_dat <- bind_rows(train_dat)

# check if the distribution is correct
table(train_dat$ctry_group)

##############################################################
################ pre-process the training data ###############
##############################################################

#### remove puctuation and turn non latin letters to ue etc.


#### add encode names with '%'


#### add only first last name



##############################################
################ save the data ###############
##############################################

# saveRDS(mainDir1)








