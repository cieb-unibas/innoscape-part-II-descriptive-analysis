cat("26: Create data for Section III.: 
    Innovator origins and gender in Pharma industries using inventor information  / MN 23.6.2020")

#######################################
## Load packages and set directories ##
#######################################

library("tidyverse")
# library(tidytext)
# require(plyr)
library("data.table")
# require(rio)
library("countrycode")
# require(stringdist)
# require(fuzzyjoin)

mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

# Version of OECD-data
# vers <- c("201907_")
vers <- c("202001_")

#######################################
############ Load data sets ###########
#######################################

## Load data on technology field
q_new <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds")) %>% 
  distinct(p_key, tech_field) %>% 
  mutate(p_key = as.character(p_key))

## Load data on inventor names and addresses


## Load regions of patents' inventors
# firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg.rds")) 
# firm_reg <- distinct(firm_reg, p_key, organization, country, .keep_all = T)
# firm_reg <- dplyr::rename(firm_reg, ctry_code = country)
# setDT(firm_reg)[, share_firm := 1/.N, .(p_key)] #Q: Should we divide an invention by the number of regions or should we attribute each region a value to 1?
# firm_reg <- mutate(firm_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"))
# firm_reg <- left_join(firm_reg, q_new, by = c("p_key"))










