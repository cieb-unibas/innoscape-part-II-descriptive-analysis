print("23: Create data for part II.	Innovation in the Swiss Pharma Sector / CR 3.6.2020")

library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
require(plyr)
require(data.table)
library(tidyverse)
require(rio)
require(countrycode)


mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"

# Version of OECD-data
# vers <- c("201907_")
vers <- c("202001_")

####################################################
## Load data used afterwards for each calculation ##
####################################################
## Load data on technology field
q_new <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds")) %>% distinct(p_key, tech_field) %>% mutate(p_key = as.character(p_key))

## Load regions of patents' inventors
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds")) 
inv_reg <- dplyr::rename(inv_reg, ctry_code = Ctry_code)
setDT(inv_reg)[, share_inv := 1/.N, .(p_key)] #Q: Should we divide an invention by the number of regions or should we attribute each region a value to 1?
inv_reg <- mutate(inv_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"), ctry_name = countrycode(ctry_code, "iso2c", "country.name.en"))
inv_reg <- left_join(inv_reg, q_new, by = c("p_key"))
inv_reg <- mutate(inv_reg, share_inv = 1) # if patent belongs to different locations -> each location gets value of 1. This takes into account international collaboration. 

## Load name of IPC
ipc <- read.csv2(paste0(mainDir1, "/raw data/ipc_4_digit.csv"))
inv_reg <- left_join(inv_reg, ipc, by = c("ipc_main" = "IPC"))

## Load backward citations to classify world-class patents
q <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds"))
q <- setDT(q)[order(-fwd_cits5), .SD[1], .(p_key)]
q <- mutate(q, cit_cat_y_5 = case_when(fwd_cits5 <= 1 ~ 0, fwd_cits5 >= 2 & fwd_cits5 < 20 ~ 1, fwd_cits5 > 19 ~ 2))
q <- dplyr::select(q, p_key, cit_cat_y_5) %>% mutate(p_key= as.character(p_key))

inv_reg <- left_join(inv_reg, q, by = c("p_key"))


#######################################################
## b.	Swiss Pharma in the Domestic Economy (Why Pharma?) #
##########################################################
## Function to calculate number of patents per geographic entity
num_pat_geo <- function(geo_level, geo_name, tech_field_start, world_class){
  inv_reg <- filter(inv_reg, tech_field %in% tech_field_start)
  inv_reg <- inv_reg[!duplicated(inv_reg[, c("p_key", geo_level)]), ]
  
  ## find important countries and regions
  imp <- aggregate(as.formula(paste0("share_inv ~", geo_level)), data = filter(inv_reg, p_year > 1989), FUN = sum)
  imp <- arrange(imp, -share_inv) 
  
  if(world_class == "yes"){
    form <- as.formula(paste0("share_inv ~ cit_cat_y_5 + p_year + ipc_main + IPC.title + ", geo_level, " + ", geo_name))
    form_agg <- as.formula(paste0("share_inv ~ cit_cat_y_5 + p_year + ", geo_level, " + ", geo_name))
    }else{
  form <- as.formula(paste0("share_inv ~ p_year + ipc_main + IPC.title + ", geo_level, " + ", geo_name))
  form_agg <- as.formula(paste0("share_inv ~ p_year + ", geo_level, " + ", geo_name))
    }
    
  reg     <- aggregate(form, FUN = sum, data = inv_reg) %>% mutate(reg, tech_field = tech_field_start)
  reg_agg <- aggregate(form_agg, FUN = sum, data = inv_reg) %>% mutate(reg_agg, tech_field = tech_field_start)
  
  ## Create some outputs used in innoscape_pharma.Rmd
  if(geo_level == "ctry_code"){
  list_geo <- imp[1:10, geo_level]
  } else if(geo_level == "Up_reg_code"){
  list_geo <- imp[1:50, geo_level]
  } else if(geo_level == "conti"){
    list_geo <- c( "Europe", "Oceania", "Americas", "Asia", "Africa")
  }
  
  setDT(reg)[, n_ipc := .N, .(p_year, ipc_main)]
  reg <- reg %>% subset(p_year > 1989 & get(geo_level) %in% list_geo & n_ipc > 9)
  
  reg_agg <- subset(reg_agg, p_year > 1989 & get(geo_level) %in% list_geo)
  reg_agg <- mutate(reg_agg, ipc_main = "all", IPC.title = "all")
  output <- rbind.fill(reg, reg_agg) %>% mutate(p_year = as.numeric(as.character(p_year)))
  return(output)
}

num_pat_conti <- num_pat_geo("conti", "conti", 16, "no") 
num_pat_ctry <- num_pat_geo("ctry_code", "ctry_name", 16, "no") 
num_pat_reg <- num_pat_geo("Up_reg_code", "Up_reg_label", 16, "no") 
rbind.fill(num_pat_conti, num_pat_ctry, num_pat_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti)) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_16.rds")
num_pat_16 <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_16.rds")

world_class_conti <- num_pat_geo("conti", "conti", 16, "yes") 
world_class_ctry <- num_pat_geo("ctry_code", "ctry_name", 16, "yes") 
world_class_reg <- num_pat_geo("Up_reg_code", "Up_reg_label", 16, "yes") 
rbind.fill(world_class_conti, world_class_ctry, world_class_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti)) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_16.rds")






