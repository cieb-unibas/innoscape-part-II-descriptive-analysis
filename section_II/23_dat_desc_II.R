print("23: Create data for part II.	Innovation in the Swiss Pharma Sector / CR 30.6.2020")

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
inv_reg <- mutate(inv_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"), ctry_name = countrycode(ctry_code, "iso2c", "country.name.en"))
inv_reg <- left_join(inv_reg, q_new, by = c("p_key"))
inv_reg <- mutate(inv_reg, share_inv = 1) # if patent belongs to different locations -> each location gets value of 1. This takes into account international collaboration. 

## Load name of IPC
ipc <- read.csv2(paste0(mainDir1, "/raw data/ipc_4_digit.csv"))
inv_reg <- left_join(inv_reg, ipc, by = c("ipc_main" = "IPC"))

## Use backward citations to classify world-class patents
q <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds"))
q <- setDT(q)[order(-fwd_cits5), .SD[1], .(p_key)]
q <- mutate(q, cit_cat_y_5 = case_when(fwd_cits5 <= 1 ~ 0, fwd_cits5 >= 2 & fwd_cits5 < 16 ~ 1, fwd_cits5 > 15 ~ 2))
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
  
  if(geo_level == "ctry_code"){
  list_geo <- imp[1:15, geo_level]
  } else if(geo_level == "Up_reg_code"){
  list_geo <- imp[1:80, geo_level]
  } else if(geo_level == "conti"){
    list_geo <- c( "Europe", "Oceania", "Americas", "Asia", "Africa")
  }
  
  setDT(reg)[, n_ipc := sum(share_inv), .(ipc_main, get(geo_level))]
  reg <- reg %>% subset(p_year > 1989 & get(geo_level) %in% list_geo & n_ipc > 9)
  
  reg_agg <- subset(reg_agg, p_year > 1989 & get(geo_level) %in% list_geo)
  reg_agg <- mutate(reg_agg, ipc_main = "all", IPC.title = "all")
  output <- rbind.fill(reg, reg_agg) %>% mutate(p_year = as.numeric(as.character(p_year)))
  return(output)
}

################################################
## Get inventors shares of patents in general ##
################################################
num_pat_conti <- num_pat_geo("conti", "conti", 16, "no") 
num_pat_ctry <- num_pat_geo("ctry_code", "ctry_name", 16, "no") 
num_pat_reg <- num_pat_geo("Up_reg_code", "Up_reg_label", 16, "no") 
num_pat_16 <- rbind.fill(num_pat_conti, num_pat_ctry, num_pat_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti), abs_rel = "abs") 

## Create some relative outputs used in innoscape_pharma.Rmd
## relative to 1990
num_pat_16 <- setDT(num_pat_16)[, share_inv := share_inv/share_inv[p_year == 1990], .(geo, ipc_main)] %>% mutate(abs_rel = "rel_1990")

## relative share of IPC to all patents per year and RCA (relative share of IPC to all patents per country to relative share of IPC to all patetnets of all countries)
dat_conti <- dplyr::filter(num_pat_16, is.na(conti) != T) 
dat_conti <- cbind(geo = "world", aggregate(share_inv ~ p_year + ipc_main, FUN = sum, data = dat_conti))
num_pat_16_3  <- rbind.fill(num_pat_16, dat_conti) 
num_pat_16_3 <- setDT(num_pat_16_3)[, share_inv := share_inv/share_inv[ipc_main == "all"], .(geo, p_year)]  %>% mutate(abs_rel = "share_all")
num_pat_16_4 <- setDT(num_pat_16_3)[, share_inv := share_inv/share_inv[geo == "world"], .(geo, p_year)]  %>% mutate(abs_rel = "rca")
num_pat_16_3 <- filter(num_pat_16_3, geo != "world")
num_pat_16_4 <- filter(num_pat_16_4, geo != "world")

## Add all data together
num_pat_16 <- rbind.fill(num_pat_16, num_pat_16_3, num_pat_16_4) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_16.rds")
num_pat_16 <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_16.rds")

##################################################
## Get inventors shares for world class patents ##
##################################################
world_class_conti <- num_pat_geo("conti", "conti", 16, "yes") 
world_class_ctry <- num_pat_geo("ctry_code", "ctry_name", 16, "yes") 
world_class_reg <- num_pat_geo("Up_reg_code", "Up_reg_label", 16, "yes") 
world_class_16 <- rbind.fill(world_class_conti, world_class_ctry, world_class_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti), abs_rel = "abs") 

## Create some relative outputs used in innoscape_pharma.Rmd
## relative to 1990
world_class_16 <- setDT(world_class_16)[, share_inv := share_inv/share_inv[p_year == 1990], .(geo, ipc_main)] %>% mutate(abs_rel = "rel_1990")

## relative share of IPC to all patents per year and RCA (relative share of IPC to all patents per country to relative share of IPC to all patetnets of all countries)
world_class_conti <- dplyr::filter(world_class_16, is.na(conti) != T) 
world_class_conti <- cbind(geo = "world", aggregate(share_inv ~ p_year + ipc_main, FUN = sum, data = world_class_conti))
world_class_16_3  <- rbind.fill(world_class_16, world_class_conti) 
world_class_16_3 <- setDT(world_class_16_3)[, share_inv := share_inv/share_inv[ipc_main == "all"], .(geo, p_year)]  %>% mutate(abs_rel = "share_all")
world_class_16_4 <- setDT(world_class_16_3)[, share_inv := share_inv/share_inv[geo == "world"], .(geo, p_year)]  %>% mutate(abs_rel = "rca")
world_class_16_3 <- filter(world_class_16_3, geo != "world")
world_class_16_4 <- filter(world_class_16_4, geo != "world")

## Add all data together
world_class_16 <- rbind.fill(world_class_16, world_class_16_3, world_class_16_4) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_16.rds")
world_class_16 <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_16.rds")







