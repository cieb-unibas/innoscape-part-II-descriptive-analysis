print("25: Create data for III. Research networks of Pharma industries using assignee information  / CR 4.6.2020")

library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
require(plyr)
require(data.table)
library(tidyverse)
require(rio)
require(countrycode)
require(stringdist)
require(fuzzyjoin)


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
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg.rds")) 
firm_reg <- distinct(firm_reg, p_key, organization, country, .keep_all = T)
firm_reg <- dplyr::rename(firm_reg, ctry_code = country)
setDT(firm_reg)[, share_firm := 1/.N, .(p_key)] #Q: Should we divide an invention by the number of regions or should we attribute each region a value to 1?
firm_reg <- mutate(firm_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"))
firm_reg <- left_join(firm_reg, q_new, by = c("p_key"))

## get p_keys of patents for which at least one assignee belongs to academia but not all
firm_reg <- setDT(firm_reg)[, c("num", "num_uni") := list(.N, sum(uni)), .(p_key)]
firm_reg <- mutate(firm_reg, uni = ifelse(num_uni == num, 0, ifelse(num_uni > 0, 1, 2))) #0: all academia; 1: some academia; 2: no academia

## Function to calculate number of collaboration among academia and companies and geographic divisions
col_acd_cmp <- function(geo_level, p_year_start, tech_field_start, uni){
  if(uni == "yes"){  
    firm_reg <- filter(firm_reg, p_year %in% p_year_start & tech_field %in% tech_field_start & uni == 1)} else {
    firm_reg <- filter(firm_reg, p_year %in% p_year_start & tech_field %in% tech_field_start) 
    }
  if(geo_level == "ctry_code"){
    firm_reg <- distinct(firm_reg, ctry_code, uni, p_key, .keep_all = T)} else if(geo_level == "reg_code"){
      firm_reg <- distinct(firm_reg, reg_code, uni, p_key, .keep_all = T)  
    } else if(geo_level == "Up_reg_code"){
      firm_reg <- distinct(firm_reg, Up_reg_code, uni, p_key, .keep_all = T)  
    }
  print(p_year_start)
  ## pairwise geographic locations per patent

  if(nrow(firm_reg) == 0){ reg <- NULL} else {
    reg <- setDT(firm_reg)[, CJ(get(geo_level), get(geo_level)), .(p_key)]
    colnames(reg) <- c("p_key", "reg_code_1", "reg_code_2")
    # reg <- filter(reg, reg_code_1 != reg_code_2) 
    reg <- mutate(reg, reg_code_12 = paste0(pmin(reg_code_1, reg_code_2), "-", pmax(reg_code_1, reg_code_2)))
    reg <- distinct(reg, p_key, reg_code_12, .keep_all = T)
    
    ## Aggregation of geographic location pairs
    reg$count <- 1
    reg <- aggregate(count ~ reg_code_1 + reg_code_2 + reg_code_12, FUN = sum, data = reg)
    reg <- mutate(reg, p_year = p_year_start, tech_field = tech_field_start)
    
  }
  
    return(reg)
}


# Calculations for country-pairs 
## academia / companies
collab_ctry <- rbind.fill(lapply(seq(1990, 2019, 1), function(x) col_acd_cmp("ctry_code", x, 16, "yes")))
collab_ctry <- mutate(collab_ctry, ctry_1 = countrycode(reg_code_1, "iso2c", "country.name.en"), ctry_2 = countrycode(reg_code_2, "iso2c", "country.name.en"))
collab_ctry %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/uni_firm_ctry_16.rds")
## all
collab_ctry <- rbind.fill(lapply(seq(1990, 2019, 1), function(x) col_acd_cmp("ctry_code", x, 16, "no")))
collab_ctry <- mutate(collab_ctry, ctry_1 = countrycode(reg_code_1, "iso2c", "country.name.en"), ctry_2 = countrycode(reg_code_2, "iso2c", "country.name.en"))
collab_ctry %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/firm_firm_ctry_16.rds")

## Calculations for region-pairs
## academia / companies
collab_reg <- rbind.fill(lapply(seq(1990, 2018, 1), function(x) col_acd_cmp("Up_reg_code", x, 16, "yes")))
regio <- read.csv2(paste0(mainDir1, "/raw data/REGPAT_REGIONS.txt"), sep = "|") %>% dplyr::select(-Ctry_code) %>% distinct(Up_reg_code, Up_reg_label)
collab_reg <- left_join(collab_reg, regio, by = c("reg_code_1" = "Up_reg_code")) %>% dplyr::rename(reg_lab_1 = Up_reg_label)
collab_reg <- left_join(collab_reg, regio, by = c("reg_code_2" = "Up_reg_code")) %>% dplyr::rename(reg_lab_2 = Up_reg_label)
collab_reg %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/uni_firm_reg_16.rds")
## all
collab_reg <- rbind.fill(lapply(seq(1990, 2019, 1), function(x) col_acd_cmp("Up_reg_code", x, 16, "no")))
regio <- read.csv2(paste0(mainDir1, "/raw data/REGPAT_REGIONS.txt"), sep = "|") %>% dplyr::select(-Ctry_code) %>% distinct(Up_reg_code, Up_reg_label)
collab_reg <- left_join(collab_reg, regio, by = c("reg_code_1" = "Up_reg_code")) %>% dplyr::rename(reg_lab_1 = Up_reg_label)
collab_reg <- left_join(collab_reg, regio, by = c("reg_code_2" = "Up_reg_code")) %>% dplyr::rename(reg_lab_2 = Up_reg_label)
collab_reg %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/firm_firm_reg_16.rds")
print("part III_ii_b. done")



### Some ideas to find out whether multiple assigness of a patent belong to the same company or not -> could add firm name and location of headquarter from orbis
## Load firm name from OECD data
# han_firm <- import(paste0(mainDir1, "/raw data/", vers, "HAN_NAMES.txt"))
# han_firm <- mutate(han_firm, organization = gsub('[[:punct:]]+','', Clean_name)) %>%  mutate(organization = tolower(organization))
# 
# ## Add unique firm number
# han_pat <- import(paste0(mainDir1, "/raw data/", vers, "HAN_PATENTS.txt"))
# han_pat <- left_join(han_pat, han_firm, by = c("HAN_ID"))
# han_pat <- mutate(han_pat, Appln_id = as.character(Appln_id))
# 
# firm_reg <- left_join(firm_reg, han_pat, by = c("pub_nbr" = "Patent_number"))
# 
# 
# t <- filter(firm_reg, str_detect(firm_reg$organization.x, regex(paste(c("roche", "novartis"), collapse = '|'), ignore_case = TRUE)) == T)
# t <- mutate(t, firm_clean = tolower(organization)) %>% mutate(firm_clean = gsub('[[:punct:] ]+',' ', firm_clean))
# 
# setDT(t)[, dist := stringdist(firm_clean, organization.y)]
# t <- setDT(t)[order(dist), .SD, .(p_key)]
# t <- setDT(t)[, num := uniqueN(organization.x), .(p_key)]
# t <- setDT(t)[, .SD[1:num], .(p_key)]







