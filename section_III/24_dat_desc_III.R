print("24: Create data for III. Research networks of Pharma industries using inventor information  / CR 3.6.2020")

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

## Using following file to consider cross-border commuters to Switzerland
inv_reg <- readRDS(paste0(mainDir1,  "/created data/inv_reg_CHcommute_adj.rds")) 
inv_reg <- dplyr::rename(inv_reg, Up_reg_label = regio_pat, Ctry_code = ctry_pat)
inv_reg <- dplyr::select(inv_reg, -tech_field)

## Focus only on subset of triadic patents
tpf <- readRDS(paste0(mainDir1, "/created data/triadic_fam.rds"))
inv_reg <- mutate(inv_reg, triadic = ifelse(p_key %in% unique(tpf$p_key) | patent_id %in% unique(tpf$patent_id), 1, 0))
inv_reg <- setDT(inv_reg)[, ind_triad := sum(triadic, na.rm = T), .(p_key)]
inv_reg <- filter(inv_reg, ind_triad > 0)

## Using following subset to only consider granted patents. It, however, overwrites the existing .RDS files
## inv_reg <- filter(inv_reg, granted == "yes")

inv_reg <- dplyr::rename(inv_reg, ctry_code = Ctry_code)
setDT(inv_reg)[, share_inv := 1/.N, .(p_key)] #Q: Should we divide an invention by the number of regions or should we attribute each region a value to 1? At the moment, we use the share...
inv_reg <- mutate(inv_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"), ctry_name = countrycode(ctry_code, origin = "eurostat", "country.name.en"))
inv_reg <- left_join(inv_reg, q_new, by = c("p_key"))
p_year <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/dat_p_year.rds")  %>% mutate(p_key = as.character(p_key))  %>% dplyr::distinct(p_key, p_year)

###########################################################
## a. Geographical diffusion (International Collaboration #
###########################################################
## Function to calculate number of collaboration among different geographical locations #
geo_col <- function(geo_level, p_year_start, tech_field_start){
## unique geographic locations per patent
inv_reg <- filter(inv_reg, p_year %in% p_year_start & tech_field %in% tech_field_start)
if(geo_level == "ctry_code"){
inv_reg <- distinct(inv_reg, ctry_code, p_key, .keep_all = T)} else if(geo_level == "reg_code"){
  inv_reg <- distinct(inv_reg, reg_code, p_key, .keep_all = T)
} else if(geo_level == "Up_reg_code"){
  inv_reg <- distinct(inv_reg, Up_reg_code, p_key, .keep_all = T)
} else if(geo_level == "conti"){
  inv_reg <- distinct(inv_reg, conti, p_key, .keep_all = T)
}

## pairwise geographic locations per patent
reg <- setDT(inv_reg)[, CJ(get(geo_level), get(geo_level)), .(p_key)]
colnames(reg) <- c("p_key", "reg_code_1", "reg_code_2")
reg <- filter(reg, reg_code_1 != reg_code_2)
reg <- mutate(reg, reg_code_12 = paste0(pmin(reg_code_1, reg_code_2), "-", pmax(reg_code_1, reg_code_2)))
reg <- distinct(reg, p_key, reg_code_12, .keep_all = T)

## Aggregation of geographic location pairs
reg$count <- 1
reg <- aggregate(count ~ reg_code_1 + reg_code_2 + reg_code_12, FUN = sum, data = reg)
reg <- mutate(reg, p_year = p_year_start, tech_field = tech_field_start)
return(reg)
}


## Calculations for continent-pairs
collab_conti <- rbind.fill(lapply(seq(1990, 2018, 1), function(x) geo_col("conti", x, 16)))
collab_conti <- mutate(collab_conti, conti_1 = reg_code_1, conti_2 = reg_code_2)
collab_conti %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/collab_conti_16.rds")

## Calculations for country-pairs
collab_ctry <- rbind.fill(lapply(seq(1990, 2018, 1), function(x) geo_col("ctry_code", x, 16)))
collab_ctry <- mutate(collab_ctry, ctry_1 = countrycode(reg_code_1, "iso2c", "country.name.en"), ctry_2 = countrycode(reg_code_2, "iso2c", "country.name.en"))
collab_ctry %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/collab_ctry_16.rds")

## Calculations for region-pairs
collab_reg <- rbind.fill(lapply(seq(1990, 2018, 1), function(x) geo_col("Up_reg_code", x, 16)))
regio <- read.csv2(paste0(mainDir1, "/raw data/REGPAT_REGIONS.txt"), sep = "|") %>% dplyr::select(-Ctry_code) %>% distinct(Up_reg_code, Up_reg_label)
collab_reg <- left_join(collab_reg, regio, by = c("reg_code_1" = "Up_reg_code")) %>% dplyr::rename(reg_lab_1 = Up_reg_label)
collab_reg <- left_join(collab_reg, regio, by = c("reg_code_2" = "Up_reg_code")) %>% dplyr::rename(reg_lab_2 = Up_reg_label)
collab_reg %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/collab_reg_16.rds")

## Combine continent, region and country to create data set used in report
collab_conti <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/collab_conti_16.rds")
collab_ctry  <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/collab_ctry_16.rds")
collab_reg   <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/collab_reg_16.rds")

rbind.fill(collab_conti, collab_ctry, collab_reg) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/collab_inv_16.rds")
print("part a. done")

###########################################
## b. Sectoral diffusion (citation flows) #
###########################################
fwd <- readRDS(paste0(mainDir1,  "/created data/", "forward_cit_dat.rds"))

## Add data on priority year of a patent (called as p_year)
fwd  <- left_join(fwd, p_year, by = c("key_cited" = "p_key")) %>% dplyr::rename(p_year_cited = p_year)
fwd  <- left_join(fwd, p_year, by = c("key_citing" = "p_key")) %>% dplyr::rename(p_year_citing = p_year)

## Create function for calculation of backward and forward citation flows by regions, countries and continents for different technologies
cit_flows <- function(tech_field_start, cit, p_year_start, geo_citing, geo_cited){
  q_sub <- filter(q_new, tech_field %in% tech_field_start)

  if(cit == "forw_cit"){
    print("forw_cit")
    fwd   <- filter(fwd, p_year_citing %in% p_year_start)
    fwd   <- inner_join(q_sub, fwd, by = c("p_key" = "key_cited")) %>% dplyr::rename(tech_field_cited = tech_field, key_cited = p_key)
    fwd   <- left_join(fwd, q_new, by = c("key_citing" = "p_key")) %>% dplyr::rename(tech_field_citing = tech_field)
    fwd   <- left_join(fwd, inv_reg, by = c("key_cited" = "p_key"))  %>% dplyr::rename(conti_cited = conti, up_reg_label_cited = Up_reg_label, up_reg_code_cited = Up_reg_code, ctry_cited = ctry_code, ctry_name_cited = ctry_name, share_inv_cited = share_inv, name_cited = name)
    fwd   <- left_join(fwd, inv_reg, by = c("key_citing" = "p_key")) %>% dplyr::rename(conti_citing = conti, up_reg_label_citing = Up_reg_label, up_reg_code_citing = Up_reg_code, ctry_citing = ctry_code, ctry_name_citing = ctry_name, share_inv_citing = share_inv, name_citing = name)
    #Q: how to deal with citing patent if it has been invented by persons from different countries?-> at the moment just add knowledge flow of 1 if at least one inventor from a country
    fwd   <- distinct(fwd, key_cited, !!geo_cited, key_citing, name_citing, .keep_all = T)
    cit_flows <- aggregate(as.formula(paste("share_inv_citing~", paste(c("p_year_citing", geo_citing, geo_cited, "tech_field_citing", "tech_field_cited"), collapse="+"))), FUN = sum, na.rm = T, data = fwd)
    cit_flows <- data.frame(cit_flows, cit = cit)
    return(cit_flows)


  } else if(cit == "back_cit"){
    print("back_cit")
    fwd   <- filter(fwd, p_year_citing %in% p_year_start)
    fwd   <- inner_join(q_sub, fwd, by = c("p_key" = "key_citing")) %>% dplyr::rename(tech_field_citing = tech_field, key_citing = p_key)
    fwd   <- left_join(fwd, q_new, by = c("key_cited" = "p_key")) %>% dplyr::rename(tech_field_cited = tech_field)
    fwd   <- left_join(fwd, inv_reg, by = c("key_cited" = "p_key"))  %>% dplyr::rename(conti_cited = conti, up_reg_label_cited = Up_reg_label, up_reg_code_cited = Up_reg_code, ctry_cited = ctry_code, ctry_name_cited = ctry_name, share_inv_cited = share_inv, name_cited = name)
    fwd   <- left_join(fwd, inv_reg, by = c("key_citing" = "p_key")) %>% dplyr::rename(conti_citing = conti, up_reg_label_citing = Up_reg_label, up_reg_code_citing = Up_reg_code, ctry_citing = ctry_code, ctry_name_citing = ctry_name, share_inv_citing = share_inv, name_citing = name)
    #Q: how to deal with citing patent if it has been invented by persons from different countries?-> at the moment just add knowledge flow of 1 if at least one inventor from a country
    fwd   <- distinct(fwd, key_citing, !!geo_citing, key_cited, name_cited, .keep_all = T)
    cit_flows <- aggregate(as.formula(paste("share_inv_cited~", paste(c("p_year_citing", geo_citing, geo_cited, "tech_field_citing", "tech_field_cited"), collapse="+"))), FUN = sum, na.rm = T, data = fwd)
    cit_flows <- data.frame(cit_flows, cit = cit)
    return(cit_flows)

  } else {"choose either back_cit or for_cit"}
}

## Create data at the continent-level
rbind.fill(lapply(seq(1990, 2018, 1), function(x) cit_flows(16, "forw_cit", x, "conti_citing", "conti_cited"))) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/forw_citations_conti_16.rds")
rbind.fill(lapply(seq(1990, 2018, 1), function(x) cit_flows(16, "back_cit", x, "conti_citing", "conti_cited"))) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_conti_16.rds")

## Create data at the country-level
rbind.fill(lapply(seq(1990, 2018, 1), function(x) cit_flows(16, "forw_cit", x, "ctry_name_citing", "ctry_name_cited"))) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/forw_citations_ctry_16.rds")
rbind.fill(lapply(seq(1990, 2018, 1), function(x) cit_flows(16, "back_cit", x, "ctry_name_citing", "ctry_name_cited"))) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_ctry_16.rds")

## Create data at the regional-level
rbind.fill(lapply(seq(1990, 2018, 1), function(x) cit_flows(16, "forw_cit", x, "up_reg_label_citing", "up_reg_label_cited"))) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/forw_citations_regio_16.rds")
rbind.fill(lapply(seq(1990, 2018, 1), function(x) cit_flows(16, "back_cit", x, "up_reg_label_citing", "up_reg_label_cited"))) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_regio_16.rds")
print("part b. done")

## Combine continent, region and country calculation to creat data set used in report
## Data on forward citations
forw_conti <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/forw_citations_conti_16.rds") %>%  mutate(geo = "conti")
forw_ctry  <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/forw_citations_ctry_16.rds") %>%  mutate(geo = "ctry")
forw_reg   <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/forw_citations_regio_16.rds") %>%  mutate(geo = "regio")
rbind.fill(forw_conti, forw_ctry, forw_reg) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/forw_citations_16.rds")

## Data on backward citations
back_conti <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_conti_16.rds") %>%  mutate(geo = "conti")
back_ctry  <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_ctry_16.rds") %>%  mutate(geo = "ctry")
back_reg   <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_regio_16.rds") %>%  mutate(geo = "regio")
rbind.fill(back_conti, back_ctry, back_reg) %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/back_citations_16.rds")


back <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/back_citations_16.rds")
forw <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/forw_citations_16.rds")


