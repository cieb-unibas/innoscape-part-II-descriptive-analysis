print("Create aggregate data of backward and forward citations for Pharma patents  / CR 3.6.2020")

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
## Load data on technology field / raw data not on Github 
q_new <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds")) %>% distinct(p_key, tech_field) %>% mutate(p_key = as.character(p_key))

## Load data on inventors / considering cross-border commuters to Switzerland / raw data not on Github 
inv_reg <- readRDS(paste0(mainDir1,  "/created data/inv_reg_CHcommute_adj.rds")) 
inv_reg <- dplyr::rename(inv_reg, Up_reg_label = regio_pat, Ctry_code = ctry_pat)
inv_reg <- dplyr::select(inv_reg, -tech_field)

## Focus only on subset of triadic patents / raw data not on Github 
tpf <- readRDS(paste0(mainDir1, "/created data/triadic_fam.rds"))
inv_reg <- mutate(inv_reg, triadic = ifelse(p_key %in% unique(tpf$p_key) | patent_id %in% unique(tpf$patent_id), 1, 0))
inv_reg <- setDT(inv_reg)[, ind_triad := sum(triadic, na.rm = T), .(p_key)]
inv_reg <- filter(inv_reg, ind_triad > 0)

inv_reg <- dplyr::rename(inv_reg, ctry_code = Ctry_code)
setDT(inv_reg)[, share_inv := 1/.N, .(p_key)] 
inv_reg <- mutate(inv_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"), ctry_name = countrycode(ctry_code, origin = "eurostat", "country.name.en"))
inv_reg <- left_join(inv_reg, q_new, by = c("p_key"))
p_year <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/dat_p_year.rds")  %>% mutate(p_key = as.character(p_key))  %>% dplyr::distinct(p_key, p_year)


#######################################
## Calculate sectoral citation flows) #
#######################################
# Load data on citations / raw data not on Github 
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

## Combine continent, region and country calculation to create data set used in report
## Data on forward citations
forw_conti <- readRDS(paste0(getwd(), "/Data creation/forw_citations_conti_16.rds")) %>%  mutate(geo = "conti")
forw_ctry  <- readRDS(paste0(getwd(), "/Data creation/forw_citations_ctry_16.rds")) %>%  mutate(geo = "ctry")
forw_reg   <- readRDS(paste0(getwd(), "/Data creation/forw_citations_regio_16.rds")) %>%  mutate(geo = "regio")
rbind.fill(forw_conti, forw_ctry, forw_reg) %>% saveRDS(paste0(getwd(), "/Data creation/forw_citations_16.rds"))

## Data on backward citations
back_conti <- readRDS(paste0(getwd(), "/Data creation/back_citations_conti_16.rds")) %>%  mutate(geo = "conti")
back_ctry  <- readRDS(paste0(getwd(), "/Data creation/back_citations_ctry_16.rds")) %>%  mutate(geo = "ctry")
back_reg   <- readRDS(paste0(getwd(), "/Data creation/back_citations_regio_16.rds")) %>%  mutate(geo = "regio")
rbind.fill(back_conti, back_ctry, back_reg) %>% saveRDS(paste0(getwd(), "/Data creation/back_citations_16.rds"))



