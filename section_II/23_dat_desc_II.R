print("23: Create data for part II.	Innovation in the Swiss Pharma Sector / CR 24.8.2020")

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
tech_field_start <- 16
## Latest year to calculate world class patents
max_year_past    <- 2015 
#########################################
## Load data on geography of inventors ##
#########################################
## Load regions of patents' inventors -> NEW: adjusted by cross-border commuters
inv_reg <- readRDS(paste0(mainDir1,  "/created data/inv_reg_CHcommute_adj.rds")) 
inv_reg <- filter(inv_reg, is.na(tech_field) != T & tech_field == tech_field_start)
## For the analysis, use ctry_pat and regio_pat 
inv_reg <- dplyr::select(inv_reg, -Ctry_code, -Up_reg_label)
inv_reg <- dplyr::rename(inv_reg, ctry_code = ctry_pat, Up_reg_label = regio_pat)
inv_reg <- mutate(inv_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"), ctry_name = countrycode(ctry_code, "iso2c", "country.name.en"), Up_reg_label = paste0(ctry_code, " - ", Up_reg_label))
inv_reg <- setDT(inv_reg)[, share_inv := 1/.N, .(p_key)]
inv_reg <- mutate(inv_reg, ipc_3 = substr(ipc_main, 1, 3)) %>% dplyr::select(-ipc_main)

## Add list of IPC per patent created in 08b_p_key_ipc.R -> could be used later on to add different technology fields
# ipc <- readRDS(paste0(mainDir1, "/created data/ipc_list.rds"))
# ipc <- setDT(ipc)[, .SD[1], .(p_key)]
# ipc <- dplyr::select(ipc, -ipc_main, -ipc_list, -ipc_3_list, -ict, -ai)
# inv_reg <- left_join(inv_reg, ipc, by = c("p_key"))

## Load name of IPC
ipc_name <- read.csv2(paste0(mainDir1, "/raw data/ipc_3_digit.csv")) %>% dplyr::select(Code.1, Description)
inv_reg <- left_join(inv_reg, ipc_name, by = c("ipc_3" = "Code.1"))

## Use backward citations to classify world-class patents -> top patent if among 10% most cited on a yearly basis / use filing year to calculate top-10 patents / Using only US-patents, since EPO-patents are significantly less cited
cit_quant <- function(pat_office, q, data, year){
  pat <- data
  pat  <- mutate(pat, pat_off = substr(pub_nbr, 1, 2))
  pat  <- filter(pat, pat_off %in% pat_office)
  pat  <- setDT(pat)[order(filing), .SD[1], .(p_key)]
  pat <- filter(pat, filing %in% year)
  pat  <- data.frame(pat)
  quant <- quantile(pat[, "fwd_cits5"], q)
  quant <- data.frame(class = seq(1, 3, 1), cut_off = quant, pub_year = year)
  return(quant)
}

pat_past <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds"))
pat_past <- mutate(pat_past, filing = as.character(filing))
get_cut_off <- do.call(rbind, lapply(seq(1990, max_year_past, 1), function(x) cit_quant(c("US"), c(0, 0.6, 0.9), pat_past, x)))
get_cut_off <- dcast(get_cut_off, pub_year ~ class, value.var = "cut_off")
colnames(get_cut_off) <- c("filing", paste0("class_", seq(1, 3, 1))) 
get_cut_off <- mutate(get_cut_off, filing = as.character(filing))
pat_past <- left_join(pat_past, get_cut_off, by = "filing")
pat_past <- mutate(pat_past, cit_cat_y_5 = case_when(fwd_cits5 <= class_2 ~ 0, fwd_cits5 > class_2 & fwd_cits5 < class_3 ~ 1, fwd_cits5 >= class_3 ~ 2), num = 1)
pat_past <- dplyr::select(pat_past, p_key, cit_cat_y_5) %>% mutate(p_key = as.character(p_key))
pat_past <- filter(pat_past, is.na(cit_cat_y_5) != T)
pat_past <- mutate(pat_past, p_key = as.character(p_key))

## Add together with data on patents
inv_reg <- left_join(inv_reg, pat_past, by = c("p_key"))

#######################################################
## b.	Swiss Pharma in the Domestic Economy (Why Pharma?) #
##########################################################
## Function to calculate number of patents per geographic entity
num_pat_geo <- function(geo_level, geo_name, tech_field_start, world_class, geo_value = "no"){
  inv_reg <- filter(inv_reg, tech_field %in% tech_field_start)
  if(geo_value == "yes"){
  inv_reg <- mutate(inv_reg, share_inv = 1) # if patent belongs to different locations -> each location gets value of 1. This takes into account international collaboration. 
  inv_reg <- inv_reg[!duplicated(inv_reg[, c("p_key", "ipc_3", geo_level)]), ] 
  }
  ## find important countries and regions
  imp <- aggregate(as.formula(paste0("share_inv ~", geo_level)), data = filter(inv_reg, p_year > 1989), FUN = sum)
  imp <- arrange(imp, -share_inv) 
  
  if(world_class == "yes"){
    form <- as.formula(paste0("share_inv ~ cit_cat_y_5 + p_year + ipc_3 + Description + ", geo_level, " + ", geo_name))
    form_agg <- as.formula(paste0("share_inv ~ cit_cat_y_5 + p_year + ", geo_level, " + ", geo_name))
  }else{
    form <- as.formula(paste0("share_inv ~ p_year + ipc_3 + Description + ", geo_level, " + ", geo_name))
    form_agg <- as.formula(paste0("share_inv ~ p_year + ", geo_level, " + ", geo_name))
  }
  
  reg     <- aggregate(form, FUN = sum, data = inv_reg) %>% mutate(tech_field = tech_field_start)
  reg_agg <- aggregate(form_agg, FUN = sum, data = inv_reg[!duplicated(inv_reg[, c("p_key", geo_level)]), ]) %>% mutate(tech_field = tech_field_start)
  
  if(geo_level == "ctry_code"){
    list_geo <- imp[1:15, geo_level]
  } else if(geo_level == "Up_reg_label"){
    list_geo <- imp[1:80, geo_level]
  } else if(geo_level == "conti"){
    list_geo <- c( "Europe", "Oceania", "Americas", "Asia", "Africa")
  }
  
  setDT(reg)[, n_ipc := sum(share_inv), .(ipc_3)]
  reg <- reg %>% mutate(rank = dense_rank(desc(n_ipc)))
  reg <- reg %>% subset(p_year > 1989 & get(geo_level) %in% list_geo)
  
  reg_agg <- subset(reg_agg, p_year > 1989 & get(geo_level) %in% list_geo)
  reg_agg <- mutate(reg_agg, ipc_3 = "all", Description = "all")
  setDT(reg_agg)[, n_ipc := sum(share_inv), .(ipc_3)]
  
  output <- rbind.fill(reg, reg_agg) %>% mutate(p_year = as.numeric(as.character(p_year)))
  output$geo_one_inv <- geo_value
  return(output)
}

################################################
## Get inventors shares of patents in general ##
################################################
do.call(rbind, lapply(c("no", "yes"), function(geo_one_inv){
num_pat_conti <- num_pat_geo("conti", "conti", tech_field_start, "no", geo_one_inv) 
num_pat_ctry <- num_pat_geo("ctry_code", "ctry_name", tech_field_start, "no", geo_one_inv) 
num_pat_reg <- num_pat_geo("Up_reg_label", "Up_reg_label", tech_field_start, "no", geo_one_inv) 
num_pat_16 <- rbind.fill(num_pat_conti, num_pat_ctry, num_pat_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti), abs_rel = "abs") 

## Create some relative outputs used in innoscape_pharma.Rmd
## relative to 1990
num_pat_16_2 <- as.data.table(num_pat_16)
num_pat_16_2 <- setDT(num_pat_16_2)[, share_inv := share_inv/share_inv[p_year == 1990], .(geo, ipc_3)] 
num_pat_16_2 <- mutate(num_pat_16_2, abs_rel = "rel_1990")

## relative share of geo within IPC per year and RCA (relative share of IPC to all patents per country to relative share of IPC to all patetnets of all countries)
dat_conti <- dplyr::filter(num_pat_16, is.na(conti) != T) 
dat_conti <- cbind(geo = "world", aggregate(share_inv ~ p_year + ipc_3, FUN = sum, data = dat_conti))
num_pat_16_3  <- rbind.fill(num_pat_16, dat_conti) 
num_pat_16_3  <- as.data.table(num_pat_16_3)

num_pat_16_3 <- setDT(num_pat_16_3)[, share_inv := share_inv/share_inv[ipc_3 == "all"], .(geo, p_year)] 
num_pat_16_3 <- mutate(num_pat_16_3, abs_rel = "share_all")

num_pat_16_4 <- as.data.table(num_pat_16_3)
num_pat_16_4 <- setDT(num_pat_16_4)[, share_inv := share_inv/share_inv[geo == "world"], .(ipc_3, p_year)]  
num_pat_16_4 <- mutate(num_pat_16_4, abs_rel = "rca")
num_pat_16_3 <- filter(num_pat_16_3, geo != "world" & ipc_3 != "all")
num_pat_16_4 <- filter(num_pat_16_4, geo != "world" & ipc_3 != "all")

## Add all data together
num_pat_16 <- rbind.fill(num_pat_16, num_pat_16_2, num_pat_16_3, num_pat_16_4)
})
) %>% 
saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds"))

## Add relative share
num_pat_16 <- readRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds"))
rbind(readRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds")), 
      setDT(num_pat_16)[, c("share_inv", "geo_one_inv") := list(share_inv[geo_one_inv == "no"]/share_inv[geo_one_inv == "yes"], "no_yes"), .(p_year, ipc_3, geo, abs_rel)]) %>% 
saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds"))

## Add yearly share of an geographic region 
num_pat_16 <- readRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds"))
num_pat_16  <- setDT(num_pat_16)[, ges_pat_year := sum(share_inv[abs_rel == "abs" & ipc_3 == "all"]), .(p_year)]
num_pat_16  <- setDT(num_pat_16)[, ges_pat_geo_year := sum(share_inv[abs_rel == "abs" & ipc_3 == "all"]), .(geo, p_year)]
num_pat_16  <- mutate(num_pat_16, share_geo = ges_pat_geo_year / ges_pat_year)
num_pat_16  <- filter(num_pat_16, abs_rel %in% c("abs"))
rbind(readRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds")), 
      setDT(num_pat_16)[, c("share_geo", "abs_rel") := list(share_geo/share_geo[p_year == 1990], "rel_1990"), .(ipc_3, geo)]) %>% 
saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", tech_field_start, ".rds"))

##################################################
## Get inventors shares for world class patents ##
##################################################
do.call(rbind, lapply(c("no", "yes"), function(geo_one_inv){
world_class_conti <- num_pat_geo("conti", "conti", tech_field_start, "yes", geo_one_inv) 
world_class_ctry <- num_pat_geo("ctry_code", "ctry_name", tech_field_start, "yes", geo_one_inv) 
world_class_reg <- num_pat_geo("Up_reg_code", "Up_reg_label", tech_field_start, "yes", geo_one_inv) 
world_class_16 <- rbind.fill(world_class_conti, world_class_ctry, world_class_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti), abs_rel = "abs") 

## Create some relative outputs used in innoscape_pharma.Rmd
## relative to 1990
world_class_16_2 <- world_class_16
world_class_16_2 <- setDT(world_class_16_2)[, share_inv := share_inv/share_inv[p_year == 1990], .(geo, ipc_3)] 
world_class_16_2 <- mutate(world_class_16_2, abs_rel = "rel_1990")

## patent per country to all patents per year and RCA (relative share of IPC to all patents per country to relative share of IPC to all patetnets of all countries)
world_class_conti <- dplyr::filter(world_class_16, is.na(conti) != T) 
world_class_conti <- cbind(geo = "world", aggregate(share_inv ~ p_year + ipc_3, FUN = sum, data = world_class_conti))
world_class_16_3  <- rbind.fill(world_class_16, world_class_conti) 
world_class_16_3 <- setDT(world_class_16_3)[, share_inv := share_inv/share_inv[ipc_3 == "all"], .(geo, p_year)]  
world_class_16_3 <- mutate(world_class_16_3, abs_rel = "share_all")

world_class_16_4 <- world_class_16_3
world_class_16_4 <- setDT(world_class_16_4)[, share_inv := share_inv/share_inv[geo == "world"], .(ipc_3, p_year)]  
world_class_16_4 <- mutate(world_class_16_4, abs_rel = "rca")
world_class_16_3 <- filter(world_class_16_3, geo != "world"& ipc_3 != "all")
world_class_16_4 <- filter(world_class_16_4, geo != "world"& ipc_3 != "all")

## Add all data together
world_class_16 <- rbind.fill(world_class_16, world_class_16_2, world_class_16_3, world_class_16_4) 
})
) %>% 
saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_", tech_field_start, ".rds"))

## Add relative share
world_class_16 <- readRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_", tech_field_start, ".rds"))
rbind(readRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_", tech_field_start, ".rds")), 
      setDT(world_class_16)[, c("share_inv", "geo_one_inv") := list(share_inv[geo_one_inv == "no"]/share_inv[geo_one_inv == "yes"], "no_yes"), .(p_year, ipc_3, geo, abs_rel, cit_cat_y_5)]) %>% 
saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/world_class_", tech_field_start, ".rds"))




