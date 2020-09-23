print("23: Create data for part II.	Innovation in the Swiss Pharma Sector / only triadic patents are used and inventors are adjusted for cross-border commuters to Switzerland / CR 23.9.2020")

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
## Latest year for which world class patents should be calculated / world class = 10% most cited patents per year over the next five years after filing
max_year_past    <- 2015 

#########################################
## Load data on geography of inventors ##
#########################################
## NEW: adjusted by cross-border commuters
inv_reg <- readRDS(paste0(mainDir1,  "/created data/inv_reg_CHcommute_adj.rds")) 
inv_reg <- dplyr::rename(inv_reg, Up_reg_label = regio_pat, ctry_code = ctry_pat)

## Focus only on subset of triadic patents
tpf <- readRDS(paste0(mainDir1, "/created data/triadic_fam.rds"))

inv_reg <- mutate(inv_reg, triadic = ifelse(p_key %in% unique(tpf$p_key) | patent_id %in% unique(tpf$patent_id), 1, 0))
inv_reg <- setDT(inv_reg)[, ind_triad := sum(triadic, na.rm = T), .(p_key)]
inv_reg <- filter(inv_reg, ind_triad > 0)

## By following subset only granted patents are considered. It, however, overwrites the existing .RDS files
## inv_reg <- filter(inv_reg, granted == "yes")

## Create function to get number of patents for different geographical entities per tech_field and p_year
num_pats_geo_func <- function(tech_field_start, max_year_past){
if(tech_field_start == "all"){
inv_reg <- distinct(inv_reg, p_key, name, .keep_all = T)  
}  else {
inv_reg <- filter(inv_reg, is.na(tech_field) != T & tech_field == tech_field_start)
}
## For the analysis, use ctry_pat and regio_pat 
# inv_reg <- dplyr::select(inv_reg, -Ctry_code, -Up_reg_label)
# inv_reg <- dplyr::rename(inv_reg, ctry_code = Ctry_code , Up_reg_label = regio_pat)
inv_reg <- mutate(inv_reg, conti = countrycode(ctry_code, origin = "eurostat", destination = "continent"), ctry_name = countrycode(ctry_code, "iso2c", "country.name.en"), Up_reg_label = paste0(ctry_code, " - ", Up_reg_label))
inv_reg <- setDT(inv_reg)[, share_inv := 1/.N, .(p_key)]
inv_reg <- mutate(inv_reg, ipc_3 = substr(ipc_main, 1, 3)) %>% dplyr::select(-ipc_main)

## Add list of IPC per patent created in 08b_p_key_ipc.R -> Deprectad, however, it could be used later on to add different technology fields
# ipc <- readRDS(paste0(mainDir1, "/created data/ipc_list.rds"))
# ipc <- setDT(ipc)[, .SD[1], .(p_key)]
# ipc <- dplyr::select(ipc, -ipc_main, -ipc_list, -ipc_3_list, -ict, -ai)
# inv_reg <- left_join(inv_reg, ipc, by = c("p_key"))

## Load name of IPC
# ipc_name <- read.csv2(paste0(mainDir1, "/raw data/ipc_3_digit.csv")) %>% dplyr::select(Code.1, Description)
# inv_reg <- left_join(inv_reg, ipc_name, by = c("ipc_3" = "Code.1"))

## World-class patents only considered when calculating data for a tech_field 
if(tech_field_start != "all"){
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

## keep only USPTO patents
pat_past <- filter(pat_past, pat_off == "US" & tech_field %in% tech_field_start)
pat_past <- mutate(pat_past, filing = as.character(filing))
get_cut_off <- do.call(rbind, lapply(seq(1990, max_year_past, 1), function(x) cit_quant(c("US"), c(0, 0.6, 0.9), pat_past, x)))
get_cut_off <- dcast(get_cut_off, pub_year ~ class, value.var = "cut_off")
colnames(get_cut_off) <- c("filing", paste0("class_", seq(1, 3, 1))) 
get_cut_off <- mutate(get_cut_off, filing = as.character(filing))
pat_past <- left_join(pat_past, get_cut_off, by = "filing")
pat_past <- mutate(pat_past, cit_cat_y_5 = case_when(fwd_cits5 <= class_2 ~ 0, fwd_cits5 > class_2 & fwd_cits5 < class_3 ~ 1, fwd_cits5 >= class_3 ~ 2))
pat_past <- dplyr::select(pat_past, p_key, cit_cat_y_5, pat_off) %>% mutate(p_key = as.character(p_key))
pat_past <- filter(pat_past, is.na(cit_cat_y_5) != T)

## Add together with data on patents
inv_reg <- left_join(inv_reg, pat_past, by = c("p_key"))
} 


############################################
## b.	Swiss Pharma in the Domestic Economy #
############################################
## Function to calculate number of patents per geographic entity
num_pat_geo <- function(geo_level, geo_name, tech_field_start, world_class){
  # inv_reg <- filter(inv_reg, tech_field %in% tech_field_start)

  ## find important countries and regions
  imp <- aggregate(as.formula(paste0("share_inv ~", geo_level)), data = filter(inv_reg, p_year > 1989), FUN = sum)
  imp <- arrange(imp, -share_inv) 
  
  if(world_class == "yes"){
    form <- as.formula(paste0("share_inv ~ cit_cat_y_5 + p_year + ", geo_level, " + ", geo_name))
    reg     <- aggregate(form, FUN = sum, data = inv_reg) %>% mutate(tech_field = tech_field_start)
    
    ## Calculate yearly share of an geographic region 
    reg <- setDT(reg)[, ges_pat_year := sum(share_inv), .(p_year, cit_cat_y_5)]
    reg <- setDT(reg)[, ges_pat_geo_year := sum(share_inv), .(get(geo_level), cit_cat_y_5, p_year)]
    reg <- mutate(reg, share_geo = ges_pat_geo_year / ges_pat_year)
    
  }else{
    form <- as.formula(paste0("share_inv ~ p_year + ", geo_level, " + ", geo_name))
    reg     <- aggregate(form, FUN = sum, data = inv_reg) %>% mutate(tech_field = tech_field_start)
    
    ## Calculate yearly share of an geographic region 
    reg <- setDT(reg)[, ges_pat_year := sum(share_inv), .(p_year)]
    reg <- setDT(reg)[, ges_pat_geo_year := sum(share_inv), .(get(geo_level), p_year)]
    reg <- mutate(reg, share_geo = ges_pat_geo_year / ges_pat_year)
}
  
if(geo_level == "ctry_code"){
  if(tech_field_start == "all"){
    list_geo <- imp[, geo_level]
  } else {
        list_geo <- imp[1:15, geo_level]
  }
  } else if(geo_level == "Up_reg_label"){
    if(tech_field_start == "all"){
      list_geo <- imp[, geo_level]
    } else {
      list_geo <- imp[1:80, geo_level]
  }
  }  else if(geo_level == "conti"){
    list_geo <- c( "Europe", "Oceania", "Americas", "Asia", "Africa")
  }
  
  reg <- reg %>% subset(p_year > 1989 & get(geo_level) %in% list_geo)
  output <- reg %>% mutate(p_year = as.numeric(as.character(p_year)))
  return(output)
}

################################################
## Get inventors shares of patents in general ##
################################################
num_pat_conti <- num_pat_geo("conti", "conti", tech_field_start, "no") 
num_pat_ctry <- num_pat_geo("ctry_code", "ctry_name", tech_field_start, "no") 
num_pat_reg <- num_pat_geo("Up_reg_label", "Up_reg_label", tech_field_start, "no") 
num_pat_16 <- rbind.fill(num_pat_conti, num_pat_ctry, num_pat_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti), abs_rel = "abs") 

## Create some relative outputs used in innoscape_pharma.Rmd
## relative to 1990
num_pat_16_2 <- data.table(num_pat_16)
num_pat_16_2 <- setDT(num_pat_16_2)[, share_inv := share_inv/share_inv[p_year == 1990], .(geo)] 
num_pat_16_2 <- setDT(num_pat_16_2)[, share_geo := share_geo/share_geo[p_year == 1990], .(geo)] 
num_pat_16_2 <- mutate(num_pat_16_2, abs_rel = "rel_1990")

## Add all data together
num_pat_16 <- rbind.fill(num_pat_16, num_pat_16_2)
num_pat_16 <- mutate(num_pat_16, geo_level = case_when(is.na(conti) != T ~ "conti", is.na(ctry_code) != T ~ " ctry", is.na(Up_reg_label) != T ~ "regio"), world_class = "no")
num_pat_16 <- dplyr::rename(num_pat_16, Continent = conti, Region = Up_reg_label, Country = ctry_name)

## Some cleanings
num_pat_16 <- filter(num_pat_16, str_detect(geo, "- NA") != T)

num_pat_16 %>% 
saveRDS(paste0("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/num_pats_geo/num_pat_", tech_field_start, ".rds"))


##################################################
## Get inventors shares for world class patents ##
##################################################
if(tech_field_start != "all"){
world_class_conti <- num_pat_geo("conti", "conti", tech_field_start, "yes") 
world_class_ctry <- num_pat_geo("ctry_code", "ctry_name", tech_field_start, "yes") 
world_class_reg <- num_pat_geo("Up_reg_label", "Up_reg_label", tech_field_start, "yes") 
world_class_16 <- rbind.fill(world_class_conti, world_class_ctry, world_class_reg) %>% mutate(geo = ifelse(is.na(conti) == T, ifelse(is.na(ctry_name) == T, Up_reg_label, ctry_name), conti), abs_rel = "abs") 

## Create some relative outputs used in innoscape_pharma.Rmd
## relative to 1990
world_class_16_2 <- data.table(world_class_16)
world_class_16_2 <- setDT(world_class_16_2)[, share_inv := share_inv/share_inv[p_year == 1990], .(geo, cit_cat_y_5)] 
world_class_16_2 <- setDT(world_class_16_2)[, share_geo := share_geo/share_geo[p_year == 1990], .(geo, cit_cat_y_5)] 
world_class_16_2 <- mutate(world_class_16_2, abs_rel = "rel_1990")

## Add all data together
world_class_16 <- rbind.fill(world_class_16, world_class_16_2)
world_class_16 <- mutate(world_class_16, geo_level = case_when(is.na(conti) != T ~ "conti", is.na(ctry_code) != T ~ " ctry", is.na(Up_reg_label) != T ~ "regio"), world_class = "yes")
world_class_16 <- dplyr::rename(world_class_16, Continent = conti, Region = Up_reg_label, Country = ctry_name)

## Some cleanings
world_class_16 <- filter(world_class_16, str_detect(geo, "- NA") != T & cit_cat_y_5 == 2)

world_class_16 %>% 
  saveRDS(paste0("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/num_pats_geo/world_class_", tech_field_start, ".rds"))
}
}

lapply(c(16, "all"), function(x) num_pats_geo_func(x, 2015))


## Add different calculations together
all <- readRDS(paste0("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/num_pats_geo/num_pat_", "all", ".rds"))
pharma <- readRDS(paste0("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/num_pats_geo/num_pat_", 16, ".rds"))

## Calculate share of geo of all patents of tech_field
pharma_share <- dplyr::select(pharma, -share_inv) %>% dplyr::rename(share_inv = share_geo) %>% mutate(indicator = case_when(abs_rel == "abs" ~ "share_pharma", abs_rel == "rel_1990" ~ "share_pharma_rel_1990"))
pharma_share <- pharma_share  %>% filter(world_class == "no") %>% dplyr::select(-world_class)
## Calculate share of tech_field 
pharma_all <- rbind.fill(all, pharma)
pharma_all <- filter(pharma_all, world_class == "no" & abs_rel == "abs") %>% dplyr::select(-world_class, -abs_rel)
pharma_all_geo <- dcast(pharma_all, p_year + geo + geo_level + ctry_code + Country + Region ~ tech_field, value.var = "share_inv")
pharma_all_geo <- mutate(pharma_all_geo, share_inv = `16`/all, indicator = "share_pharma_total", tech_field = 16) %>% dplyr::select(-all, -`16`)

## Calculate total number patents 
pharma_all_world <- filter(pharma_all, geo == "Europe")
pharma_all_world <- distinct(pharma_all_world, tech_field, p_year, .keep_all = T)
pharma_all_world <- dcast(pharma_all_world, p_year~ tech_field, value.var = "ges_pat_year")
pharma_all_world <- mutate(pharma_all_world , inv_share_world = `16`/all) %>% dplyr::select(p_year, inv_share_world)

## Calculate RCA of geo_level: share_geo/share_world
pharma_all_rca <- left_join(pharma_all_geo, pharma_all_world, by = c("p_year"))
pharma_all_rca <- mutate(pharma_all_rca, share_inv = share_inv / inv_share_world, indicator = "rca_pharma_total", tech_field = 16) %>% dplyr::select(-inv_share_world)

## Put all together
pharma <- dplyr::rename(pharma, indicator = abs_rel) %>% filter(world_class == "no") %>% dplyr::select(-world_class)
pharma <- rbind.fill(pharma, pharma_share, pharma_all_geo, pharma_all_rca )
pharma <- dplyr::select(pharma, -ges_pat_year, -ges_pat_geo_year, -abs_rel, -share_geo)
pharma <- filter(pharma, is.na(share_inv) != T)
pharma %>% saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/num_pat_", 16, ".rds"))


