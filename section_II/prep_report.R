## Calculate the final data used in the report II in order to reduce real-time calculations within r-markdown / CR 29.9.2020

library(tidyr)
library(tidytext)
library(stringr)
require(plyr)
require(data.table)
library(tidyverse)
require(rio)
require(countrycode)
library(dplyr)

#####################
# Data on Citations #
#####################
# Create function to make calculations for backward or forward citations. Set type = "forw" or type = "back". Moreover, cut_off determines the min share 
citations_func <- function(type = "back"){

# Load the data
if(type == "back"){
  citflow <- readRDS(paste0(getwd(), "/section_III/back_citations_16.rds"))
  } else if(type == "forw"){
    citflow <- readRDS(paste0(getwd(), "/section_III/forw_citations_16.rds"))
    citflow <- dplyr::rename(citflow, conti_citing = conti_cited, conti_cited = conti_citing, tech_field_citing = tech_field_cited, tech_field_cited = tech_field_citing, share_inv_cited = share_inv_citing, 
                           ctry_name_citing = ctry_name_cited, up_reg_label_citing = up_reg_label_cited,
                           ctry_name_cited = ctry_name_citing, up_reg_label_cited = up_reg_label_citing)
  }else{
  print("Wrong type used")
}

techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/oecd_tech_field.RDS")
colnames(techlab) <- c("tech_field_cited", "tech_name")
citflow_final <- merge(citflow, techlab, by = c("tech_field_cited"))

# (1) Data preparation,labeling and initial filtering
citflow_ctry <- filter(citflow_final, geo %in% c("ctry"))
citflow_ctry <- subset(citflow_ctry, 
                       select = c("p_year_citing", 
                                  "tech_field_citing", 
                                  "tech_name",
                                  "tech_field_cited", 
                                  "share_inv_cited", 
                                  "ctry_name_citing", 
                                  "ctry_name_cited"))

# Create new data in order to be able to consider citations made to pharma patents
citflow_ctry_temp <- filter(citflow_ctry, tech_field_cited == 16)
citflow_ctry_temp$tech_name <- "Cited Pharmaceuticals"
citflow_ctry_temp$tech_field_cited <- 40
citflow_ctry <- rbind(citflow_ctry_temp, citflow_ctry)

# Function to calculate a country's share of backward citations 
citflow_ctry_func <- function(ctry = "Switzerland"){
# citflow_ctry <- filter(citflow_ctry, ctry_name_cited != ctry & ctry_name_citing==ctry | ctry_name_cited == ctry & tech_name == "Pharmaceuticals")

citflow_ctry <- filter(citflow_ctry, ctry_name_citing==ctry)
citflow_ctry <- subset(citflow_ctry, select = c("tech_field_cited", 
                                               "tech_name", 
                                               "tech_field_citing",
                                               "ctry_name_cited" ,
                                               "ctry_name_citing", 
                                               "p_year_citing",
                                               "share_inv_cited"))

citflow_ctry %>% 
  mutate_if(is.factor, as.character) -> citflow_ctry # transforming factors into characters

citflow_ctry$group <- countrycode(sourcevar = citflow_ctry[, "ctry_name_cited"],
                                  origin = "country.name",
                                  destination = "continent") # Creating continent var (group)
# Keep own country seperate
citflow_ctry <- mutate(citflow_ctry, group = ifelse(ctry_name_cited == ctry_name_citing, "Domestic", group))

# Agregation summing, without cited pharma patents
citflow_ctry <- dplyr::group_by(citflow_ctry, tech_field_citing, tech_field_cited, tech_name, p_year_citing, group) %>%
  summarise(share_inv_cited = sum(share_inv_cited[tech_field_cited!="16"], na.rm = TRUE))%>%
  bind_rows(citflow_ctry, .)

# citflow_ctry <- aggregate(share_inv_cited ~ tech_field_citing + tech_field_cited + tech_name + p_year_citing + group + ctry_name_citing, FUN = function(x)sum(x, na.rm = T), data = filter(citflow_ctry, tech_field_cited!="16"))

# Creating a All (continents) group
citflow_ctry<-citflow_ctry %>%
  group_by(tech_field_cited, tech_field_citing, tech_name, p_year_citing, ctry_name_citing) %>%
  summarise(group = "All", share_inv_cited = sum(share_inv_cited[tech_field_cited!="16"], na.rm = TRUE)) %>%
  bind_rows(citflow_ctry, .)

# citflow_ctry_all <- data.frame(group = "All", aggregate(share_inv_cited ~ tech_field_citing + tech_field_cited + tech_name + p_year_citing + ctry_name_citing, 
#                                                         FUN = function(x)sum(x, na.rm = T), data = filter(citflow_ctry, tech_field_cited!="16")))

# citflow_ctry <- rbind(citflow_ctry, citflow_ctry_all)

# Create normalized shares per year
  citflow_ctry <- setDT(citflow_ctry)[, total_num := sum(share_inv_cited[group == "All"], na.rm = T), .(p_year_citing, ctry_name_citing)] %>%
    mutate(share_inv_cited = share_inv_cited / total_num)
  
# Create tech_field_cited by group
citflow_ctry <- mutate(citflow_ctry, tech_field_cited = ifelse(tech_field_cited != 16, paste0(tech_field_cited, substr(group, 1, 2)), tech_field_cited), ctry_cited = ctry, type = type)
}

citflow_ctry_data <- do.call(rbind.fill, lapply(list("Switzerland", "Germany", "United States", "Italy", "France", "China", "India", "Sweden", "Japan"), function(x) citflow_ctry_func(x)))

# Keep only flows from Asia, Americas and Europe; others are irrelevant
citflow_ctry_data <- filter(citflow_ctry_data, group %in% c("Asia", "Europe", "Americas", "All", "Domestic") & is.na(ctry_name_citing) != T)

# Change variable names again if type == "forw"
if(type == "forw"){
  citflow_ctry_data <- dplyr::rename(citflow_ctry_data, tech_field_citing = tech_field_cited, tech_field_cited = tech_field_citing, share_inv_citing = share_inv_cited, 
                           ctry_citing = ctry_cited)}

citflow_ctry_data %>% saveRDS(paste0("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/report/citflow_ctry_", type, ".rds"))
}

# Run the previsouly created function
lapply(list("back", "forw"), function(x) citations_func(x))

#####################
# Create trade data #
#####################

options(scipen=999)
# Load trade data
tradedata <- readRDS("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/section_I/oecd_trade_ch_final.rds")
# tradedata_temp <- filter(tradedata, year == 2018 & variable == "val_export")
# tradedata_temp <- distinct(tradedata_temp, par.code, .keep_all = T)
# tradedata_temp <- setDT(tradedata_temp)[order(-value), .SD[1:60, ]]
# tradedata <- filter(tradedata, par.code %in% unique(tradedata_temp$par.code))

tradedata <- filter(tradedata, year %in% c(1990, seq(1994, 2019, 5)))
tradedata <- mutate(tradedata, Country = paste0(countrycode(par.code, "iso3c", "country.name.en"), "\nYear: ", year, "\nValue: ", ifelse(variable %in% "val_export", paste0(round(value/1000, 0), " million USD"), paste0(round(value*100, 0), "%"))))

# filter the data to leave only Switzerland:
volumes <-transform(tradedata, year = as.numeric(year)) %>% # Transform to numeric values
  mutate_at(c("lat_par", "long_par", "lat_rep", "long_rep"), as.numeric)
volumes %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/report/trad_data.rds")

# Create the world map
map_world <- map_data(map = "world") %>%
  filter(region != "Antarctica")
map_world <- map_world[seq(1, nrow(map_world), 18), ]

map_world %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/report/map_data.rds")

##############################
# Create data on employment #
##############################
# Load data on employment
ilo <- readRDS(paste0(getwd(), "/section_I/iloemp_2020_final.rds"))
ilo <- subset(ilo, ind.code == "21")

# Rename countries from ISO to complete name
ilo <- mutate(ilo, country = countrycode(country, "iso3c", "country.name.en"))

# Keep only countries having at least 6 years of data
ilo <- setDT(ilo)[, num := .N, .(ind.code, country, variable)]
ilo <- filter(ilo, num >= 6) %>% dplyr::select(-num)

# Prepare the data
ilo <- mutate(ilo, variable = case_when(variable == "share.emp" ~ "Share of employed", 
                                        variable == "num.emp" ~ "Number of employed in thousands")) %>%
       mutate(Country = paste0(country, "\nYear: ", year, "\nIndicator: ", variable, "\nValue: ", 
                               ifelse(variable == "Share of employed", round(value, 4), round(value, 2))))
ilo[ilo$variable == "Share of employed", "value"] <- ilo[ilo$variable == "Share of employed", ]$value * 100

# save the data
ilo %>% saveRDS(paste0(getwd(), "/report/ilo.rds"))

#####################################
# Create data on labor productivity #
#####################################
# load & prepare the data
labprod <- readRDS(paste0(getwd(), "/section_I/lab_prod_final_ch.rds"))

# shorten industry names
labprod$ind.name <- gsub("Manufacture of ", "", x = labprod$ind.name)
labprod$ind.name <- paste0(toupper(substr(labprod$ind.name, 1, 1)),
                                  substr(labprod$ind.name, 2, nchar(labprod$ind.name)))

labprod <- mutate(labprod, variable = case_when(variable == "ilo_prod" ~ "Only domestic workers", 
                                                variable == "bfs_prod" ~ "Total jobs in firms",
                                                T ~ "Domestic and cross-border workers")) %>%
           mutate(Industry = paste0(ind.name, "\nYear: ", year, "\nIndicator: ", variable, "\nValue: ", round(value/1000000, 2), " in millions CHF"))

# save the data
labprod %>% saveRDS(paste0(getwd(), "/report/labprod.rds"))

####################################
# Create data on gross value added #
####################################

# load the data
gva <- readRDS(paste0(getwd(), "/section_I/gva_data.rds"))

# make sure we have the same as in labor productivity plot
industries <- select(labprod, ind.code, ind.name) %>% 
  rename(industry_name = ind.name) %>% distinct(ind.code, .keep_all = TRUE)
gva <- merge(gva, industries, by = "ind.code", all.x = TRUE)

# shorten industry names for the non-matched industries 
for(i in 1:nrow(gva)){
  if(is.na(gva$industry_name[i])){
  gva$industry_name[i] <- gsub("Manufacture of ", "", x = gva$ind.name[i])
  gva$industry_name[i] <- paste0(toupper(substr(gva$industry_name[i], 1, 1)),
                             substr(gva$industry_name[i], 2, nchar(gva$industry_name[i])))
  }
}
gva <- gva %>% select(- ind.name) %>% rename(ind.name = industry_name)
gva <- mutate(gva, variable = case_when(variable == "gva_prchange" ~ "GVA percentage change",
                                        variable == "gva_abs" ~ "GVA in millions CHF",
                                        variable == "gva_share" ~ "Share in GDP"
                                        )) %>%
  mutate(Industry = paste0(ind.name, "\nIndicator: ", variable, "\nYear: ", year, "\nValue: ", round(value, 2)))

# percentages
gva[gva$variable == "GVA percentage change", "value"] <- gva[gva$variable == "GVA percentage change", ]$value * 100
gva[gva$variable == "Share in GDP", "value"] <- gva[gva$variable == "Share in GDP", ]$value * 100

# save the data
gva %>% saveRDS(paste0(getwd(), "/report/gva_data_ch.rds"))

#################################
# Create data on patent counts #
#################################

# Load patent data 
numpat_data <- readRDS("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/section_II/num_pat_16.rds")

# Preparing tech field labels and filtering data
tech_label <- readRDS("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/section_I/oecd_tech_field.RDS")
colnames(tech_label) <- c("tech_field", "tech_name")

numpat_data_temp <- filter(numpat_data, p_year == 2000 & indicator == "abs")
numpat_data_temp <- setDT(numpat_data_temp)[, rank_2000 := dense_rank(desc(share_inv)), .(geo_level, indicator)]
numpat_data_temp <- dplyr::select(numpat_data_temp, geo, rank_2000)
numpat_data <- left_join(numpat_data, numpat_data_temp, by = c("geo"))
numpat_data <- merge(numpat_data, tech_label)

numpat_data <-subset(numpat_data, select = c("p_year", "tech_field", "tech_name", "Continent", "Region", "Country", "geo", "geo_level", "indicator", "share_inv", "rank_2000"))
unique(numpat_data$indicator)
numpat_data <- filter(numpat_data, indicator =="abs" | indicator =="share_pharma_total" | indicator =="rca_pharma_total")
numpat_data <- filter(numpat_data, p_year <=2015)
numpat_data <- dplyr::mutate(numpat_data, indicator = case_when(indicator == "abs" ~ "Absolute numbers", indicator == "share_pharma_total" ~ "% of total patents", indicator == "rca_pharma_total" ~ "RCA pharma patents"))

# Prepare the data
numpat_data <- mutate(numpat_data, Geo = paste0(geo, "\nIndicator: ", indicator, "\nYear: ", p_year, "\nValue: ", ifelse(indicator == "Absolute numbers", round(share_inv, 0), 
                                                                                                                         ifelse(indicator == "% of total patents", paste0(round(share_inv*100, 0), "%"),
                                                                                                                                round(share_inv, 2)))))

numpat_data <- as.data.frame(numpat_data)  
numpat_data <- mutate(numpat_data, `Rank based on year 2000` = rank_2000)

numpat_data %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part-II-descriptive-analysis/report/numpat_data.rds")




