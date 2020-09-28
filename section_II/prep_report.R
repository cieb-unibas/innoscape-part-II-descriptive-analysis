## Calculate the final data used in the report II in order to reduce real-time calculations within r-markdown / CR 28.9.2020

library(tidyr)
library(dplyr)
library(tidytext)
library(stringr)
require(plyr)
require(data.table)
library(tidyverse)
require(rio)
require(countrycode)


# Load data 
citflow <- readRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/section_III/back_citations_16.rds")
techlab <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/oecd_tech_field.RDS")
colnames(techlab) <- c("tech_field_cited", "tech_name")
citflow_final <- merge(citflow, techlab)

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
citflow_ctry <- filter(citflow_ctry, ctry_name_cited != ctry & ctry_name_citing==ctry | ctry_name_cited == ctry & tech_name == "Pharmaceuticals")
citflow_ctry <-subset(citflow_ctry, select = c("tech_field_cited", 
                                               "tech_name", 
                                               "tech_field_citing",
                                               "ctry_name_cited" ,
                                               "p_year_citing",
                                               "share_inv_cited"))

citflow_ctry %>% 
  mutate_if(is.factor, as.character) -> citflow_ctry # transforming factors into characters

citflow_ctry$group <- countrycode(sourcevar = citflow_ctry[, "ctry_name_cited"],
                                  origin = "country.name",
                                  destination = "continent") # Creating continent var (group)

# Agregation summing, without cited pharma patents
citflow_ctry <- citflow_ctry %>% 
  group_by(tech_field_citing, tech_field_cited, tech_name, p_year_citing, group) %>% 
  summarise(share_inv_cited = sum(share_inv_cited[tech_field_cited!="16"], na.rm = TRUE))

# Create normalized shares per year
citflow_ctry <- setDT(citflow_ctry)[, total_num := sum(share_inv_cited), .(p_year_citing)] %>%
  mutate(share_inv_cited = share_inv_cited / total_num)

# Creating a All (continents) group
citflow_ctry<-citflow_ctry %>% 
  group_by(tech_field_cited, tech_field_citing, tech_name, p_year_citing) %>% 
  summarise(group = "All", share_inv_cited = sum(share_inv_cited)) %>%                 
  bind_rows(citflow_ctry, .)

# Create tech_field_cited by group
citflow_ctry <- mutate(citflow_ctry, tech_field_cited = ifelse(tech_field_cited != 16, paste0(tech_field_cited, substr(group, 1, 2)), tech_field_cited), ctry_cited = ctry)
}

citflow_ctry_data <- do.call(rbind.fill, lapply(list("Switzerland", "Germany", "United States", "Italy", "France", "China", "India", "Sweden", "Ireland", "Japan", "Canada", "Israel"), function(x) citflow_ctry_func(x)))

# Keep only flows from Asia, Americas and Europe; others are irrelevant
citflow_ctry_data <- filter(citflow_ctry_data, group %in% c("Asia", "Europe", "Americas", "All"))
citflow_ctry_data %>% saveRDS("/scicore/home/weder/rutzer/innoscape/part II descriptive analysis/report/citflow_ctry.rds")


