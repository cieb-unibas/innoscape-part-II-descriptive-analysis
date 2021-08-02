## Calculate the final data used in the report II in order to reduce real-time calculations within r-markdown / CR 2.10.2020

library(tidyr)
library(tidytext)
library(stringr)
require(plyr)
require(data.table)
library(tidyverse)
require(rio)
require(countrycode)
library(dplyr)
library(fst)

#####################
# Data on Citations #
#####################
# Create function to make calculations for backward or forward citations. Set type = "forw" or type = "back". Moreover, cut_off determines the min share 
citations_func <- function(type = "back"){

# Load the data
if(type == "back"){
  citflow <- readRDS(paste0(getwd(), "/Data creation/back_citations_16.rds"))
  } else if(type == "forw"){
    citflow <- readRDS(paste0(getwd(), "/Data creation/forw_citations_16.rds"))
    citflow <- dplyr::rename(citflow, conti_citing = conti_cited, conti_cited = conti_citing, tech_field_citing = tech_field_cited, tech_field_cited = tech_field_citing, share_inv_cited = share_inv_citing, 
                           ctry_name_citing = ctry_name_cited, up_reg_label_citing = up_reg_label_cited,
                           ctry_name_cited = ctry_name_citing, up_reg_label_cited = up_reg_label_citing)
  }else{
  print("Wrong type used")
}

techlab <- readRDS(paste0(getwd(), "/Data creation/oecd_tech_field.RDS"))
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
citflow_cont <- citflow_ctry %>%
  group_by(tech_field_citing, tech_field_cited, tech_name, p_year_citing, group) %>%
  summarise(share_inv_cited = sum(share_inv_cited[tech_field_cited!="16"], na.rm = TRUE))


# Creating a All (continents) group
citflow_all <-citflow_ctry %>%
  group_by(tech_field_cited, tech_field_citing, tech_name, p_year_citing) %>%
  summarise(group = "All", share_inv_cited = sum(share_inv_cited[tech_field_cited!="16"], na.rm = TRUE)) 

# Put both together (all and continent)
citflow_ges <- rbind(citflow_cont, citflow_all)


# Create normalized shares per year
citflow_ges <- setDT(citflow_ges)[, total_num := sum(share_inv_cited[group == "All"], na.rm = T), .(p_year_citing)] %>%
    mutate(share_inv_cited = share_inv_cited / total_num)
  
# Create tech_field_cited by group
citflow_ges <- mutate(citflow_ges, tech_field_cited = ifelse(tech_field_cited != 16, 
                                                             ifelse(tech_field_cited == 40, paste0(16, substr(group, 1, 2)), paste0(tech_field_cited, substr(group, 1, 2))), tech_field_cited), 
                                   ctry_cited = ctry, type = type)



return(citflow_ges)
}

citflow_ctry_data <- do.call(rbind.fill, lapply(list("Switzerland", "Germany", "United States", "Italy", "France", "Sweden", "Japan"), function(x) citflow_ctry_func(x)))

# Keep only flows from Asia, Americas and Europe; others are irrelevant
citflow_ctry_data <- filter(citflow_ctry_data, group %in% c("Asia", "Europe", "Americas", "All", "Domestic"))

# Add zero to missings for network_plot
# add_missing <- function(year_check, ctry_check, group_check, type = "back", data){
#   data <- filter(data, p_year_citing == year_check, ctry_cited == ctry_check, group == group_check)  
#   
#   if(nrow(data) == 0){
#     new <- data.frame(tech_field_citing = 16, tech_field_cited = 16, tech_name  = "Pharmaceuticals", p_year_citing = year_check, group = group_check, share_inv_cited = 0, total_num  = 0, ctry_cited = ctry_check, type = type)
#   } else{
#     new <- NULL
#   }
#   
#   return(new)
# }
# 
# china_miss <- do.call(rbind, lapply(seq(1990, 2015, 1), function(x, y)add_missing(x, "China", y, type = type, data = citflow_ctry_data), c("Europe", "All", "Americas", "Domestic", "Asia")))
# india_miss <- do.call(rbind, lapply(seq(1990, 2015, 1), function(x, y)add_missing(x, "India", y, type = type, data = citflow_ctry_data), c("Europe", "All", "Americas", "Domestic", "Asia")))

# Add all data together
# citflow_ctry_data <- rbind(citflow_ctry_data, china_miss, india_miss)

#! I kept the variable names the same for forward and backward citations in order to easily create the forwarc network in citnetwork_output.RMD (ie just use the same names there)
citflow_ctry_data %>% saveRDS(paste0(getwd(), "/report_en/citflow_ctry_", type, ".rds"))
citflow_ctry_data %>% write.fst(paste0(getwd(), "/report_en/citflow_ctry_", type, ".fst"))
citflow_ctry_data %>% write.csv(paste0(getwd(), "/report_en/citflow_ctry_", type, ".csv"), row.names = F)
}

# Run the previously created function
lapply(list("back", "forw"), function(x) citations_func(x))


#####################
# Create trade data #
#####################

options(scipen=999)
# Load trade data
tradedata <- readRDS(paste0(getwd(), "/Data creation/oecd_trade_ch_final.rds"))

# Transform to numeric values
volumes <-transform(tradedata, year = as.numeric(year)) %>% 
  mutate_at(c("lat_par", "long_par", "lat_rep", "long_rep", "value"), as.numeric)

volumes <- mutate(volumes, Country = paste0(countrycode(par.code, "iso3c", "country.name.en"), 
           "\nYear: ", year, "\nValue: ", ifelse(variable %in% "val_export", paste0(round(value/1000, 0), " million USD"), paste0(round(value*100, 0), "%"))))

# Keep only countries having Swiss exports for at least 28 years
volumes <- distinct(volumes, par.code, year, variable, .keep_all = T)
volumes <- setDT(volumes)[, num := .N, .(par.code, variable)]
volumes <- filter(volumes, num > 27)

# Sort data
volumes <- volumes %>%
           dplyr::group_by(par.code, variable) %>%
           arrange(year, par.code, )
volumes <- volumes %>%
           filter(variable %in% c("share_in_tot2") & par.code %in% c("USA", "DEU"))

volumes %>% saveRDS(paste0(getwd(), "/report_en/trad_data.rds"))
volumes %>% write.fst(paste0(getwd(), "/report_en/trad_data.fst"))
volumes %>% write.csv(paste0(getwd(), "/report_en/trad_data.csv"), row.names = F)

# Create the world map
map_world <- map_data(map = "world") %>%
  filter(region != "Antarctica")
map_world <- map_world[seq(1, nrow(map_world), 18), ]

map_world %>% saveRDS(paste0(getwd(), "/report_en/map_data.rds"))
map_world %>% write.fst(paste0(getwd(), "/report_en/map_data.fst"))
map_world %>% write.csv(paste0(getwd(), "/report_en/map_data.csv"), row.names = F)


##############################
# Create data on employment #
##############################
# Load data on employment
ilo <- readRDS(paste0(getwd(), "/Data creation/iloemp_2020_final.rds"))
ilo <- subset(ilo, ind.code == "21")

# Rename countries from ISO to complete name
ilo <- mutate(ilo, country = countrycode(country, "iso3c", "country.name.en"))

# Keep only countries having at least 6 years of data
ilo <- setDT(ilo)[, num := .N, .(ind.code, country, variable)]
ilo <- filter(ilo, num >= 6) %>% dplyr::select(-num)

# Prepare the data
ilo <- mutate(ilo, variable = case_when(variable == "share.emp" ~ "Share of employed", 
                                        variable == "num.emp" ~ "Number of employed in thousands")) %>%
       mutate(Country = paste0(country, "\nIndicator: ", variable, "\nYear: ", year, "\nValue: ", 
                               ifelse(variable == "Share of employed", paste0(round(value*100, 2), "%"), paste0(round(value, 2), " thousand"))))
ilo[ilo$variable == "Share of employed", "value"] <- ilo[ilo$variable == "Share of employed", ]$value * 100

# save the data
ilo %>% saveRDS(paste0(getwd(), "/report_en/ilo.rds"))
ilo %>% write.fst(paste0(getwd(), "/report_en/ilo.fst"))
ilo %>% write.csv(paste0(getwd(), "/report_en/ilo.csv"), row.names = F)


#####################################
# Create data on labor productivity #
#####################################
# load & prepare the data
labprod <- readRDS(paste0(getwd(), "/Data creation/lab_prod_final_ch.rds"))

# shorten industry names
labprod$ind.name <- gsub("Manufacture of ", "", x = labprod$ind.name)
labprod$ind.name <- paste0(toupper(substr(labprod$ind.name, 1, 1)),
                                  substr(labprod$ind.name, 2, nchar(labprod$ind.name)))

labprod <- mutate(labprod, variable = case_when(variable == "ilo_prod" ~ "Only domestic workers", 
                                                variable == "bfs_prod" ~ "Total jobs in firms",
                                                T ~ "Domestic and cross-border workers")) %>%
           mutate(Industry = paste0(ind.name,  "\nIndicator: Labor productivity of ", tolower(variable), "\nYear: ", year, "\nValue: ", round(value/1000000, 2), " million CHF"))

# save the data
labprod %>% saveRDS(paste0(getwd(), "/report_en/labprod.rds"))
labprod %>% write.fst(paste0(getwd(), "/report_en/labprod.fst"))
labprod %>% write.csv(paste0(getwd(), "/report_en/labprod.csv"))

####################################
# Create data on gross value added #
####################################

# load the data
gva <- readRDS(paste0(getwd(), "/Data creation/gva_data_ch.rds"))

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
  mutate(Industry = paste0(ind.name, "\nIndicator: ", variable, "\nYear: ", year, "\nValue: ", ifelse(variable == "GVA in millions CHF", paste0(round(value, 2), " million CHF"), paste0(round(value*100, 2), "%"))))

# percentages
gva[gva$variable == "GVA percentage change", "value"] <- gva[gva$variable == "GVA percentage change", ]$value * 100
gva[gva$variable == "Share in GDP", "value"] <- gva[gva$variable == "Share in GDP", ]$value * 100

# save the data
gva %>% saveRDS(paste0(getwd(), "/report_en/gva_plot/gva_data_ch.rds"))
gva %>% write.fst(paste0(getwd(), "/report_en/gva_plot/gva_data_ch.fst"))
gva %>% write.csv(paste0(getwd(), "/report_en/gva_plot/gva_data_ch.csv"))

#################################
# Create data on patent counts #
#################################

# Load patent data 
numpat_data <- readRDS(paste0(getwd(), "/Data creation/num_pat_16.rds"))

# Preparing tech field labels and filtering data
tech_label <- readRDS(paste0(getwd(), "/Data creation/oecd_tech_field.RDS"))
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
numpat_data <- dplyr::mutate(numpat_data, indicator = case_when(indicator == "abs" ~ "Absolute numbers", indicator == "share_pharma_total" ~ "% of total patents", indicator == "rca_pharma_total" ~ "RCA of pharma patents"))

# Prepare the data
numpat_data <- mutate(numpat_data, Geo = paste0(geo, "\nIndicator: ", indicator, "\nYear: ", p_year, "\nValue: ", ifelse(indicator == "Absolute numbers", round(share_inv, 0), 
                                                                                                                         ifelse(indicator == "% of total patents", paste0(round(share_inv*100, 0), "%"),
                                                                                                                                round(share_inv, 2)))))

numpat_data <- as.data.frame(numpat_data)  
numpat_data <- mutate(numpat_data, `Rank based on year 2000` = rank_2000)

numpat_data %>% saveRDS(paste0(getwd(), "/report_en/numpat_data.rds"), compress = FALSE)
numpat_data %>% write.fst(paste0(getwd(), "/report_en/numpat_data.fst"))
numpat_data %>% write.csv(paste0(getwd(), "/report_en/numpat_data.csv"))




