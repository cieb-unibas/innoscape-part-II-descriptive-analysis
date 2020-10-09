print("Create economic indicators for descriptive analysis of pharma industry")
# Last modified 01.10.2020 / DF

require(data.table)
require(plyr)
library(dplyr)
library(tidyr)
require(ggplot2)
library(tidyverse)
library(readr)
library(readxl)
library(countrycode)
library(viridis)
library(reshape2)
library(stringr)
library("rio")
rm(list = ls())

################################################################################
#a. Employment figures #
################################################################################

# Load data on employment / raw data not on Github
  ilo_2020 <-read.csv2("/scicore/home/weder/GROUP/Innovation/02_section_I_data/EMP_TEMP_SEX_EC2_NB_A.csv",header=T,sep=",",stringsAsFactors = FALSE,dec=".")

# Filtering and cleaning
  ilo_2020 <- filter(ilo_2020, sex == "SEX_T")
  colnames(ilo_2020)
  ilo_2020 <-subset(ilo_2020, select = c("ref_area", "classif1", "time", "obs_value" ))
  ilo_2020 <-dplyr::filter(ilo_2020, grepl('EC2_ISIC4', classif1))
  ilo_2020 <-as.data.frame(sapply(ilo_2020,gsub,pattern="EC2_ISIC4_",replacement=""))
  ilo_2020 %>% mutate_if(is.factor, as.character) -> ilo_2020 # transforming factors into characters
  ilo_2020$obs_value <- as.numeric(ilo_2020$obs_value)
  
  colnames(ilo_2020) <- c("country", "IND", "year", "num.emp")

# Calculating the share of employment for each industry of every country in a given year  
  setDT(ilo_2020)[,tot_emp:=num.emp[IND == "TOTAL"],by=list(country, IND, year)]
  ilo_2020$tot_emp[is.na(ilo_2020$tot_emp)] <- 0
  setDT(ilo_2020)[,tot_emp:=max(tot_emp),by=list(country, year)]
  ilo_2020 <- subset(ilo_2020,IND!="TOTAL")
  setDT(ilo_2020)[,share.emp:=num.emp/tot_emp,by=list(country, IND, year)]
  
# Filtering industries (manufacturing only)  
  ilo_2020 <- filter(ilo_2020, IND %in% c("B05","B06","B07","B08","B09", 
                                          "C10", "C11", "C12", "C13", "C14", "C15", 
                                          "C16", "C17", "C18", "C19", "C20", "C21", 
                                          "C22", "C23", "C24", "C25", "C26", "C27" , 
                                          "C28", "C29", "C30", "C31", "C32", "C33"))
  
  ilo_2020$IND <- sub("B", "", ilo_2020$IND) # removing B symbol from ind name
  ilo_2020$IND <- sub("C", "", ilo_2020$IND) # removing C symbol from ind name
  
# Aggregating industries to match NOGA representation of FSO  
  ilo_2020 <- mutate(ilo_2020, ind.code = case_when(IND %in% c("05", "06", "07", "08", "09") ~ "5-9",
                                                    IND %in% c("16", "17", "18") ~ "16-18",
                                                    IND %in% c("22", "23") ~ "22-23",
                                                    IND %in% c("24", "25") ~ "24-25",
                                                    IND %in% c("29", "30") ~ "29-30",
                                                    IND %in% c("31", "32", "33") ~ "31-33",
                                                    IND == "21" ~ "21",
                                                    IND == "26" ~ "26",
                                                    IND == "27" ~  "27",
                                                    IND == "28" ~  "28",
                                                    IND %in% c("10", "11", "12") ~  "10-12",
                                                    IND %in% c("13", "14", "15") ~  "13-15",
                                                    IND %in% c("19", "20") ~  "19-20"))
  ilo_2020 %>% drop_na() -> ilo_2020 # droping n.a.s
  
  ilo_2020 <- ilo_2020 %>% 
    group_by(ind.code, country, year) %>% 
    summarise_at(vars(num.emp, share.emp), funs(sum(.,na.rm=TRUE))) # summing emp variables for new industry groups

#Transforming into long format  
  ilo_2020 <-melt(setDT(ilo_2020), id.vars = c("country", "ind.code", "year"), measure.vars = c("num.emp", "share.emp"))
  
#Loading the industry labels / raw data not on Github
  labels_ind <- read_excel("/scicore/home/weder/GROUP/Innovation/02_section_I_data/label_sector.xlsx", sheet = "Sheet1")
  labels_ind %>% mutate_if(is.factor, as.character) -> labels_ind  

#Merging employment data and labels
  iloemp_2020_final<-merge(ilo_2020, labels_ind)
  
#Saving data
  saveRDS(object = iloemp_2020_final, file = paste0(getwd(), "/Data creation/iloemp_2020_final.rds"))  
  write.csv(iloemp_2020_final, paste0(getwd(), "/Data creation/iloemp_2020_final.csv"), row.names = FALSE)  
  

  
################################################################################
#b. GVA figures #
################################################################################

# Load data on GVA / raw data not on Github
  gva_abs <-read_excel("/scicore/home/weder/GROUP/Innovation/02_section_I_data/gva_sectoral.xlsx", sheet = "gva_absol")

# Wide to long format
  gva_abs<-reshape(gva_abs, 
                   direction = "long",
                   varying = list(names(gva_abs)[3:25]),
                   v.names = "gva_abs",
                   idvar = c("ind.code", "ind.name"),
                   timevar = "year",
                   times = 1997:2019)

# Filtering and cleaning data
  gva_abs <-subset(gva_abs, select = c("ind.code", "ind.name", "year", "gva_abs"))
  row.names(gva_abs) <- NULL
  gva_abs <-na.omit(gva_abs)
  gva_abs %>% mutate_if(is.factor, as.character) -> gva_abs

# Calculating GVA shares by industry and year
  setDT(gva_abs)[,tot:=sum(gva_abs),by=list(year)]
  setDT(gva_abs)[,gva_share:=gva_abs/tot,by=list(ind.code, ind.name, year)]

# Calculating GVA pct change (y-o-y) by industry
  gva_abs %>%
    group_by(ind.code) %>% 
    arrange(year, .by_group = TRUE) %>%
    mutate(gva_prchange = (gva_abs-lag(gva_abs))/lag(gva_abs)) -> gva_abs
 
# Filtering needed columns and manufacturing sector only
  gva_data_ch <-subset(gva_abs, select = c("ind.code", "ind.name", "year", "gva_abs", "gva_share","gva_prchange"))
  gva_data_ch <- filter(gva_data_ch, ind.code %in% c("5-9", "10-12", "13-15", "16","17","18", "19-20", "21","22", "23","24", "25", "26", "27", "28", "29", "30", "31", "32", "33"))
  
# Transforming GVA figures to long format
  gva_data_ch <-melt(setDT(gva_data_ch), id.vars = c("ind.code", "ind.name", "year"), measure.vars = c("gva_prchange","gva_abs", "gva_share"))

# Transforming GVA figures to long format
  saveRDS(object=gva_data_ch, file = paste0(getwd(), "/report/gva_data_ch.rds"))
  write.csv(gva_data_ch, paste0(getwd(), "/report/gva_data_ch.csv"), row.names = FALSE)  
  
  
################################################################################
#c. Labor productivity #
################################################################################

#Load JOBS data (BFS)
  emp_bsf <-read_excel("/scicore/home/weder/GROUP/Innovation/02_section_I_data/emp_sectoral_quarter.xlsx", sheet = "Sheet1")

# Wide to long format
  emp_bsf<-reshape(emp_bsf, 
                   direction = "long",
                   varying = list(names(emp_bsf)[3:31]),
                   v.names = "empbsf",
                   idvar = c("ind.code", "ind.name"),
                   timevar = "year",
                   times = 1991:2019)
  row.names(emp_bsf) <- NULL
  
# Transform factors to characters
emp_bsf %>% mutate_if(is.factor, as.character) -> emp_bsf
  
# Filtering manufacturing industries
  emp_bsf <- filter(emp_bsf, ind.code %in% c("5-9", "10-12", "13-15", "16-18", "19-20", "21", "22-23", "24-25", "26", "27", "28", "29-30", "31-33"))
  
  
# Loading GVA absolute values
  gva_absval <-read_excel("/scicore/home/weder/GROUP/Innovation/02_section_I_data/gva_sectoral.xlsx", sheet = "gva_absol")
  
# Wide to long format
  gva_absval<-reshape(gva_absval, 
                      direction = "long",
                      varying = list(names(gva_absval)[3:25]),
                      v.names = "gva_abs",
                      idvar = c("ind.code", "ind.name"),
                      timevar = "year",
                      times = 1997:2019)
  
# Leaving only necessary columns
  gva_absval <-subset(gva_absval, select = c("ind.code", "year", "gva_abs"))

# Grouping industries to match BFS jobs data
  gva_absval <- filter(gva_absval, ind.code %in% c("05-09", "10-12", "13-15", "16", "17", "18", "19-20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31" , "32" , "33", "33"))
  gva_absval <- mutate(gva_absval, ind.code = case_when(ind.code %in% c("16", "17", "18") ~ "16-18",
                                                        ind.code %in% c("22", "23") ~ "22-23",
                                                        ind.code %in% c("24", "25") ~ "24-25",
                                                        ind.code %in% c("29", "30") ~ "29-30",
                                                        ind.code %in% c("31", "32", "33") ~ "31-33",
                                                        ind.code == "21" ~ "21",
                                                        ind.code == "26" ~ "26",
                                                        ind.code == "27" ~  "27",
                                                        ind.code == "28" ~  "28",
                                                        ind.code == "10-12" ~  "10-12",
                                                        ind.code == "13-15" ~  "13-15",
                                                        ind.code == "19-20" ~  "19-20",
                                                        ind.code == "05-09" ~  "5-9"))

# Summing GVA according to new industry groups
  gva_absval <- gva_absval %>% 
    group_by(ind.code, year) %>% 
    summarise(gva_abs = sum(gva_abs))
  
# MERGING GVA absolute values and EMPLOYMENT (JOBS) BFS
  gva_absval_empbsf<-merge(emp_bsf, gva_absval)
  
# Loading data on CROSS-BORDER COMMUTERS
  cross_border <- read_excel("/scicore/home/weder/GROUP/Innovation/02_section_I_data/grenzgaenger_ch.xlsx", sheet = "Sheet1")

# Wide to long format
  cross_border<-reshape(as.data.frame(cross_border), 
                        direction = "long",
                        varying = list(names(cross_border)[2:19]),
                        v.names = "crossbord",
                        idvar = c("ind.code"),
                        timevar = "year",
                        times = 2002:2019)
  row.names(cross_border) <- NULL
  
# Loading EMPLOYMENT ILO data
  emp_iloch <- readRDS("/scicore/home/weder/GROUP/Innovation/02_section_I_data/ilo_2020_final.rds")
  
# Filtering for needed columns, Switzerland figures and manufacturing industries
  emp_iloch <-subset(emp_iloch, select = c("IND", "country", "year", "nemp"))
  colnames(emp_iloch) <- c("ind.code", "country", "year", "empilo")
  emp_iloch <- filter(emp_iloch, country == "CHE")
  emp_iloch <- filter(emp_iloch, ind.code %in% c("B08", "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C27" , "C28", "C29", "C30", "C31", "C32", "C33"))
  emp_iloch$ind.code <- sub("C", "", emp_iloch$ind.code)
  emp_iloch$ind.code <- sub("B", "", emp_iloch$ind.code)

# Grouping manufacturing industries to match BFS classification
  emp_iloch <- mutate(emp_iloch, ind.code = case_when(ind.code %in% c("16", "17", "18") ~ "16-18",
                                                      ind.code %in% c("22", "23") ~ "22-23",
                                                      ind.code %in% c("24", "25") ~ "24-25",
                                                      ind.code %in% c("29", "30") ~ "29-30",
                                                      ind.code %in% c("31", "32", "33") ~ "31-33",
                                                      ind.code == "21" ~ "21",
                                                      ind.code == "26" ~ "26",
                                                      ind.code == "27" ~  "27",
                                                      ind.code == "28" ~  "28",
                                                      ind.code == "08" ~  "5-9",
                                                      ind.code %in% c("10", "11", "12") ~  "10-12",
                                                      ind.code %in% c("13", "14", "15") ~  "13-15",
                                                      ind.code %in% c("19", "20") ~  "19-20"))
 
# Summing employment figures according to new industry groups
  emp_iloch <- emp_iloch %>% 
    group_by(ind.code, country, year) %>% 
    summarise(emp_ilo = sum(empilo))
  
# MERGING GVA and EMPLOYMENT (JOBS) BFS with ILO EMPLOYMENT 
  lab_prod_final<-merge(gva_absval_empbsf, emp_iloch)
 
# Bringing all figures to the same level of representation (to have single units, not 000 or mill.)
  lab_prod_final$empbsf <- lab_prod_final$empbsf
  lab_prod_final$emp_ilo <- lab_prod_final$emp_ilo * 1000
  lab_prod_final$gva_abs <- lab_prod_final$gva_abs * 1000000
  
# MERGING WITH CROSS BORDER COMMUTERS
  lab_prod_final_ch<-merge(lab_prod_final, cross_border)
 
# Creating employment figure that contains domestic + cross_border workers
  setDT(lab_prod_final_ch)[,ilo_cross:=emp_ilo+crossbord,by=list(ind.code, ind.name, year)]
  
# Calculating labor productivity indicator for three different employment definitions
  setDT(lab_prod_final_ch)[,ilo_prod:=gva_abs/emp_ilo,by=list(ind.code, ind.name, year)]
  setDT(lab_prod_final_ch)[,bfs_prod:=gva_abs/empbsf,by=list(ind.code, ind.name, year)]
  setDT(lab_prod_final_ch)[,ilocross_prod:=gva_abs/ilo_cross,by=list(ind.code, ind.name, year)]
  
# Filtering needed columns and transforming productivity figures to long format
  lab_prod_final_ch <-subset(lab_prod_final_ch, select = c("ind.code", "year", "ind.name", "ilo_prod", "bfs_prod", "ilocross_prod"))
  lab_prod_final_ch <-melt(setDT(lab_prod_final_ch), id.vars = c("ind.code", "year", "ind.name"), measure.vars = c("ilo_prod", "bfs_prod", "ilocross_prod"))
  
# SAVING THE FINAL DATASET
  saveRDS(object=lab_prod_final_ch, file = paste0(getwd(), "/Data creation/lab_prod_final_ch.rds"))
  write.csv(lab_prod_final_ch, paste0(getwd(), "/Data creation/lab_prod_final_ch.csv"), row.names = FALSE)  
  
################################################################################
#d. Export / trade figures #
################################################################################
 
# Loading trade data / raw data not on Github
  oecd.trade.new <- readRDS("/scicore/home/weder/GROUP/Innovation/02_section_I_data/oecd.trade.new.raw")

# Filtering important columns and adjusting their names
  colnames(oecd.trade.new) <- c("rep.code", "trade.flow", "imp.iso", "kind", "exp.industry", "value.share", "jahr", "exp.export")
  oecd.trade.new <- filter(oecd.trade.new, trade.flow == "EXPO" & kind == "TOTAL" & value.share == "VALUE")
  colnames(oecd.trade.new) <- c("rep.code", "trade.flow", "par.code", "kind", "ind.code", "value.share", "year", "value")
  oecd.trade.new <-subset(oecd.trade.new, select = c("rep.code", "par.code", "ind.code", "year", "value" ))
 
# Filtering manufacturing industries to match NOGA BFS
  oecd.trade.new <- filter(oecd.trade.new, ind.code %in% c("DTOTAL", "D01T03", "D05T08", "D10T12", "D13T15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T32", "D35"))

# Grouping industries to match NOGA BFS
  oecd.trade.new <- mutate(oecd.trade.new, ind.code = case_when(
    ind.code %in% c("D19", "D20") ~ "19-20",
    ind.code == "DTOTAL" ~ "TOTAL",
    ind.code == "D01T03" ~ "01-03",
    ind.code == "D05T08" ~ "05-09",
    ind.code == "D10T12" ~ "10-12",
    ind.code == "D13T15" ~ "13-15",
    ind.code == "D16" ~  "16",
    ind.code == "D17" ~  "17",
    ind.code == "D18" ~  "18",
    ind.code == "D21" ~  "21",
    ind.code == "D22" ~ "22",
    ind.code == "D23" ~  "23",
    ind.code == "D24" ~ "24",
    ind.code == "D25" ~  "25",
    ind.code == "D26" ~ "26",
    ind.code == "D27" ~ "27",
    ind.code == "D28" ~  "28",
    ind.code == "D29" ~ "29",
    ind.code == "D30" ~ "30",
    ind.code == "D31T32" ~ "31-33"))
 
# Transform factors to characters and make trade figures numeric
  oecd.trade.new %>% mutate_if(is.factor, as.character) -> oecd.trade.new
  oecd.trade.new$value <- as.numeric(oecd.trade.new$value)

# Sum values over new industry groups
  oecd.trade.new <- oecd.trade.new %>% 
    group_by(rep.code, par.code, ind.code, year) %>% 
    summarise(value = sum(value))
  
# Preparing gps coordinates for each country / raw data not on Github
  gps <-rio::import("/scicore/home/weder/GROUP/Innovation/02_section_I_data/gps.csv")
  names(gps)
  gpspar <-subset(gps, select = c("\"Alpha-3 code\"", "\"Latitude (average)\"", "\"Longitude (average)\"\"" )) 
  gpsrep <-subset(gps, select = c("\"Alpha-3 code\"", "\"Latitude (average)\"", "\"Longitude (average)\"\"" ))
  
  names(gpspar)[names(gpspar) == "\"Latitude (average)\""] <- "lat_par"
  names(gpspar)[names(gpspar) == "\"Longitude (average)\"\""] <- "long_par"
  names(gpspar)[names(gpspar) == "\"Alpha-3 code\""] <- "par.code"
  
  names(gpsrep)[names(gpsrep) == "\"Latitude (average)\""] <- "lat_rep"
  names(gpsrep)[names(gpsrep) == "\"Longitude (average)\"\""] <- "long_rep"
  names(gpsrep)[names(gpsrep) == "\"Alpha-3 code\""] <- "rep.code"
  
  gpspar$par.code = gsub("\"","",gpspar$par.code)
  gpspar$lat_par = gsub("\"","",gpspar$lat_par)
  gpspar$long_par = gsub("\"","",gpspar$long_par)
  
  gpsrep$rep.code = gsub("\"","",gpsrep$rep.code)
  gpsrep$lat_rep = gsub("\"","",gpsrep$lat_rep)
  gpsrep$long_rep = gsub("\"","",gpsrep$long_rep)
  
# Merging trade data with gps coordinates
  oecd_trade_final_2020_gpspar_new<-merge(oecd.trade.new,gpspar,by=c("par.code"))
  oecd_trade_final_2020_gps_par_rep_new<-merge(oecd_trade_final_2020_gpspar_new,gpsrep,by=c("rep.code"))
  
  oecd_trade_final_2020_gps_par_rep_new %>% mutate_if(is.factor, as.character) -> oecd_trade_final_2020_gps_par_rep_new # transforming factors into characters
  
# Preparing the industry labels / raw data not on Github
  sec_lab <-read_excel("/scicore/home/weder/GROUP/Innovation/02_section_I_data/sec_lab.xlsx", sheet = "Sheet1")
  
# Merging trade data with industry labels
  oecd_trade_final_2020_gps_par_rep_new<-merge(oecd_trade_final_2020_gps_par_rep_new,sec_lab,by=c("ind.code"))
  
# Calculating shares A. (Share of reporters industry exports in total exports to a given destination)
  oecd_trade_final_2020_gps_par_rep_new <-transform(oecd_trade_final_2020_gps_par_rep_new, val_export = as.numeric(value)) # Transform to numeric values
  setDT(oecd_trade_final_2020_gps_par_rep_new)[,totexp:=sum(val_export),by=list(rep.code, par.code, year)]
  oecd_trade_final_2020_gps_par_rep_new <-transform(oecd_trade_final_2020_gps_par_rep_new, totexp = as.numeric(totexp)) # Transform to numeric values
  setDT(oecd_trade_final_2020_gps_par_rep_new)[,share_in_tot:=val_export/totexp,by=list(rep.code, par.code, ind.code, year)]
  
# Calculating shares B. (Share of reporters industry exports in its total world exports in a given year)
  setDT(oecd_trade_final_2020_gps_par_rep_new)[,totexp2:=sum(val_export),by=list(rep.code, ind.code, year)]
  setDT(oecd_trade_final_2020_gps_par_rep_new)[,share_in_tot2:=val_export/totexp2,by=list(rep.code, par.code, ind.code, year)]
  
# Filtering columns needed
  oecd_trade_final_2020_gps_par_rep_new <-subset(oecd_trade_final_2020_gps_par_rep_new, select = c("rep.code", "par.code", "ind.code","ind.name", "year", "val_export","share_in_tot","share_in_tot2","lat_par","long_par","lat_rep","long_rep" ))
  oecd_trade_final_2020_gps_par_rep_new$ind.name <-as.character(oecd_trade_final_2020_gps_par_rep_new$ind.name)

# Transform trade indicators from wide to long format
  oecd_trade_final_2020_gps_par_rep_new_final <-melt(setDT(oecd_trade_final_2020_gps_par_rep_new), id.vars = c("rep.code","par.code","ind.code","ind.name","year","lat_par","long_par","lat_rep","long_rep"), measure.vars = c("share_in_tot","share_in_tot2","val_export"))
  
# Filtering out Switzerland and Pharma
  oecd_trade_ch_final <- subset(oecd_trade_final_2020_gps_par_rep_new_final,rep.code=="CHE" & ind.code=="21")
  
# SAVING DATA
  saveRDS(object = oecd_trade_ch_final, paste0(getwd(), "/Data creation/oecd_trade_ch_final.rds"))
  write.csv(oecd_trade_ch_final, aste0(getwd(), "/Data creation/oecd_trade_ch_final.csv"), row.names = FALSE)  
  
    

