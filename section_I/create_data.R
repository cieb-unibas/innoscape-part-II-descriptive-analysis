print("Create economic indicators for intro to part II descriptive analysis")
# Last modified 07.07.2020 / DF

require(data.table)
require(plyr)
library(dplyr)
require(ggplot2)
library(countrycode)
library(viridis)
library(reshape2)
library(stringr)
library("rio")
rm(list = ls())

########################
#a. Employment figures # 
########################

# ILO DATA #
names(ilo_2020)

ilo_2020 <-read.csv2("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/employees_01072020.csv",header=T,sep=",",stringsAsFactors = FALSE,dec=".")
ilo_2020 <-subset(ilo_2020, select = c("ref_area", "classif1", "time", "obs_value"))
split_sek <- data.frame(do.call('rbind', strsplit(as.character(ilo_2020$classif1),'_',fixed=TRUE)))
ilo_2020$IND<-paste0("D",split_sek$X3)
ilo_2020$ISIC<-paste0("D",split_sek$X2)
ilo_2020<-ilo_2020[apply(ilo_2020,1,function(rowdata){
  !any(grepl("ISIC3", rowdata))}),]
ilo_2020<-subset(ilo_2020, select = -c(classif1) )
names(ilo_2020)[names(ilo_2020) == "ref_area"] <- "country"
names(ilo_2020)[names(ilo_2020) == "obs_value"] <- "nemp"
names(ilo_2020)[names(ilo_2020) == "time"] <- "year"
col_order <- c("country", "IND", "year", "nemp")
ilo_2020 <- ilo_2020[, col_order]
ilo_2020$IND<-sub(".", "", ilo_2020$IND)

setDT(ilo_2020)[,tot_emp:=nemp[IND == "TOTAL"],by=list(country, IND, year)]
ilo_2020$tot_emp[is.na(ilo_2020$tot_emp)] <- 0
setDT(ilo_2020)[,tot_emp:=max(tot_emp),by=list(country, year)]
ilo_2020 <- subset(ilo_2020,IND!="TOTAL")
setDT(ilo_2020)[,share_emp:=nemp/tot_emp,by=list(country, IND, year)]

#Preparing labels
ilo_lab <-read.csv2("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/ilostat_indlab.csv",header=T,sep=",",stringsAsFactors = FALSE,dec=".")
ilo_lab <-subset(ilo_lab, select = c("classif1", "classif1.label"))
split_lab <- data.frame(do.call('rbind', strsplit(as.character(ilo_lab$classif1),'_',fixed=TRUE)))
ilo_lab$IND<-paste0("",split_lab$X3)
split_lab2 <- data.frame(do.call('rbind', strsplit(as.character(ilo_lab$classif1.label),'Economic activity (ISIC-Rev.4),',fixed=TRUE)))
split_lab2 <- data.frame(do.call('rbind', strsplit(as.character(split_lab2$X2),'-',fixed=TRUE)))
names(split_lab2)[names(split_lab2) == "X2"] <- "IND_lab"
ilo_lab$lab<-paste0("",split_lab2$IND_lab)
ilo_lab <-subset(ilo_lab, select = c("IND", "lab"))
ilo_lab <- subset(ilo_lab,IND!="TOTAL")

#Merging main data with labels
ilo_2020_final <- merge(ilo_2020, ilo_lab)

#TEST:Pharma only 

ilo_2020_pharma <- ilo_2020_final %>%
  filter(IND == "C21") %>%
  filter(year >= 2008)

ggplot(ilo_2020_pharma) + 
  geom_line(aes(x = year, y = share_emp, group = country, colour = country)) + 
  geom_text(data = subset(ilo_2020_pharma, year == "2019"), aes(label = country, colour = country, x = year, y = share_emp), hjust = -.1) +
  scale_colour_discrete(guide = 'none') 


########################
#d. Labor productivity #
########################

# NEW OECD STAN DATA #
stan_2020 <-read.csv2("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/STAN_2020.csv",header=T,sep=",",stringsAsFactors = FALSE,dec=".")
stan_2020 <-subset(stan_2020, select = c("LOCATION", "Country", "Variable", "IND", "Industry", "Time", "Unit", "PowerCode", "Reference.Period", "Value", "Flags"))
stan_2020 <-dcast(stan_2020, LOCATION + Country + IND+ Time+ Unit+ PowerCode+ Reference.Period+ Flags~ Variable, value.var="Value")
names(stan_2020)
stan_2020 <-subset(stan_2020, select = c("LOCATION", "Country", "IND", "Time", "PowerCode", "Number of employees", "Number of persons engaged (total employment)", "Value added, current prices"))
names(stan_2020)[names(stan_2020) == "LOCATION"] <- "cou.code"
names(stan_2020)[names(stan_2020) == "Country"] <- "cou.name"
names(stan_2020)[names(stan_2020) == "Time"] <- "year"
names(stan_2020)[names(stan_2020) == "Number of employees"] <- "nemp"
names(stan_2020)[names(stan_2020) == "Number of persons engaged (total employment)"] <- "temp"
names(stan_2020)[names(stan_2020) == "Value added, current prices"] <- "valad"

stan_2020$valad[is.na(stan_2020$valad)] <- 0
setDT(stan_2020)[,valad:=max(valad),by=list(cou.code, IND, year)]
stan_2020 <- subset(stan_2020,PowerCode!="Millions")
stan_2020$valad <- replace(stan_2020$valad, stan_2020$valad == 0, NA)

stan_2020 <-subset(stan_2020, select = c("cou.code", "cou.name", "IND", "year", "nemp", "temp", "valad"))

stan_2020 <- filter(stan_2020, IND %in% c("D05T09", "D10T12", "D13T15", "D16T18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T33", "D35" , "D36T39" , "D41T43"))
stan_2020 <- mutate(stan_2020, IND = case_when(IND %in% c("D19", "D20") ~ "C19-C20",
                                               IND %in% c("D22", "D23") ~ "C22-C23",
                                               IND %in% c("D24", "D25") ~ "C24-C25",
                                               IND %in% c("D29", "D30") ~ "C29-C30",
                                               IND == "D05T09" ~ "B",
                                               IND == "D10T12" ~ "C10-C12",
                                               IND == "D13T15" ~ "C13-C15",
                                               IND == "D16T18" ~  "C16-18",
                                               IND == "D19" ~  "C19",
                                               IND == "D20" ~  "C20",
                                               IND == "D21" ~  "C21",
                                               IND == "D26" ~ "C26",
                                               IND == "D27" ~ "C27",
                                               IND == "D28" ~  "C28",
                                               IND == "D31T33" ~ "C31-C33",
                                               IND == "D35" ~  "D35",
                                               IND == "D36T39" ~  "E36-39",
                                               IND == "D41T43" ~  "F41-43",))

#Exchange Rates#
dollar_exrates <-rio::import("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/exch_rate.csv")
names(dollar_exrates)

dollar_exrates <-subset(dollar_exrates, select = c("LOCATION,\"\"INDICATOR\"" ,"\"TIME\"", "\"Value\"")) 
names(dollar_exrates)[names(dollar_exrates) == "LOCATION,\"\"INDICATOR\""] <- "cou.code"
names(dollar_exrates)[names(dollar_exrates) == "\"TIME\""] <- "year"
names(dollar_exrates)[names(dollar_exrates) == "\"Value\""] <- "fxrate"

dollar_exrates$cou.code = gsub("\"","",dollar_exrates$cou.code)
dollar_exrates$year = gsub("\"","",dollar_exrates$year)

split_excrate <- data.frame(do.call('rbind', strsplit(as.character(dollar_exrates$cou.code),',',fixed=TRUE)))
dollar_exrates$cou.code<-paste0("",split_excrate$X1)

#Merging STAN with Exchange Rates#
stan_final_2020<-merge(stan_2020,dollar_exrates,by=c("year","cou.code"))

#Lab productivity vars#
#setDT(stan_final_2020)[,nemp:=nemp*1000,by=list(cou.code, IND, year)]
#setDT(stan_final_2020)[,valad:=valad*1000000,by=list(cou.code, IND, year)]
setDT(stan_final_2020)[,valad_dol:=valad/fxrate,by=list(cou.code, IND, year)]
setDT(stan_final_2020)[,lab_prod:=valad_dol/nemp,by=list(cou.code, IND, year)]

#TEST:Pharma only 
stan_2020_pharma <- stan_final_2020 %>%
  filter(IND == "C21") %>%
  filter(year >= 2008)

ggplot(stan_2020_pharma) + 
  geom_line(aes(x = year, y = lab_prod, group = cou.code, colour = cou.code)) + 
  geom_text(data = subset(stan_2020_pharma, year == "2018"), aes(label = cou.code, colour = cou.code, x = year, y = lab_prod), hjust = -.1) +
  scale_colour_discrete(guide = 'none') 

############################
#d. Export / trade figures #
############################

oecd_trade_2020 <-rio::import("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/oecd_trade_new_2020.csv")
names(oecd_trade_2020)
oecd_trade_2020 <-subset(oecd_trade_2020, select = c("COU,\"\"Reporting country\"", "\"PAR\"", "\"Partner country\"", "\"IND\"", 
                                                     "\"Industry\"", "\"TIME\"", "\"Unit Code\"", "\"Value\""))
names(oecd_trade_2020)[names(oecd_trade_2020) == "COU,\"\"Reporting country\""] <- "rep.cou"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"PAR\""] <- "par.code"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"Partner country\""] <- "par.name"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"IND\""] <- "ind.code"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"Industry\""] <- "ind.name"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"TIME\""] <- "year"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"Unit Code\""] <- "unit"
names(oecd_trade_2020)[names(oecd_trade_2020) == "\"Value\""] <- "value"

oecd_trade_2020$rep.cou = gsub("\"","",oecd_trade_2020$rep.cou)
oecd_trade_2020$par.code = gsub("\"","",oecd_trade_2020$par.code)
oecd_trade_2020$par.name = gsub("\"","",oecd_trade_2020$par.name)
oecd_trade_2020$ind.code = gsub("\"","",oecd_trade_2020$ind.code)
oecd_trade_2020$ind.name = gsub("\"","",oecd_trade_2020$ind.name)
oecd_trade_2020$year = gsub("\"","",oecd_trade_2020$year)
oecd_trade_2020$unit = gsub("\"","",oecd_trade_2020$unit)
oecd_trade_2020$value = gsub("\"","",oecd_trade_2020$value)

split_rep <- data.frame(do.call('rbind', strsplit(as.character(oecd_trade_2020$rep.cou),',',fixed=TRUE)))

oecd_trade_2020$rep.code<-paste0("",split_rep$X1)
oecd_trade_2020$rep.name<-paste0("",split_rep$X2)

oecd_trade_2020 <-subset(oecd_trade_2020, select = c("rep.code", "rep.name", "par.code", "par.name", "ind.code", "ind.name", "year", "unit","value" )) 
oecd_ind_label <-subset(oecd_trade_2020, select = c("ind.code", "ind.name")) 
oecd_trade_2020 <-subset(oecd_trade_2020, select = c("rep.code", "par.code", "ind.code", "year", "value" )) 


#Swiss export data
setwd("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/")
files <- list.files(pattern="*.TXT")
swiss_trade_2020 <- do.call(rbind.fill, lapply(files[1:length(files)], function(x) read.csv2(x,sep="|", header = F, skipNul = TRUE, fileEncoding="latin1")))
colnames(swiss_trade_2020) <- c("rep.code", "trade.flow", "par.code", "kind", "ind.code", "value.share", "year", "exp.export")
swiss_trade_2020 <- filter(swiss_trade_2020, trade.flow == "EXPO" & kind == "TOTAL" & value.share == "VALUE")
swiss_trade_2020 <-subset(swiss_trade_2020, select = c("rep.code", "par.code", "ind.code", "year", "exp.export" )) 
names(swiss_trade_2020)[names(swiss_trade_2020) == "exp.export"] <- "value"

#Append main data-set with Swiss data
oecd_trade_final_2020 <- rbind(oecd_trade_2020, swiss_trade_2020) 

setDT(oecd_trade_final_2020)[,tot_exp:=value[ind.name == "TOTAL"],by=list(rep.code, par.code, year)]

oecd_trade_final_2020 <- filter(oecd_trade_final_2020, ind.code %in% c("D05T09", "D10T12", "D13T15", "D16T18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T33", "D35" , "D36T39" , "D41T43"))
names(oecd_trade_final_2020)[names(oecd_trade_final_2020) == "Number of persons engaged (total employment)"] <- "nemp"

oecd_trade_final_2020 <- mutate(oecd_trade_final_2020, ind.code = case_when(ind.code %in% c("D19", "D20") ~ "C19-C20",
                                                                            ind.code %in% c("D22", "D23") ~ "C22-C23",
                                                                            ind.code %in% c("D24", "D25") ~ "C24-C25",
                                                                            ind.code %in% c("D29", "D30") ~ "C29-C30",
                                                                            ind.code == "D05T09" ~ "B",
                                                                            ind.code == "D10T12" ~ "C10-C12",
                                                                            ind.code == "D13T15" ~ "C13-C15",
                                                                            ind.code == "D16T18" ~  "C16-18",
                                                                            ind.code == "D19" ~  "C19",
                                                                            ind.code == "D20" ~  "C20",
                                                                            ind.code == "D21" ~  "C21",
                                                                            ind.code == "D26" ~ "C26",
                                                                            ind.code == "D27" ~ "C27",
                                                                            ind.code == "D28" ~  "C28",
                                                                            ind.code == "D31T33" ~ "C31-C33",
                                                                            ind.code == "D35" ~  "D35",
                                                                            ind.code == "D36T39" ~  "E36-39",
                                                                            ind.code == "D41T43" ~  "F41-43",))
#TEST:Pharma, CHE/USA only 

trade_pharma_ch <- oecd_trade_final_2020 %>%
  filter(rep.code == "CHE") %>%
  filter(ind.code == "C21") %>%
  filter(par.code == "USA")


ggplot(trade_pharma_ch) + 
  geom_line(aes(x = year, y = value, group = par.code, colour = par.code)) + 
  geom_text(data = subset(trade_pharma_ch, year == "2019"), aes(label = par.code, colour = par.code, x = year, y = value), hjust = -.1) +
  scale_colour_discrete(guide = 'none') 

################################################################################
################################################################################

