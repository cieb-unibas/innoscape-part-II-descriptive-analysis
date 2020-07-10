print("Create economic indicators for intro to part II descriptive analysis")
# Last modified 29.6.2020 / DF

require(data.table)
require(plyr)
library(dplyr)
require(ggplot2)
library(countrycode)
library(viridis)
library(reshape2)
rm(list = ls())
# test

				
########################
# NEW OECD STAN DATA   #
########################

stan_2020 <-read.csv2("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/STAN_2020.csv",header=T,sep=",",stringsAsFactors = FALSE,dec=".")
stan_2020 <-subset(stan_2020, select = c("LOCATION", "Country", "Variable", "IND", "Industry", "Time", "Unit", "PowerCode", "Reference.Period", "Value", "Flags"))
stan_2020 <-dcast(stan_2020, LOCATION + Country + IND+ Time+ Unit+ PowerCode+ Reference.Period+ Flags~ Variable, value.var="Value")

########################
#b. Employment figures #
########################

## CR: I would use here data from the OECD Stan database. You can find a file from last year on our scicore group folder: /scicore/home/weder/GROUP/Innovation/01_patent_data/raw data/econ_indicators/STAN.txt

########################
#c. Added value in GDP #
########################

## CR: I would use here data from the OECD Stan database. You can find a file from last year on our scicore group folder: /scicore/home/weder/GROUP/Innovation/01_patent_data/raw data/econ_indicators/STAN.txt


########################
#d. Labor productivity #
########################

## CR: I would use here data from the OECD Stan database. You can find a file from last year on our scicore group folder: /scicore/home/weder/GROUP/Innovation/01_patent_data/raw data/econ_indicators/STAN.txt

############################
#d. Export / trade figures #
############################

## CR: Here are some code snippets used for a SECO project done last year, so the used OECD trade data is quite new. However, we could also download the latest one. 
setwd("/scicore/home/weder/GROUP/Innovation/01_patent_data/raw data/econ_indicators/")
files <- list.files(pattern="*.TXT")
oecd.trade <- do.call(rbind.fill, lapply(files[1:length(files)], function(x) read.csv2(x,sep="|", header = F, skipNul = TRUE, fileEncoding="latin1")))
colnames(oecd.trade) <- c("exp.iso", "trade.flow", "imp.iso", "kind", "exp.industry", "value.share", "jahr", "exp.export")
oecd.trade <- filter(oecd.trade, trade.flow == "EXPO" & kind == "TOTAL" & value.share == "VALUE")

oecd.trade <- filter(oecd.trade, exp.industry %in% c("D01T03", "D05T08", "D10T12", "D13T15", "D16", "D17", "D18", "D19", "D20", "D21", "D22", "D23", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T32", "D35"))
oecd.trade <- mutate(oecd.trade, exp.industry = case_when(exp.industry %in% c("D16", "D17") ~ "C16-C17",
																													exp.industry %in% c("D19", "D20") ~ "C19-C20",
																													exp.industry == "D01T03" ~ "A01-A03",
																													exp.industry == "D05T08" ~ "B",
																													exp.industry == "D10T12" ~ "C10-C12",
																													exp.industry == "D13T15" ~ "C13-C15",
																													exp.industry == "D18" ~  "C18",
																													exp.industry == "D21" ~  "C21",
																													exp.industry == "D22" ~ "C22",
																													exp.industry == "D23" ~  "C23",
																													exp.industry == "D24" ~ "C24",
																													exp.industry == "D25" ~  "C25",
																													exp.industry == "D26" ~ "C26",
																													exp.industry == "D27" ~ "C27",
																													exp.industry == "D28" ~  "C28",
																													exp.industry == "D29" ~ "C29",
																													exp.industry == "D30" ~ "C30",
																													exp.industry == "D31T32" ~ "C31_C32",
																													exp.industry == "D35" ~  "D35"))






