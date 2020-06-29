print("Create economic indicators for intro to part II descriptive analysis")
# Last modified 24.6.2020 / DF

require(data.table)
require(plyr)
library(dplyr)
require(ggplot2)
library(countrycode)
library(viridis)
library(reshape2)
rm(list = ls())
# test
###################################################################
#a. Data for long term plot of emplyoment / section Brief history #
###################################################################
## CR: Code snippet from a project on de-industrialization. The data is used to create plots at the aggregate sectoral level. So other subsets should be used-> at the industry rather the sectoral level.

##Employment 1888-1990
emp_old <-read.csv2("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/emp_lange_zeitreihe_long.csv",header=T,sep=";",stringsAsFactors = FALSE,dec=".")
emp_old <- mutate(emp_old,sek=paste0(Branche1,Branche2,Branche3)) # Creates sek variable (paste) from combining branche1, branche2, branche3 
emp_old <- mutate(emp_old,sek=gsub("-", "", emp_old$sek)) # removes - simbol (substitutes it with empty space) for sek var
emp_old <- dplyr::select(emp_old,-Branche1,-Branche2,-Branche3) # drops branche1-branche3 vars
emp_old <- melt(emp_old,id=c("sek","Sektor", "code")) # reshaping data from wide to long format
emp_old <- mutate(emp_old,jahr=substr(variable,2,5))
setDT(emp_old)[,rel.emp:=value/value[sek=="total"],list(jahr)] # share in total emp for each sector by given year
emp_old <- subset(emp_old,sek!="Total") # subset of data where sek does not equal total (droping values for total)
emp_old <- mutate(emp_old, jahr = as.numeric(as.character(jahr)))


## Plot of change of sector structure
pdf("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I/sektor_struk_1888_1990.pdf",width=20,height=15)
ggplot(data = filter(emp_old, !(sek %in% c("total", "Sonstige Industrie")) & Sektor %in% c("2. Sektor")), aes( y = rel.emp, x = reorder(jahr, -jahr), fill = sek)) + 
	geom_bar(stat = "identity", width = 0.8, position = "fill", size = 2) +
	coord_flip(expand = F) +
	scale_fill_viridis(option="D", begin = 0, end = 1, discrete=T, alpha = 0.9) + 
  scale_y_continuous(labels = scales::percent) +
	xlab("Jahr") +
	ylab("Prozent des 2. Sektors") +
	theme(axis.text.y = element_text(size = 35),axis.text.x = element_text(size = 30),axis.title.x=element_text(size=30), axis.title.y=element_text(size=35), strip.background = element_blank(), strip.text.x = element_blank(), panel.spacing = unit(2, "lines"), legend.text = element_text(size = 30), legend.title = element_blank(), aspect.ratio = 0.8/1, plot.margin=grid::unit(c(0,0,0,0), "mm"))
dev.off()				



##Employment > 1990 -> consider to download the latest data from BFS webpage?
setwd("/scicore/home/weder/fildra00/Innovation/innoscape-part-II-descriptive-analysis/section_I")
bfs_emp<-read.csv2("emp_total_vza.csv",header=T,sep=";",stringsAsFactors = FALSE,dec=".",fileEncoding="latin1")
x<-regmatches(bfs_emp$Wirtschaftsabteilung, gregexpr("[[:digit:]]+",bfs_emp$Wirtschaftsabteilung)) # create a list vector containing codes characters		 
t<-data.frame(start=sapply(x,function(m)m[1]),stop=sapply(x,function(m)m[2])) # separate two parts of the character in list
t$start<-as.character(t$start) # define start as string in t list
t$stop<-as.character(t$stop) # define stop as string in t list
t$start<-ifelse(nchar(t$start)==1,paste0(0,t$start),t$start)										 
t$stop<-ifelse(nchar(t$stop)==1,paste0(0,t$stop),t$stop)	
t$start<-ifelse(is.na(t$start),"",t$start)
t$stop<-ifelse(is.na(t$stop),"",t$stop)

bfs_emp$IND<-ifelse(nchar(t$stop)!=0,paste0("D",t$start,"T",t$stop),paste0("D",t$start))
bfs_emp$Q<-substr(bfs_emp$Quartal,5,6)
bfs_emp$jahr<-substr(bfs_emp$Quartal,1,4)
bfs_emp<-dplyr::rename(bfs_emp,emp=Beschäftigungsgrad...Total.Geschlecht...Total,vza=Vollzeitäquivalente.Geschlecht...Total)
bfs_emp<-subset(bfs_emp,jahr>1991&jahr<2017)
bfs_emp$emp<-as.numeric(bfs_emp$emp)/1000
bfs_emp$vza<-as.numeric(bfs_emp$vza)/1000


bfs_emp$emp<-ifelse(bfs_emp$Q=="",bfs_emp$emp*4,bfs_emp$emp)
bfs_emp$vza<-ifelse(bfs_emp$Q=="",bfs_emp$vza*4,bfs_emp$vza)
setDT(bfs_emp)[,emp:=sum(emp)/4,by=list(IND,jahr)]
setDT(bfs_emp)[,vza:=sum(vza)/4,by=list(IND,jahr)]
bfs_emp<-subset(bfs_emp,Q=="Q1"|Q=="")

bfs_emp_manu <- setDT(bfs_emp)[,rel.emp:=vza/vza[IND=="D05T96"],list(jahr)]
bfs_emp_manu <- filter(bfs_emp_manu,IND%in%c("D10T12","D13T15","D16T18","D31T33","D22T23","D24T25", "D27", "D28" ,"D29T30","D21","D26","D19T20", "D35", "D36T39", "D41T43"))
bfs_emp_manu <- dplyr::select(bfs_emp_manu,Wirtschaftsabteilung,emp,jahr,rel.emp,IND)
bfs_emp_manu <- filter(bfs_emp_manu,jahr%in%c("1992","1995","2000","2005","2010","2015"))


bfs_emp_manu <- dplyr::select(bfs_emp_manu,Wirtschaftsabteilung,emp,jahr, IND,code)
bfs_emp_manu <- filter(bfs_emp_manu,jahr%in%c("1995","2000","2005","2010","2016"))
bfs_emp_ind <- aggregate(emp ~ jahr + code, FUN = sum, data = bfs_emp_manu, na.rm = T, na.action = NULL)

sect.dat <- emp_old[!duplicated(emp_old[, c("code", "sek", "Sektor")]), ]
sect.dat <- dplyr::select(sect.dat, sek, Sektor, code)
bfs_emp_ind <- merge(bfs_emp_ind, sect.dat, by = c("code"))
bfs_emp_ind <- dplyr::rename(bfs_emp_ind, rel.emp = emp)
bfs_emp_ind <- mutate(bfs_emp_ind, jahr = as.numeric(as.character(jahr)))

## Plot of change of sector structure
pdf("C:/Users/christian rutzer/Dropbox/De-Industrialisierung/Text_Rolf/plot/sektor_struk_1992_2016.pdf",width=20,height=8)
ggplot(data = filter(bfs_emp_ind, !(sek %in% c("Sonstige Industrie")) & Sektor == "2. Sektor"), aes( y = rel.emp, x = reorder(jahr, -jahr), fill = sek)) + 
	geom_bar(stat = "identity", width = 0.8, position = "fill") +
	coord_flip(expand = F) +
	scale_fill_viridis(option="D", begin = 0, end = 1, discrete=T, alpha = 0.9) + 
	scale_y_continuous(labels = scales::percent) +
	xlab("Jahr") +
	ylab("Prozent des 2. Sektors") +
	theme(axis.text.y = element_text(size = 35),axis.text.x = element_text(size = 30),axis.title.x=element_text(size=30), axis.title.y=element_text(size=35), strip.background = element_blank(), strip.text.x = element_blank(), panel.spacing = unit(2, "lines"), legend.text = element_text(size = 30), legend.title = element_blank(), aspect.ratio = 0.4/1,plot.margin=grid::unit(c(0,0,0,0), "mm"))
dev.off()			


bfs_emp_manu <- mutate(bfs_emp_manu, jahr = as.numeric(as.character(jahr)), Wirtschaftsabteilung = gsub('[[:digit:]]+', '', Wirtschaftsabteilung)) %>% mutate(Wirtschaftsabteilung = gsub('-', '', Wirtschaftsabteilung))
pdf("C:/Users/christian rutzer/Dropbox/De-Industrialisierung/Text_Rolf/plot/sektor_struk_serv_1992_2016.pdf",width=20,height=8)
ggplot(data = filter(bfs_emp_manu, IND %in% c("D45T47", "D49T53", "D55T56", "D58T60", "D62T63", "D64T66", "D68", "D69T75", "D77T82", "D84", "D85", "D86T88", "D90T93")), aes( y = rel.emp, x = reorder(jahr, -jahr), fill = Wirtschaftsabteilung)) + 
	geom_bar(stat = "identity", width = 0.8, position = "fill") +
	coord_flip(expand = F) +
	scale_fill_viridis(option="D", begin = 0, end = 1, discrete=T, alpha = 0.9) + 
	scale_y_continuous(labels = scales::percent) +
	xlab("Jahr") +
	ylab("Prozent des 3. Sektors") +
	theme(axis.text.y = element_text(size = 35),axis.text.x = element_text(size = 30),axis.title.x=element_text(size=30), axis.title.y=element_text(size=35), strip.background = element_blank(), strip.text.x = element_blank(), panel.spacing = unit(2, "lines"), legend.text = element_text(size = 30), legend.title = element_blank(), aspect.ratio = 0.7/1,plot.margin=grid::unit(c(0,0,0,0), "mm"))
dev.off()			

pdf("C:/Arbeit/Forschungsstelle/Projekte/Projekt HKBB/Text/plot/sektor_bws_schweiz.pdf",width=16,height=10)
ggplot() +
	# geom_area(position="stack",alpha=0.5) +
	geom_area(data=bfs_emp_manu,  aes(x=jahr,y=rel.emp,fill=factor(IND),group=IND) ,alpha=0.5,position="stack") +	
	geom_line(data=bfs_emp_manu,  aes(x=jahr,y=rel.emp,fill=factor(IND),group=IND), position="stack") +
	theme_minimal()  +
	geom_line(position="stack")+
	guides(fill= guide_legend(reverse=T)) +
	scale_fill_viridis(option="inferno", begin = 0.25, end = 0.85, discrete=TRUE) +
	xlab("Jahr") +
	ylab("Anteil nominale Wertschöpfung") +
	theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold"),legend.title=element_blank(),legend.text=element_text(size=16)) +
	theme(legend.title=element_blank(),legend.position = "bottom") 
	# scale_x_continuous(breaks = seq(1890,2020,20)
dev.off()	


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






