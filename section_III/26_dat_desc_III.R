cat("26: Create data for Section III.: 
    Determine innovator origin and gender in Pharma industries using inventor names
    (MN 23.06.2020)")

#######################################
## Load packages and set directories ##
#######################################

# packages for data processing: ------------------------------------------------
library("tidyverse")
library("data.table")
library("countrycode")
library("stringi")
#library("OECD")

# directories ------------------------------------------------------------------
mainDir1 <- "/scicore/home/weder/GROUP/Innovation/01_patent_data"
if(substr(getwd(), nchar(getwd())-38, nchar(getwd())) != "/innoscape-part-II-descriptive-analysis"){
  print("make sure your working directory is the GitHub repository. Please specify with setwd().")}else{
    print("Directories are set and packages are loaded")}


#######################################
############ Load data sets ###########
#######################################

## Load data on gender shares of university graduates --------------------------
grad_dat <- read.csv(paste0(getwd(), "/section_III/oecd_graduates.csv"))
names(grad_dat)[1] <- "COUNTRY"
total_year <- grad_dat %>% filter(SEX == "T") %>% 
  select(COUNTRY, FIELD, Field, ISC11_LEVEL, YEAR, Value) %>%
  rename(total_graduates = Value)
grad_dat <- grad_dat %>% filter(SEX == "F") %>% 
  select(COUNTRY, SEX, FIELD, ISC11_LEVEL, YEAR, Value)
grad_dat <- merge(grad_dat, total_year, 
                  by = c("COUNTRY", "FIELD", "ISC11_LEVEL", "YEAR"),
                  all.x = TRUE)
total_year <- NULL
grad_dat <- mutate(grad_dat, female_share_graduates = Value / total_graduates)
grad_dat <- grad_dat[is.nan(grad_dat$female_share_graduates) == FALSE &
                       is.na(grad_dat$female_share_graduates) == FALSE, ]
grad_dat$COUNTRY <- countrycode(grad_dat$COUNTRY, "iso3c", "iso2c")
grad_dat <- rename(grad_dat, Ctry_code = COUNTRY, p_year = YEAR, 
                   N_female_graduates = Value)
grad_dat <- select(grad_dat, - SEX)
print("Data on university graduates loaded")

## Load sociodemographic characteristics of patents' inventors -----------------
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg_gender.rds")) 
print("Properties of patent inventors loaded")
print("All necessary data is loaded")

#########################################################################
######## create gender data on regions & countries for analysis #########
#########################################################################

## clean the data --------------------------------------------------------------
inv_reg$gender <- as.numeric(inv_reg$gender)
inv_reg$p_year <- as.numeric(inv_reg$p_year)
dat <- inv_reg # to keep original inv_reg dataframe
dat <- dat[is.na(dat$gender) == FALSE, ] # drop NA's

## gender shares among patent inventors --------------------------------
# overall:
table(dat$gender)/nrow(dat) 
# non-predicted USPTO patent inventors only:
table(dat[is.na(dat$Up_reg_code)==TRUE, "gender"])/nrow(dat[is.na(dat$Up_reg_code),]) 
# => slightly lower gender share among USPTO inventors

## calculate shares of female inventors per region and p_year
female_inv_shares_reg <- dat %>% 
  group_by(Up_reg_code, Ctry_code, p_year) %>%
  summarise(total_inventors_reg = n(),
            female_inventors_reg = sum(gender == 0, na.rm = TRUE),
            female_inventor_share_reg = female_inventors_reg / total_inventors_reg)%>%
  filter(total_inventors_reg > 30)
female_inv_shares_reg <- merge(female_inv_shares_reg, 
                               inv_reg[!duplicated(inv_reg$Up_reg_code), c("Up_reg_code", "Up_reg_label")],
                               by = "Up_reg_code", all.x = TRUE)
# => relatively many observations have NA as regional code
# => use this for a table

## calculate shares of female inventors per country and p_year:
female_inv_shares_ctry <- dat %>% 
  group_by(Ctry_code, p_year) %>%
  summarise(total_inventors_ctry = n(),
            female_inventors_ctry = sum(gender == 0, na.rm = TRUE),
            female_inventor_share_ctry = female_inventors_ctry / total_inventors_ctry)%>%
  filter(total_inventors_ctry > 30)

## merge female graduate shares and female inventor shares together ------------
female_inv_shares_ctry <- merge(female_inv_shares_ctry, grad_dat, 
                    by = c("Ctry_code", "p_year"), all = TRUE)
female_inv_shares_ctry$Ctry_label <- countrycode(female_inv_shares_ctry$Ctry_code,
                                                 "iso2c", "country.name")

# overall female shares among graduates (2010-2017)
tmp <- female_inv_shares_ctry %>%
  group_by(Ctry_code, FIELD, ISC11_LEVEL) %>%
  summarise(N_female_graduates_overall = sum(N_female_graduates, na.rm = TRUE),
            total_graduates_overall = sum(total_graduates, na.rm = TRUE),
            female_share_graduates_overall = N_female_graduates_overall / total_graduates_overall,
            female_inventor_share_ctry_overall = sum(female_inventors_ctry, na.rm = TRUE) /
              sum(total_inventors_ctry, na.rm = TRUE))
female_inv_shares_ctry <- merge(female_inv_shares_ctry, tmp, 
                                by = c("Ctry_code", "FIELD", "ISC11_LEVEL"),
                                all.x =TRUE)
tmp <- NULL
female_inv_shares_ctry <- mutate(female_inv_shares_ctry,
       inventor_graduate_ratio = female_inventor_share_ctry / female_share_graduates_overall)
gender_dat <- list(#female_inv_shares_reg,
                   female_inv_shares_ctry)
names(gender_dat) <- c(#"region",
                       "country")

print("Data ready for analysis")

## save the dataset for report -------------------------------------------------
# dat <- list(dat)
#saveRDS(dat, paste0(getwd(),"/report/iv_gender_nationalities.rds"))
#print("Data saved as 'inv_gender_nationalities.rds'")

#######################################
######## Premliminary Analysis ########
#######################################

## Plot female inventor shares per country over time:
selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK", "FR")
plot_data <- gender_dat[["country"]] %>%
  filter(Ctry_code %in% selected_countries,
         p_year >= 1990, p_year < 2017,
         total_inventors_ctry > 30)
plot_data$isoyear <- paste0(plot_data$Ctry_code, plot_data$p_year)
plot_data <- plot_data[!duplicated(plot_data$isoyear), ]
ggplot(plot_data, aes(x = p_year, y = female_inventor_share_ctry, color = Ctry_code))+
  geom_line()+geom_point(aes(shape = Ctry_code))+ylim(c(0, 0.4))+
  labs(x="year", y="Share of females among patent inventors")

## overall share of female biology graduates between 2010-2017
plot_data <- filter(gender_dat[["country"]] , FIELD == "F05", ISC11_LEVEL == "L7")
plot_data <- plot_data[!duplicated(plot_data$Ctry_code), ]
plot_data %>% select(Ctry_code, female_share_graduates_overall) %>% 
  arrange(desc(female_share_graduates_overall))

## plot shares of female master graduates over time
selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK")
plot_data <- gender_dat[["country"]] %>%
  filter(Ctry_code %in% selected_countries,
         FIELD == "F05",
         ISC11_LEVEL == "L7",
         p_year >= 1990,
         total_graduates > 50) %>%
  distinct(Ctry_code, .keep_all = TRUE)
ggplot(plot_data, aes(y = female_share_graduates_overall, x= Ctry_code, fill = Ctry_code))+
  geom_col()+ylim(0,1)+
  ggtitle(paste0("Female share of graduates in ", plot_data$Field[1], 
                 " (ISC-Level ", plot_data$ISC11_LEVEL[1],")"))

## correlation plot graduates vs. inventors
selected_countries <- c("CH", "US", "UK", "DE", "AT", "DK", "FR", "IT", "ES", "PT", "CA",
                        "KR", "FI", "SE", "NO", "NL", "BE", "PL", "CZ", "HU", "SK", "IE",
                        "IL")
plot_data <- gender_dat[["country"]] %>%
  filter(Ctry_code %in% selected_countries,
         FIELD == "F05", ISC11_LEVEL == "L7",
         p_year >= 2010,
         total_graduates > 50,
         total_inventors_ctry > 30) %>%
  distinct(Ctry_code, .keep_all = TRUE)
ggplot(plot_data, aes(x = female_share_graduates_overall, 
                      y = female_inventor_share_ctry_overall))+
  geom_point(aes(size = total_graduates), alpha = 0.5, color = "steelblue")+
  geom_text(aes(label = Ctry_code), size = 3, nudge_y = 0.005, nudge_x = -0.003)+
  xlim(0.4, 0.75)+ylim(0, 0.4)+theme(legend.position = "none")+
  geom_vline(xintercept = mean(plot_data$female_share_graduates_overall), 
             linetype = "dotted")+
  geom_hline(yintercept = mean(plot_data$female_inventor_share_ctry_overall), 
             linetype = "dotted")
# => could also add here a color that indicates how familiy friendly an economy is.
# => Japan is an outlier

#########################################################
## Assign ethnic groups and nationalities to inventors ##
#########################################################






