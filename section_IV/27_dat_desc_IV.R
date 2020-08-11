#######################################################################
print("Determine whether patent is from academia or private firm CR 10.8.2020")
#######################################################################
require(data.table)
require(ggplot2)
require(dplyr)
require(geosphere)
require(fuzzyjoin)
require(plyr)
require(stringr)
require(stringdist)
require(RecordLinkage)
require(fuzzyjoin)
require(tm)

mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")
tech_field_start <- 16


########################################################################
## Some analysis on whether patent is from academia or private firm#####
########################################################################
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_", tech_field_start, ".rds"))
firm_reg <- filter(firm_reg, granted == "yes") ## consider only granted patents
firm_reg <- setDT(firm_reg)[, c("num", "num_uni") := list(.N, sum(uni)), .(p_key)]
firm_reg <- mutate(firm_reg, uni_firm = ifelse(num_uni == num, "uni", ifelse(num_uni > 0, "uni_firm", "firm")))

## Number of patents (1) uni (2) uni and firm (3) firm
uni_firm_ctry <- aggregate(num ~ uni_firm + country + p_year, FUN = sum, data = mutate(firm_reg, num = 1))
uni_firm_ctry <- setDT(uni_firm_ctry)[, rel := num / sum(num), .(country, p_year)]

ggplot(filter(uni_firm_ctry, country %in% c("CH", "DE", "US", "GB") & p_year < 2016), aes(x = p_year, y = rel, color = country, shape = uni_firm)) +
  geom_line() +
  geom_point()
#-> US pharma firms do more often cooperate with universities, whereas Swiss firms do not

## Consider only subset of patents with uni and firm
uni_firm <- filter(firm_reg, uni_firm == "uni_firm")
uni_firm <- setDT(uni_firm)[, c("num", "num_uni") := list(.N, sum(uni)), .(p_key, country)]
uni_firm <- mutate(uni_firm, uni_firm = case_when(num == num_uni ~ "uni", num_uni == 0 ~ "firm", T ~ "uni_firm")) #which country has (a) only uni, (b) only firm or (c) both as inventors of a patent
uni_firm <- mutate(uni_firm, num = 1)
uni_firm_sub <- aggregate(num ~ uni_firm + country + p_year, FUN = sum, data = uni_firm)
uni_firm_sub <- setDT(uni_firm_sub)[, rel := num / sum(num), .(country, p_year)]

ggplot(filter(uni_firm_sub, country %in% c("CH", "US")  & p_year < 2016 & uni_firm %in% c("firm", "uni_firm")), aes(x = p_year, y = rel, color = country, shape = uni_firm)) +
  geom_line() +
  geom_point()
#-> Swiss pharma firms often cooperate with foreign universities, whereas US firms often cooperate with US universities.