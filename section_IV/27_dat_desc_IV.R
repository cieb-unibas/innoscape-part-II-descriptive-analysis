#######################################################################
print("27: Create analysis for cluster basel: where are inventions registered and where do they actually happen/ CR 30.7.2020")
#######################################################################
require(data.table)
require(ggplot2)
require(dplyr)
require(geosphere)
require(fuzzyjoin)
require(plyr)
require(stringr)
require(stringdist)

mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")
tech_field_start <- 16

## Load data created in 20 and 21
inv_reg_us <- readRDS(paste0(mainDir1, "/created data/inv_reg/inv_reg_us_", tech_field_start, ".rds")) %>% dplyr::select(p_key, name, inventor_id, lat, lng, ctry_code, Up_reg_label, patent_id)
inv_reg_us <- setDT(inv_reg_us)[, num_inv := .N, .(p_key)]
inv_reg_us <- distinct(inv_reg_us, p_key, name, .keep_all =  T)

firm_reg_us <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_us_", tech_field_start, ".rds")) %>% dplyr::select(p_key, organization, lat, lng, country, Up_reg_label)
firm_reg_us <- distinct(firm_reg_us, p_key, organization, lat, lng, .keep_all = T)

## Use country of EP patents if there are equivalents between US and EP patents
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_", tech_field_start, ".rds")) %>% dplyr::select(p_key, organization, lat, lng, country, pub_nbr)
firm_reg <- filter(firm_reg, substr(pub_nbr, 1, 2) == "EP")
firm_reg_both <- inner_join(firm_reg_us, firm_reg, by = "p_key")
firm_reg_both <- mutate(firm_reg_both, organization.x = tolower(organization.x),  organization.y = tolower(organization.y)) %>% mutate(diff = stringdist(organization.x, organization.y))
firm_reg_both <- setDT(firm_reg_both)[order(diff), .SD[1], .(p_key, organization.x)][diff<10]
firm_reg_both <- dplyr::select(firm_reg_both, p_key, organization.x, lat.x, lng.x, country.y) %>% dplyr::rename(organization = organization.x, lat = lat.x, lng = lng.x, country = country.y)
firm_reg_both <- mutate(firm_reg_both, lat = ifelse(str_detect(organization, "hoffmann") & country == "CH", 47.5584, lat), lng = ifelse(str_detect(organization, "hoffmann") & country == "CH", 7.5733, lng))

firm_reg_us <- filter(firm_reg_us, !(p_key %in% firm_reg_both$p_key))
# firm_reg_us <- mutate(firm_reg_us, lat = ifelse(str_detect(organization, "Hoffmann-La Roche"), 47.5584, lat), lng = ifelse(str_detect(organization, "Hoffmann-La Roche"), 7.5733, lng), country = ifelse(str_detect(organization, "Hoffmann-La Roche"), "CH", country))
firm_reg_us <- rbind.fill(firm_reg_us, firm_reg_both)


## Find crossborder-commuters
inv_firm <- inner_join(inv_reg_us, firm_reg_us, by = c("p_key"))
inv_firm <- mutate(inv_firm, lat_diff = abs(lat.x -lat.y), lng_diff = abs(lng.x - lng.y)) %>%  mutate(dist = lat_diff^2 + lng_diff^2, max_dist = ifelse(lat_diff < 1 & lng_diff < 1, 0, 1))
inv_firm <- setDT(inv_firm)[order(dist), .SD[1], .(p_key, inventor_id)]
inv_firm <- mutate(inv_firm, cross_board = ifelse(ctry_code != country & max_dist == 0, "yes", "no")) ## crossborder-commuter if inventor life not in the same country as location of firm AND both geographic locations are not too far away

## Detect patents which are developed only by crossborder-commuters and thus cannot be seen in statistics
inv_firm <- setDT(inv_firm)[, cbind("num_inv", "num_cross") := list(.N, sum(cross_board == "yes", na.action = NULL)), .(p_key)]
inv_firm <- mutate(inv_firm, pat_only_crossbord = ifelse(num_inv == num_cross, "yes", "no"))
inv_firm <- distinct(inv_firm, p_key, inventor_id, .keep_all = T)


## Add forward citations
q <- readRDS(paste0(mainDir1, "/created data/info_cited_pat.rds"))
q <- setDT(q)[order(-fwd_cits5), .SD[1], .(p_key)]
q <- mutate(q, cit_cat_y_5 = case_when(fwd_cits5 <= 1 ~ 0, fwd_cits5 >= 2 & fwd_cits5 < 16 ~ 1, fwd_cits5 > 15 ~ 2))
q <- dplyr::select(q, p_key, cit_cat_y_5) %>% mutate(p_key= as.character(p_key))

inv_ch <- left_join(inv_ch, q, by = "p_key")

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