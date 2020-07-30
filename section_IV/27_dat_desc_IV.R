#######################################################################
print("27: Create analysis for cluster basel: where are inventions registered and where do they actually happen/ CR 30.7.2020")
#######################################################################
require(data.table)
require(ggplot2)
require(dplyr)
require(geosphere)
require(fuzzyjoin)
require(plyr)

mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")
tech_field_start <- 16

## Load data created in 20 and 21
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg/inv_reg_us_", tech_field_start, ".rds")) %>% dplyr::select(p_key, name, inventor_id, lat, lng, ctry_code, Up_reg_label, patent_id)
inv_reg <- setDT(inv_reg)[, num_inv := .N, .(p_key)]
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_us_", tech_field_start, ".rds")) %>% dplyr::select(p_key, organization, lat, lng, country, Up_reg_label)
inv_firm <- inner_join(inv_reg, firm_reg, by = c("p_key"))
inv_firm <- mutate(inv_firm, lat_diff = abs(lat.x -lat.y), lng_diff = abs(lng.x - lng.y))
inv_firm <- setDT(inv_firm)[, ctry_in := ctry_code %in% country, .(p_key)]

inv_firm <- mutate(inv_firm, cross_board = ifelse(ctry_in == FALSE & lat_diff < 0.5 & lng_diff < 0.5, "yes", "no"))
inv_firm <- distinct(inv_firm, p_key, inventor_id, .keep_all = T)
inv_firm <- setDT(inv_firm)[, cbind("num_inv", "num_cross") := list(.N, sum(cross_board == "yes")), .(p_key)]

inv_firm_sub <- filter(inv_firm, num_cross == num_inv)

## Considering only firms having patents applied from Switzerland
firm_ch  <- filter(firm_reg, country == "CH" & p_year > 1989) %>% mutate(n = 1)


inv_ch <- filter(inv_reg, p_key %in% firm_ch$p_key)
inv_ch   <- setDT(inv_ch)[, num := ifelse(Ctry_code %in% c("CH", "DE", "FR"), 1, 0), .(p_key)]
inv_ch <- filter(inv_ch, num != 0 & Up_reg_label %in% c("Freiburg", "Alsace", "Rhône-Alpes") | str_detect(Ctry_code, "CH") == T)
inv_ch <- distinct(inv_ch, Pub_nbr, Ctry_code, .keep_all = T)
inv_ch <- setDT(inv_ch)[order(Ctry_code), ctr := paste(Ctry_code, collapse = "-"), .(Pub_nbr)]

patent_inv <- aggregate(num ~ ctr + cit_cat_y_5, FUN = sum, data = distinct(inv_ch, p_key, .keep_all = T))
patent_inv <- mutate(patent_inv, status = ifelse(str_detect(ctr, "CH") & !(str_detect(ctr, "FR|DE")), "only ch", ifelse(str_detect(ctr, "CH") & (str_detect(ctr, "FR|DE")), "ch and cross-border", "only cross-border")))
setDT(patent_inv)[, ges_per_status := sum(num), .(status, cit_cat_y_5)]


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