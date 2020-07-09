#######################################################################
# Cluster Basel / CR 30.6.2020 ##
#######################################################################
require(data.table)
require(ggplot2)
require(dplyr)
require(geosphere)
require(fuzzyjoin)
require(plyr)
print("Function to get regio data for USPTO inventor firms")

mainDir1 <- c("/scicore/home/weder/GROUP/Innovation/01_patent_data")

## Check where the patent inventors from the cluster basel are coming from
inv_reg <- readRDS(paste0(mainDir1, "/created data/inv_reg.rds")) 
## Considering only firms having patents applied from Switzerland
firm_reg <- readRDS(paste0(mainDir1, "/created data/firm_reg.rds"))
firm_ch  <- filter(firm_reg, country == "CH" & tech_f == 16 & p_year > 1989) %>% mutate(n = 1)

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
