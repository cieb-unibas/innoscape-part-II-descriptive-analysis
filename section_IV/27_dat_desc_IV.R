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
require(RecordLinkage)
require(fuzzyjoin)
require(tm)

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

# firm_reg_us <- filter(firm_reg_us, !(p_key %in% firm_reg_both$p_key))
# firm_reg_us <- rbind.fill(firm_reg_us, firm_reg_both)


## Find crossborder-commuters
inv_firm <- inner_join(inv_reg_us, firm_reg_us, by = c("p_key"))
inv_firm <- mutate(inv_firm, lat_diff = abs(lat.x -lat.y), lng_diff = abs(lng.x - lng.y)) %>%  filter(is.na(lat_diff) != T) %>% mutate(dist = lat_diff^2 + lng_diff^2, max_dist = ifelse(lat_diff < 1 & lng_diff < 1, 0, 1))
inv_firm <- setDT(inv_firm)[order(dist), .SD[1], .(p_key, inventor_id)] ## Use firm location having the lowest distance to the inventor location
inv_firm <- mutate(inv_firm, cross_board = ifelse(ctry_code != country & max_dist == 0, "yes", ifelse(ctry_code == country, "no", "maybe"))) ## crossborder-commuter if inventor life not in the same country as location of firm AND both geographic locations are not too far away
inv_firm <- dplyr::rename(inv_firm, lat = lat.x, lng = lng.x)
inv_firm <- distinct(inv_firm, p_key, organization, inventor_id, lat, lng, .keep_all = T)

## Create function to derive crossborder-commuters for those patents which have unambigous information 
derive_cross_board_func <- function(ctry_firm, inv_ctry){##first input parameter is for the country where the patent has been invented and the second for the countries where the inventors may come from (crossborder-commuters)
  ## Get list of firms of country ctry_firm
  firm_list <- readRDS(paste0(mainDir1, "/created data/firm_reg/firm_reg_", tech_field_start, ".rds")) %>% dplyr::select(p_key, organization, lat, lng, country, pub_nbr)
  firm_list <- filter(firm_list, country %in% ctry_firm) %>% mutate(firm_list, organization = tolower(organization))
  firm_list$organization <- gsub('[[:punct:] ]+',' ', firm_list$organization)
  firm_list <- mutate(firm_list, organization = trimws(removeWords(organization, c("ex", "us", "gmbh", "s a", "ac", "sa", "ing", "a g", "co", "corp", "inc", "ltd", "llc", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", 
                                                                            "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "international", "technologies", "technology", "products", "group", "holdings", "elevator", "china", "germany", 
                                                                            "usa", "engineering", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", "corp", "div", 
                                                                            "kaisha", "cie",  "incorporated", "industries", "/", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "industrietreuhand", 
                                                                            "kommanditgesellschaft", "pourrecherchemicrotechnique", "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "research")), which = "both")) 
  firm_list <- setDT(firm_list)[, num := .N, .(organization)]## use only firms for which we know that they have at least some patents in Switzerland
  firm_list <- filter(firm_list, num > 5)
  firm_list <- aggregate(cbind(lat, lng) ~ organization, FUN = function(x)mean(x, na.rm = T, na.action = NULL), data = firm_list)
  firm_list <- distinct(firm_list, organization, lat, lng)
  
  inv_firm_maybe <- filter(inv_firm, cross_board == "maybe" & ctry_code %in% inv_ctry & !(country %in% c(ctry_firm, inv_ctry))) %>% dplyr::select(organization, ctry_code, p_key, inventor_id, lat, lng) 
  inv_firm_maybe$organization <- gsub('[[:punct:] ]+',' ', inv_firm_maybe$organization)
  inv_firm_maybe <- mutate(inv_firm_maybe, organization = trimws(removeWords(tolower(organization), c("ex", "us", "gmbh", "s a", "ac", "sa", "ing", "a g", "co", "corp", "inc", "ltd", "llc", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", 
                                                                            "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "international", "technologies", "technology", "products", "group", "holdings", "elevator", "china", "germany", 
                                                                            "usa", "engineering", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "pourrecherchemicrotechnique", "corp", "div", 
                                                                            "kaisha", "cie",  "incorporated", "industries", "/", "gmbh", "ing", "corp", "inc", "ltd", "gmbh", "ag", "limited", "plc", "corporation", "company", "aktiengesellschaft", "industrietreuhand", 
                                                                            "kommanditgesellschaft", "pourrecherchemicrotechnique", "corp", "div", "kaisha", "cie",  "incorporated", "industries", "/", "research")), which = "both")) 
  
  ## crossborder-commuter if inventor life not in the same country as location of firm AND both geographic locations are not too far away
  close_firm <- difference_left_join(inv_firm_maybe, firm_list, by = c("lat", "lng"), max_dist = 1)
  close_firm <- filter(close_firm, is.na(organization.y) != T)
  close_firm <- mutate(close_firm, name_diff = stringdist(organization.x, organization.y))
  close_firm <- mutate(close_firm, lat_diff = abs(lat.x -lat.y), lng_diff = abs(lng.x - lng.y), dist = lat_diff^2 + lng_diff^2) %>% filter(lat_diff < 1 & lng_diff < 1 & name_diff < 4)
  close_firm <- setDT(close_firm)[order(dist, name_diff), .SD[1], .(p_key, inventor_id)]
  close_firm <- filter(close_firm, is.na(organization.y) != T) %>% mutate(cross_board = "yes", ctry_pat = ctry_firm) %>% select(inventor_id, p_key, cross_board, ctry_pat)
  return(close_firm)
  }

derive_cross_board <- derive_cross_board_func("CH", c("DE", "FR", "IT", "AT"))
inv_firm <- left_join(inv_firm, derive_cross_board, by = c("p_key", "inventor_id"))
inv_firm <- mutate(inv_firm, cross_board = ifelse(cross_board.y == "yes" & is.na(cross_board.y) != T, "yes", cross_board.x)) %>% 
            mutate(ctry_pat = ifelse(ctry_code == country, ctry_code, ifelse(ctry_code != country & cross_board.x == "yes", country, ifelse(ctry_code != country & is.na(cross_board.y) != T, ctry_pat, ctry_code))))
inv_firm <- distinct(inv_firm, p_key, inventor_id, .keep_all = T)

## Detect patents which are developed only by crossborder-commuters and thus cannot be seen in statistics
inv_firm <- setDT(inv_firm)[, cbind("num_inv", "num_cross") := list(.N, sum(cross_board == "yes", na.action = NULL)), .(p_key)]
inv_firm <- mutate(inv_firm, pat_only_crossboard = ifelse(num_inv == num_cross, "yes", "no"))

## Add p_year
## Add p_year
dat_p_year <- readRDS("/scicore/home/weder/GROUP/Innovation/01_patent_data/created data/dat_p_year.rds")  %>% mutate(p_key = as.character(p_key)) %>% dplyr::select(pub_nbr, p_year)
inv_firm <- left_join(inv_firm, dat_p_year, by = c("patent_id" = "pub_nbr"))

## Create variable to see whether using inventors' or firms' location lead to correct results regarding the "true" location of an invention
inv_firm <- mutate(inv_firm, correct_ctry_firm = ifelse(country == ctry_pat, "yes", "no"), correct_ctry_inv = ifelse(ctry_code == ctry_pat, "yes", "no"))
inv_firm <- setDT(inv_firm)[, inv_share := 1/.N, .(p_key)]

## Set share = 1 per country and patent
inv_ctry <- distinct(inv_firm, p_key, ctry_code, .keep_all = T)
inv_ctry <- aggregate(inv_share ~ ctry_code + p_year, FUN = length, data = inv_ctry)

pat_ctry <- distinct(inv_firm, p_key, ctry_pat, .keep_all = T)
pat_ctry <- aggregate(inv_share ~ ctry_pat + p_year, FUN = length, data = pat_ctry)

inv_pat_ctry <- left_join(pat_ctry, inv_ctry, by = c("ctry_pat" = "ctry_code", "p_year"))
inv_pat_ctry  <- mutate(inv_pat_ctry, pat_inv_ctry = inv_share.x/(inv_share.y))

## Use number of inventors per country as weight 
inv_share_correct <- aggregate(inv_share ~ ctry_pat + correct_ctry_inv + p_year, FUN = sum, data = inv_firm)
inv_share_correct <- dcast(inv_share_correct, ctry_pat + p_year ~ correct_ctry_inv, value.var = "inv_share")
inv_share_correct <- mutate(inv_share_correct, share_correct = (no + yes)/yes)

table(inv_firm$correct_ctry_inv, inv_firm$ctry_pat)
table(inv_firm$correct_ctry_firm, inv_firm$ctry_pat)

ggplot(filter(inv_share_correct, ctry_pat %in% c("CH")  & p_year < 2016), aes(x = p_year, y = share_correct, color = ctry_pat)) +
  geom_line() +
  geom_point() +
  xlab("Priority Year") +
  ylab("True location / inventor location")




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