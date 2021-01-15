# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novo Insulin CHC
# Purpose:      Readin
# programmer:   Zhe Liu
# Date:         2020-12-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## PCHC info
pchc.mapping <- read.xlsx("02_Inputs/Universe_PCHCCode_20201217.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.mapping %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.mapping %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

## CHPA
chpa.info <- read.xlsx('02_Inputs/ims_chpa_to20Q3.xlsx', cols = 1:21, startRow = 4) %>%  
  distinct(corp = Corp_Desc, type = MNF_TYPE, atc3 = ATC3_Code, atc4 = ATC4_Code,  
           molecule = Molecule_Desc, product = Prd_desc, pack = Pck_Desc,  
           packid = Pack_ID)

## market definition
market.def <- chpa.info %>% 
  mutate(market = case_when(atc3 %in% c('A10N', 'A10S', 'A10P') ~ 'MNIAD(GLP1/DPP4/SGLT2)', 
                            atc3 %in% c('A10H', 'A10J', 'A10K', 'A10L', 'A10M') ~ 'OAD', 
                            atc3 %in% c('A10C', 'A10D') ~ 'INS', 
                            TRUE ~ NA_character_)) %>% 
  filter(!is.na(market)) %>% 
  select(atc3, packid, market)


##---- Raw data ----
## Servier
raw.servier1 <- read_csv('02_Inputs/data/Servier_ahbjjssdzj_17181920Q1Q2Q3_fj1718_nozj20Q3_packid_moleinfo.csv', 
                        locale = locale(encoding = "GB18030")) %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10N', 'A10S', 'A10P', 'A10H', 'A10J', 'A10K', 'A10L', 'A10M')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.servier2 <- read.xlsx('02_Inputs/data/Servier_福建省_2019_packid_moleinfo(predicted by Servier_fj_2018_packid_moleinfo_v3).xlsx') %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10N', 'A10S', 'A10P', 'A10H', 'A10J', 'A10K', 'A10L', 'A10M')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.servier3 <- read.xlsx('02_Inputs/data/Servier_福建省_2020_packid_moleinfo(predicted by Servier_fj_2018_packid_moleinfo_v3).xlsx') %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10N', 'A10S', 'A10P', 'A10H', 'A10J', 'A10K', 'A10L', 'A10M')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.servier4 <- read.xlsx('02_Inputs/data/Servier_浙江省_2020Q3_packid_moleinfo(predicted by Servier_zj_2020Q1Q2_packid_moleinfo_v3).xlsx') %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10N', 'A10S', 'A10P', 'A10H', 'A10J', 'A10K', 'A10L', 'A10M')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.servier5 <- read_csv('02_Inputs/data/tj_18Q3_20Q2_packid_moleinfo.csv', 
                         locale = locale(encoding = "GB18030")) %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10N', 'A10S', 'A10P', 'A10H', 'A10J', 'A10K', 'A10L', 'A10M')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.servier <- bind_rows(raw.servier1, raw.servier2, raw.servier3, raw.servier4, raw.servier5) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  filter(quarter <= '2020Q3') %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid), 
         market = case_when(atc3 %in% c('A10N', 'A10S', 'A10P') ~ 'MNIAD(GLP1/DPP4/SGLT2)', 
                            atc3 %in% c('A10H', 'A10J', 'A10K', 'A10L', 'A10M') ~ 'OAD', 
                            atc3 %in% c('A10C', 'A10D') ~ 'INS', 
                            TRUE ~ NA_character_)) %>% 
  filter(units > 0, sales > 0, !is.na(market)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales)

## 胰岛素
raw.yds1 <- read_csv('02_Inputs/data/yidaosu_ahbjjssdzj171819_fj1718_packid_moleinfo.csv', 
                     locale = locale(encoding = "GB18030")) %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds2 <- read_csv('02_Inputs/data/yidaosu_ahbjjssdzj_2020Q1Q2_ahbjjs20Q3_packid_moleinfo.csv', 
                     locale = locale(encoding = "GB18030")) %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds3 <- read_csv('02_Inputs/data/yidaosu_sd_20Q3_packid_moleinfo.csv', 
                     locale = locale(encoding = "GB18030")) %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds4 <- read.xlsx('02_Inputs/data/yidaosu_福建省_2019_packid_moleinfo(predicted by yidaosu_fj_2018_packid_moleinfo_v2).xlsx') %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds5 <- read.xlsx('02_Inputs/data/yidaosu_福建省_2020_packid_moleinfo(predicted by yidaosu_fj_2018_packid_moleinfo_v2).xlsx') %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds6 <- read.xlsx('02_Inputs/data/yidaosu_浙江省_2020Q3_packid_moleinfo(predicted by yidaosu_zj_2020Q1Q2_packid_moleinfo_v2).xlsx') %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds7 <- read_csv('02_Inputs/data/tj_yidaosu_18Q3_20Q3_packid_moleinfo.csv', 
                     locale = locale(encoding = "GB18030")) %>% 
  filter(stri_sub(ATC4_Code, 1, 4) %in% c('A10C', 'A10D')) %>% 
  mutate(Year = as.character(Year), 
         Month = as.character(Month), 
         Prd_desc_ZB = as.character(Prd_desc_ZB))

raw.yds <- bind_rows(raw.yds1, raw.yds2, raw.yds3, raw.yds4, raw.yds5, raw.yds6, raw.yds7) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  filter(quarter <= '2020Q3') %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid), 
         market = case_when(atc3 %in% c('A10N', 'A10S', 'A10P') ~ 'MNIAD(GLP1/DPP4/SGLT2)', 
                            atc3 %in% c('A10H', 'A10J', 'A10K', 'A10L', 'A10M') ~ 'OAD', 
                            atc3 %in% c('A10C', 'A10D') ~ 'INS', 
                            TRUE ~ NA_character_)) %>% 
  filter(units > 0, sales > 0, !is.na(market)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales)

## Guangzhou
raw.gz1 <- read_feather('02_Inputs/data/Servier_guangzhou_17181920Q1Q2Q3_packid_moleinfo.feather')

raw.gz <- raw.gz1 %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = '广东', 
           city = '广州', 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  filter(quarter <= '2020Q3') %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  filter(!is.na(pchc)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid), 
         market = case_when(atc3 %in% c('A10N', 'A10S', 'A10P') ~ 'MNIAD(GLP1/DPP4/SGLT2)', 
                            atc3 %in% c('A10H', 'A10J', 'A10K', 'A10L', 'A10M') ~ 'OAD', 
                            atc3 %in% c('A10C', 'A10D') ~ 'INS', 
                            TRUE ~ NA_character_)) %>% 
  filter(units > 0, sales > 0, !is.na(market)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales)

## Shanghai
raw.sh1 <- read.xlsx('02_Inputs/data/上海_2017.xlsx')
raw.sh2 <- read.xlsx('02_Inputs/data/上海_2018.xlsx')

raw.sh <- bind_rows(raw.sh1, raw.sh2) %>% 
  mutate(quarter_m = stri_sub(Date, 5, 6)) %>% 
  distinct(year = stri_sub(Date, 1, 4), 
           quarter = ifelse(quarter_m %in% c('01', '02', '03'), 
                            stri_paste(year, 'Q1'), 
                            ifelse(quarter_m %in% c('04', '05', '06'), 
                                   stri_paste(year, 'Q2'), 
                                   ifelse(quarter_m %in% c('07', '08', '09'), 
                                          stri_paste(year, 'Q3'), 
                                          ifelse(quarter_m %in% c('10', '11', '12'), 
                                                 stri_paste(year, 'Q4'), 
                                                 year)))), 
           date = as.character(Date), 
           province = '上海', 
           city = '上海', 
           pchc = PCHC, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  mutate(pchc = case_when(pchc == 'PCHC06729' ~ 'PCHC06728', 
                          pchc == 'PCHC06622' ~ 'PCHC06620', 
                          pchc == 'PCHC06645' ~ 'PCHC06644', 
                          pchc == 'PCHC06722' ~ 'PCHC06721', 
                          pchc == 'PCHC06840' ~ 'PCHC06839', 
                          TRUE ~ pchc)) %>% 
  left_join(chpa.info, by = 'packid') %>% 
  filter(quarter <= '2020Q3') %>% 
  left_join(pchc.mapping4, by = c('province', 'city', 'pchc')) %>% 
  filter(pchc != '#N/A', stri_sub(packid, 1, 5) %in% stri_sub(market.def$packid, 1, 5)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid), 
         market = case_when(atc3 %in% c('A10N', 'A10S', 'A10P') ~ 'MNIAD(GLP1/DPP4/SGLT2)', 
                            atc3 %in% c('A10H', 'A10J', 'A10K', 'A10L', 'A10M') ~ 'OAD', 
                            atc3 %in% c('A10C', 'A10D') ~ 'INS', 
                            TRUE ~ NA_character_)) %>% 
  filter(units > 0, sales > 0, !is.na(market)) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales)

## total
raw.total <- bind_rows(raw.servier, raw.yds, raw.gz, raw.sh) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(raw.total, '03_Outputs/Novo_CHC_Raw.xlsx')
