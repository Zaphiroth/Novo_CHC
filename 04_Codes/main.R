# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novo CHC
# Purpose:      Main
# programmer:   Zhe Liu
# Date:         2021-01-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Total sample ----
imp.total <- raw.total %>% 
  mutate(flag = 0) %>% 
  bind_rows(imp.sh, imp.fj) %>% 
  filter(quarter >= '2019Q1', quarter <= '2020Q3')

write_feather(imp.total, '03_Outputs/Novo_CHC_Imp.feather')


##---- Universe info ----
## PCHC
pchc.universe <- read.xlsx("02_Inputs/2020_PCHC_Universe更新维护.xlsx", 
                           sheet = "2020 CHC universe", cols = 1:19)

pchc.universe.m <- pchc.universe %>% 
  distinct(province = gsub('省|市', '', `省`), 
           city = gsub('市', '', `地级市`), 
           district = `区[县/县级市]`, 
           pchc = `新版PCHC_Code`, 
           est = `其中：西药药品收入（千元）`) %>% 
  filter(est > 0) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

hospital.universe <- bind_rows(imp.total, pchc.universe.m) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup() %>% 
  filter(!is.na(province), !is.na(city), !is.na(district), !is.na(est)) %>% 
  mutate(flag_sample = if_else(pchc %in% unique(imp.total$pchc), 1, 0))

## city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = if_else(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = if_else(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)


##---- Run Projection ----
source('04_Codes/functions/ProjectSample.R', encoding = 'UTF-8')
source('04_Codes/functions/ProjectNation.R', encoding = 'UTF-8')
source('04_Codes/functions/UpdatePrice.R', encoding = 'UTF-8')

proj.sample <- ProjectSample(imp.total = imp.total, 
                             hospital.universe = hospital.universe)

proj.nation <- ProjectNation(proj.sample = proj.sample, 
                             hospital.universe = hospital.universe, 
                             city.tier = city.tier)

proj.price <- UpdatePrice(proj.nation = proj.nation, 
                          imp.total = imp.total)

proj.result <- proj.price %>% 
  filter(quarter >= '2019Q1', quarter <= '2020Q3')

write_feather(proj.result, '03_Outputs/Novo_CHC_Proj.feather')


##---- Format info ----
## target city
kTargetCity <- c('北京', '上海', '广州', '杭州', '天津', '南京', '苏州', '宁波', 
                 '无锡', '温州', '福州', '济南', '青岛', '嘉兴', '南通', '徐州', 
                 '常州', '盐城', '潍坊', '镇江', '厦门')

## city EN
city.en <- read.xlsx('02_Inputs/CityEN.xlsx')

## IQVIA info
iqvia.info <- read_excel('02_Inputs/IQVIA County Dec 19.xlsx', 
                         sheet = 2, range = cell_cols('A:W')) %>% 
  mutate(PACK = stri_pad_left(PACK, 7, 0)) %>% 
  # separate(`PACK SHORT DESC`, c('padding', 'dosage'), sep = 'x', 
  #          remove = FALSE, convert = TRUE) %>% 
  distinct(PACK, `TC IV SHORT DESC`, `ATC IV DESC`, `CORPORATE SHORT DESC`, 
           `CORPORATE DESC`, `MANUFACT. SHORT DESC`, `MANUFACT. DESC`, 
           `PRODUCT SHORT DESC`, `PRODUCT DESC`, `PACK SHORT DESC`, `PACK DESC`, 
           PACK, CR, Category, Category2, Product, MOLECULE, INSVIAL, INSPEN, 
           INSBAS)

write.xlsx(iqvia.info, '05_Internal_Review/IQVIA_Info.xlsx')

## MU
CalcMU <- function(kPackDesc, per) {
  
  # kPackDesc <- 'VIAL SC 1K 10ML   1'
  
  kPackDesc <- gsub('\\s+', ' ', stri_trim(kPackDesc))
  split.matrix <- stri_split_fixed(kPackDesc, ' ', simplify = TRUE)
  iu <- as.numeric(gsub('IU', '', split.matrix[which(grepl('IU', split.matrix))]))
  ml <- as.numeric(gsub('ML', '', split.matrix[which(grepl('ML', split.matrix))]))
  num <- as.numeric(split.matrix[length(split.matrix)])
  
  if (per == 1) {
    mu <- iu * ml * num / 1000000
    mu <- ifelse(length(mu) > 0, mu, NaN)
  } else if (per == 0) {
    mu <- iu * num / 1000000
    mu <- ifelse(length(mu) > 0, mu, NaN)
  } else {
    mu <- NaN
  }
  
  return(mu)
}

mu.info <- chpa.info %>% 
  filter(atc3 %in% c('A10C', 'A10D')) %>% 
  mutate(pack = toupper(pack), 
         per = if_else(grepl('/ML', pack), 1, 0), 
         pack = gsub('/ML', '', pack), 
         mu = mapply(CalcMU, pack, per)) %>% 
  select(packid, mu)

## quantity
qty.info <- chpa.info %>% 
  mutate(last_space_position = stri_locate_last(pack, regex = '\\s')[, 1], 
         quantity = as.integer(str_squish(substr(pack, last_space_position, 
                                                 nchar(pack))))) %>% 
  select(packid, quantity)


##---- Run formation ----
source('04_Codes/functions/FormatNovo.R', encoding = 'UTF-8')

novo.result <- FormatNovo(proj.result = proj.result, 
                          iqvia.info = iqvia.info, 
                          qty.info = qty.info, 
                          mu.info = mu.info, 
                          target.city = kTargetCity, 
                          city.en = city.en)

write.xlsx(novo.result, '03_Outputs/Novo_CHC_2019Q1_2020Q3.xlsx')
