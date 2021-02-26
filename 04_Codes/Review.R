# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novo CHC 2
# Purpose:      Review
# programmer:   Zhe Liu
# date:         2021-01-27
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Check SOP ----
# CHPA
chpa.format <- read.xlsx('02_Inputs/ims_chpa_to20Q3_format.xlsx')

novo.chpa <- chpa.format %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC3_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  left_join(market.def, by = c('ATC3_Code' = 'atc3', 'Pack_ID' = 'packid')) %>% 
  left_join(qty.info, by = c('Pack_ID' = 'packid')) %>% 
  left_join(mu.info, by = c('Pack_ID' = 'packid')) %>% 
  mutate(volume = if_else(market == 'INS', mu * UNIT, quantity * UNIT)) %>% 
  filter(!is.na(market), 
         UNIT > 0, RENMINBI > 0, 
         stri_sub(quarter, 1, 4) %in% c('2019', '2020')) %>% 
  select(Pack_ID, Date = quarter, ATC3 = ATC3_Code, MKT = market, 
         Molecule_Desc, Prod_Desc = Prd_desc, Pck_Desc, 
         Corp_Desc, Sales = RENMINBI, Units = volume)

write.xlsx(novo.chpa, '05_Internal_Review/Novo_CHC_CHPA_2019Q1_2020Q3.xlsx')


##---- Price ----
# price.check <- novo.result %>% 
#   mutate(price = round(Sales / Units)) %>% 
#   group_by(Channel, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID) %>% 
#   mutate(Sales = sum(Sales, na.rm = TRUE)) %>% 
#   ungroup() %>% 
#   arrange(Channel, City, MKT, -Sales, Date) %>% 
#   distinct(Channel, Date, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID, price) %>% 
#   pivot_wider(id_cols = c(Channel, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID), 
#               names_from = Date, 
#               values_from = price)
# 
# write.xlsx(price.check, '05_Internal_Review/Servier_CHC2_2018Q1_2020Q3_Price_Check.xlsx')
