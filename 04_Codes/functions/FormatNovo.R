# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Format Novo
# programmer:   Zhe Liu
# Date:         2021-01-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


FormatNovo <- function(proj.result, 
                       iqvia.info, 
                       qty.info, 
                       mu.info, 
                       target.city, 
                       city.en) {
  
  ##---- Summary ----
  novo.summary <- proj.result %>% 
    group_by(year, quarter, province, city, market, packid) %>% 
    summarise(units = sum(units, na.rm = TRUE), 
              sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(city.en, by = 'city')
  
  ##---- Quater ----
  novo.quarter <- novo.summary %>% 
    group_by(year, quarter, province = 'Nation', city = 'Nation', 
             city_en = 'Nation', market, packid) %>% 
    summarise(units = sum(units, na.rm = TRUE), 
              sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    bind_rows(novo.summary) %>% 
    filter(city %in% c('Nation', target.city)) %>% 
    left_join(iqvia.info, by = c('packid' = 'PACK')) %>% 
    left_join(qty.info, by = 'packid') %>% 
    left_join(mu.info, by = 'packid') %>% 
    mutate(Volume = if_else(market == 'INS', mu * units, quantity * units), 
           INSVIAL = if_else(INSVIAL == 'NA', NA_character_, INSVIAL), 
           INSPEN = if_else(INSPEN == 'NA', NA_character_, INSPEN), 
           INSBAS = if_else(INSBAS == 'NA', NA_character_, INSBAS)) %>% 
    select(City = city_en, MKT = market, `TC IV SHORT DESC`, `ATC IV DESC`, 
           `CORPORATE SHORT DESC`, `CORPORATE DESC`, `MANUFACT. SHORT DESC`, 
           `MANUFACT. DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
           `PACK SHORT DESC`, `PACK DESC`, PACK = packid, CR, Category, 
           Category2, Product, MOLECULE, INSVIAL, INSPEN, INSBAS, quarter, 
           Value = sales, Volume) %>% 
    filter(Value > 0, Volume > 0, !is.na(MOLECULE)) %>% 
    pivot_longer(cols = c(Value, Volume), 
                 names_to = 'Measure', 
                 values_to = 'value') %>% 
    pivot_wider(names_from = quarter, 
                values_from = value, 
                values_fill = 0) %>% 
    pivot_longer(cols = starts_with('20'), 
                 names_to = 'quarter', 
                 values_to = 'value')
  
  ##---- MAT ----
  # novo.mat <- novo.quarter %>% 
  #   group_by(City, PACK, Measure) %>% 
  #   arrange(quarter) %>% 
  #   mutate(value = value + lag(value, 1) + lag(value, 2) + lag(value, 3)) %>% 
  #   ungroup() %>% 
  #   filter(!is.na(value)) %>% 
  #   mutate(quarter = stri_paste('MAT ', stri_sub(quarter, 3, 6)))
  
  ##---- YTD ----
  # novo.ytd <- novo.quarter %>% 
  #   mutate(qtr = stri_sub(quarter, 5, 6)) %>% 
  #   group_by(City, PACK, Measure) %>% 
  #   arrange(quarter) %>% 
  #   mutate(value = case_when(qtr == 'Q1' ~ value, 
  #                            qtr == 'Q2' ~ value + lag(value, 1), 
  #                            qtr == 'Q3' ~ value + lag(value, 1) + lag(value, 2), 
  #                            qtr == 'Q4' ~ value + lag(value, 1) + lag(value, 2) + lag(value, 3), 
  #                            TRUE ~ NaN)) %>% 
  #   ungroup() %>% 
  #   select(-qtr) %>% 
  #   filter(!is.na(value)) %>% 
  #   mutate(quarter = stri_paste('YTD ', stri_sub(quarter, 3, 6)))
  
  ##---- Result ----
  # novo.result <- bind_rows(novo.quarter, novo.mat, novo.ytd) %>% 
  #   pivot_wider(names_from = quarter, 
  #               values_from = value) %>% 
  #   arrange(Measure)
  
  novo.result <- novo.quarter %>% 
    pivot_wider(names_from = Measure, 
                values_from = value) %>% 
    select(Quarter = quarter, 
           City, 
           MKT, 
           Molecule_Desc = MOLECULE, 
           Prod_Desc = `PRODUCT DESC`, 
           Corp_Desc = `CORPORATE DESC`, 
           Corp_Type = `CORPORATE DESC`, 
           ATC4 = `TC IV SHORT DESC`, 
           Pack_Desc = `PACK DESC`, 
           Pack_ID = PACK, 
           Value = Value, 
           Volume = Volume)
  
  return(novo.result)
}
