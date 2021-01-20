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
    filter(city %in% c('Nation', kTargetCity)) %>% 
    left_join(iqvia.info, by = c('packid' = 'PACK')) %>% 
    left_join(qty.info, by = 'packid') %>% 
    left_join(mu.info, by = 'packid') %>% 
    mutate(Volume = if_else(market == 'INS', mu * units, quantity * units), 
           INSVIAL = if_else(INSVIAL == 'NA', NA_character_, INSVIAL), 
           INSPEN = if_else(INSPEN == 'NA', NA_character_, INSPEN), 
           INSBAS = if_else(INSBAS == 'NA', NA_character_, INSBAS)) %>% 
    select(City = city_en, `TC IV SHORT DESC`, `ATC IV DESC`, 
           `CORPORATE SHORT DESC`, `CORPORATE DESC`, `MANUFACT. SHORT DESC`, 
           `MANUFACT. DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
           `PACK SHORT DESC`, `PACK DESC`, PACK = packid, CR, Category, 
           Category2, Product, MOLECULE, INSVIAL, INSPEN, INSBAS, quarter, 
           Value = sales, Volume) %>% 
    # filter(Value > 0, Volume > 0) %>% 
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
  novo.mat <- novo.quarter %>% 
    group_by(City, PACK, Measure) %>% 
    arrange(quarter) %>% 
    mutate(value = value + lag(value, 1) + lag(value, 2) + lag(value, 3)) %>% 
    ungroup() %>% 
    filter(!is.na(value)) %>% 
    mutate(quarter = stri_paste('MAT ', stri_sub(quarter, 3, 6)))
  
  ##---- YTD ----
  novo.ytd <- novo.quarter %>% 
    mutate(q = stri_sub(quarter, 5, 6)) %>% 
    group_by(City, PACK, Measure) %>% 
    arrange(quarter) %>% 
    mutate(value = case_when(q == 'Q1' ~ value, 
                             q == 'Q2' ~ value + lag(value, 1), 
                             q == 'Q3' ~ value + lag(value, 1) + lag(value, 2), 
                             q == 'Q4' ~ value + lag(value, 1) + lag(value, 2) + lag(value, 3), 
                             TRUE ~ NaN)) %>% 
    ungroup() %>% 
    select(-q) %>% 
    filter(!is.na(value)) %>% 
    mutate(quarter = stri_paste('YTD ', stri_sub(quarter, 3, 6)))
  
  ##---- Result ----
  novo.result <- bind_rows(novo.quarter, novo.mat, novo.ytd) %>% 
    pivot_wider(names_from = quarter, 
                values_from = value) %>% 
    arrange(Measure)
  
  return(novo.result)
}
