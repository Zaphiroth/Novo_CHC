# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Format Novo
# programmer:   Zhe Liu
# Date:         2021-01-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


FormatNovo <- function(proj.result, 
                       iqvia.info, 
                       mu.info) {
  
  ##---- Summary ----
  novo.quarter <- proj.result %>% 
    group_by(year, quarter, province, city, market, packid) %>% 
    summarise(units = sum(units, na.rm = TRUE), 
              sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(iqvia.info, by = c('packid' = 'PACK')) %>% 
    left_join(mu.info, by = 'packid') %>% 
    mutate(Volume = if_else(market == 'INS', mu * units, dosage * units), 
           INSVIAL = if_else(INSVIAL == 'NA', NA_character_, INSVIAL), 
           INSPEN = if_else(INSPEN == 'NA', NA_character_, INSPEN), 
           INSBAS = if_else(INSBAS == 'NA', NA_character_, INSBAS)) %>% 
    select(Province = province, City = city, `TC IV SHORT DESC`, `ATC IV DESC`, 
           `CORPORATE SHORT DESC`, `CORPORATE DESC`, `MANUFACT. SHORT DESC`, 
           `MANUFACT. DESC`, `PRODUCT SHORT DESC`, `PRODUCT DESC`, 
           `PACK SHORT DESC`, `PACK DESC`, PACK = packid, CR, Category, 
           Category2, Product, MOLECULE, INSVIAL, INSPEN, INSBAS, quarter, 
           Value = sales, Volume)
  
  novo.mat <- novo.quarter %>% 
    group_by(Province, City, PACK) %>% 
    arrange(quarter) %>% 
    mutate(Value = Value + lag(Value, 1) + lag(Value, 2) + lag(Value, 3), 
           Volume = Volume + lag(Volume, 1) + lag(Volume, 2) + lag(Volume, 3)) %>% 
    ungroup() %>% 
    filter(Value > 0, Volume > 0) %>% 
    mutate(quarter = stri_paste('MAT ', stri_sub(quarter, 3, 6)))
  
  novo.ytd <- novo.quarter %>% 
    mutate(q = stri_sub(quarter, 5, 6)) %>% 
    group_by(Province, City, PACK) %>% 
    arrange(quarter) %>% 
    mutate(Value = case_when(q == 'Q1' ~ Value, 
                             q == 'Q2' ~ Value + lag(Value, 1), 
                             q == 'Q3' ~ Value + lag(Value, 1) + lag(Value, 2), 
                             q == 'Q4' ~ Value + lag(Value, 1) + lag(Value, 2) + lag(Value, 3), 
                             TRUE ~ NaN), 
           Volume = case_when(q == 'Q1' ~ Volume, 
                              q == 'Q2' ~ Volume + lag(Volume, 1), 
                              q == 'Q3' ~ Volume + lag(Volume, 1) + lag(Volume, 2), 
                              q == 'Q4' ~ Volume + lag(Volume, 1) + lag(Volume, 2) + lag(Volume, 3), 
                              TRUE ~ NaN)) %>% 
    ungroup() %>% 
    filter(Value > 0, Volume > 0) %>% 
    mutate(quarter = stri_paste('YTD ', stri_sub(quarter, 3, 6)))
  
}
