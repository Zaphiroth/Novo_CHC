# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novo CHC
# Purpose:      Imputation of Fujian
# programmer:   Zhe Liu
# Date:         2021-01-18
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model ----
## model set
fj.model.data <- raw.total %>% 
  mutate(flag = if_else(province == '福建', 1, 0))

fj.model.set <- fj.model.data %>% 
  filter(year %in% c('2018', '2019', '2020'), quarter <= '2020Q3') %>% 
  distinct(date, province, city, district, pchc, packid, sales, flag) %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

## model
fj.train.set <- fj.model.set[fj.model.set$flag == 0, ]
fj.test.set <- fj.model.set[fj.model.set$flag == 1, ]

fj.knn.model <- kknn(flag ~ ., 
                     train = fj.train.set[, -(1:4)], 
                     test = fj.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
fj.model.indice <- as.data.frame(fj.knn.model$C) %>% 
  lapply(function(x) {
    fj.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(fj.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

fj.model.weight <- as.data.frame(fj.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(fj.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Growth ----
## model growth
fj.model.growth <- fj.model.data %>% 
  filter(flag == 0) %>% 
  distinct(year, date, quarter, province, city, district, pchc, packid, sales, flag) %>% 
  group_by(knn_pchc = pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(fj.model.indice, by = 'knn_pchc') %>% 
  left_join(fj.model.weight, 
            by = c('province', 'city', 'district', 'pchc', 'knn_level')) %>% 
  group_by(pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = stri_sub(date, 5, 6)) %>% 
  pivot_wider(id_cols = c(pchc, packid, month), 
              names_from = year, 
              values_from = sales, 
              values_fill = 0) %>% 
  mutate(growth_1718 = `2018` / `2017`, 
         growth_1819 = `2019` / `2018`, 
         growth_1920 = `2020` / `2019`) %>% 
  select(pchc, packid, month, `2017` = growth_1718, `2018` = growth_1819, 
         `2019` = growth_1920) %>% 
  pivot_longer(cols = starts_with('20'), 
               names_to = 'year', 
               values_to = 'growth') %>% 
  filter(!is.na(growth), !is.infinite(growth)) %>% 
  mutate(date = stri_paste(year, month)) %>% 
  select(year, date, pchc, packid, growth)


##---- Prediction ----
## predict 2019
fj.predict.sales.19 <- fj.model.data %>% 
  filter(year %in% c('2018'), flag == 1) %>% 
  left_join(fj.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2018', '2019', date),
         quarter = gsub('2018', '2019', quarter),
         year = '2019',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)

## predict 2020
fj.predict.sales.20 <- fj.predict.sales.19 %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2019Q3')) %>% 
  left_join(fj.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2019', '2020', date),
         quarter = gsub('2019', '2020', quarter),
         year = '2020',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)


##---- Result ----
imp.fj <- bind_rows(fj.predict.sales.19, fj.predict.sales.20) %>% 
  filter(market %in% c('MNIAD(GLP1/DPP4/SGLT2)', 'OAD')) %>% 
  anti_join(fj.model.data, by = c('year', 'date', 'quarter', 'province', 'city', 
                                  'district', 'pchc', 'market', 'packid')) %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales, flag)
