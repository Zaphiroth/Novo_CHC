# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Novo CHC
# Purpose:      Imputation of Tianjin
# programmer:   Zhe Liu
# Date:         2021-01-19
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model ----
## model set
tj.model.data <- raw.total %>% 
  mutate(flag = if_else(province == '福建', 1, 0))

tj.model.set <- tj.model.data %>% 
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
tj.train.set <- tj.model.set[tj.model.set$flag == 0, ]
tj.test.set <- tj.model.set[tj.model.set$flag == 1, ]

tj.knn.model <- kknn(flag ~ ., 
                     train = tj.train.set[, -(1:4)], 
                     test = tj.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
tj.model.indice <- as.data.frame(tj.knn.model$C) %>% 
  lapply(function(x) {
    tj.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(tj.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

tj.model.weight <- as.data.frame(tj.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(tj.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Growth ----
## model growth
tj.model.growth <- tj.model.data %>% 
  filter(flag == 0) %>% 
  distinct(year, date, quarter, province, city, district, pchc, packid, sales, flag) %>% 
  group_by(knn_pchc = pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(tj.model.indice, by = 'knn_pchc') %>% 
  left_join(tj.model.weight, 
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