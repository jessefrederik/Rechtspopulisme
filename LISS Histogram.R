#source('lisspanel.R')

cv_lags <- cv_filtered_def %>%
  filter(!is.na(etnic_scale)) %>% 
  arrange(nomem_encr, jaar) %>%
  group_by(nomem_encr) %>%
  ## Find difference in etnic_scale with different lags from 1 to 14
  mutate(
    diff_1 = etnic_scale - dplyr::lag(etnic_scale, 1),
    diff_2 = etnic_scale - dplyr::lag(etnic_scale, 2),
    diff_3 = etnic_scale - dplyr::lag(etnic_scale, 3),
    diff_4 = etnic_scale - dplyr::lag(etnic_scale, 4),
    diff_5 = etnic_scale - dplyr::lag(etnic_scale, 5),
    diff_6 = etnic_scale - dplyr::lag(etnic_scale, 6),
    diff_7 = etnic_scale - dplyr::lag(etnic_scale, 7),
    diff_8 = etnic_scale - dplyr::lag(etnic_scale, 8),
    diff_9 = etnic_scale - dplyr::lag(etnic_scale, 9),
    diff_10 = etnic_scale - dplyr::lag(etnic_scale, 10),
    diff_11 = etnic_scale - dplyr::lag(etnic_scale, 11),
    diff_12 = etnic_scale - dplyr::lag(etnic_scale, 12),
    diff_13 = etnic_scale - dplyr::lag(etnic_scale, 13),
    diff_14 = etnic_scale - dplyr::lag(etnic_scale, 14)
  ) %>%
  pivot_longer(cols = starts_with("diff"),
               names_to = "lag",
               values_to = "diff") 

cv_lags %>% 
  mutate(diff = round(diff,1)) %>% 
  filter(!is.na(diff)) %>%
  group_by(lag, diff = as.factor(diff)) %>% 
  reframe(
    count = n()
  ) %>%
  group_by(lag) %>% 
  mutate(count = count/sum(count) *100) %>%
  mutate(lag = fct_recode(lag,
                          "Jaar 1" = "diff_1",
                          "Jaar 2" = "diff_2",
                          "Jaar 3" = "diff_3",
                          "Jaar 4" = "diff_4",
                          "Jaar 5" = "diff_5",
                          "Jaar 6" = "diff_6",
                          "Jaar 7" = "diff_7",
                          "Jaar 8" = "diff_8",
                          "Jaar 9" = "diff_9",
                          "Jaar 10" = "diff_10",
                          "Jaar 11" = "diff_11",
                          "Jaar 12" = "diff_12",
                          "Jaar 13" = "diff_13",
                          "Jaar 14" = "diff_14")) %>% 
  mutate(lag = fct_relevel(lag, "Jaar 1", "Jaar 2", "Jaar 3", "Jaar 4", "Jaar 5", "Jaar 6", "Jaar 7", "Jaar 8", "Jaar 9", "Jaar 10", "Jaar 11", "Jaar 12", "Jaar 13", "Jaar 14")) %>%
  arrange(lag, diff) %>% 
  pivot_wider(names_from = lag, values_from = count) %>%
  pivot_longer(cols = `Jaar 1`:`Jaar 14`,
               names_to = "lag",
               values_to = "count") %>%
  
  ## Levels of lag are now Jaar 1, Jaar 2, Jaar 3 etc.
  mutate(lag = factor(lag, levels = paste0("Jaar ", 1:14)),
         count = tidyr::replace_na(count, 0)) %>%
  arrange(lag, diff) %>%
    write.csv(row.names = F, "lisspanel.csv")