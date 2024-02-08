setwd('/Users/jessefrederik/Documents/GitHub/LISS Panel nativisme')
source('lisspanel.R')

## Inladen verkiezingsdata van Parlgov: https://parlgov.org/ 

setwd('/Users/jessefrederik/Documents/GitHub/Rechtspopulisme')
election <- read.csv('datafiles/view_election.csv') %>%
  filter(
    country_name == "Netherlands",
    election_type == "parliament",
    election_date > "2005-01-01" &
      election_date < "2022-01-01"
  ) %>%
  mutate(
    year = year(election_date),
    party_name_short = ifelse(party_name_short == "VN", "Volt", party_name_short)
  )  %>%
  select(partij = party_name_short, year, vote_share) %>%
  mutate(vote_share = vote_share / 100)

## Inladen opkomstdata van Parlement.com: https://www.parlement.com/id/vh8lnhrp8wsz/opkomst_bij_tweede_kamerverkiezingen


opkomst <-
  read.csv("datafiles/opkomst.csv", dec = ",") %>%
  mutate(Opkomstpercentage = Opkomstpercentage / 100,
         opkomst_neg = 1 - Opkomstpercentage) %>%
  pivot_longer(
    cols = c(Opkomstpercentage, opkomst_neg),
    names_to = "stem_past",
    values_to = "prop_opkomst"
  ) %>%
  mutate(stem_past = case_when(
    stem_past == "Opkomstpercentage" ~ 1,
    stem_past == "opkomst_neg" ~ 2
  ))


## Survey weights maken op basis van de opkomst in het LISS-panel en de werkelijke opkomst
cv_filtered_weights_stem <- cv_filtered_def %>%
  ## Filter niet-stemgerechtigden uit de data
  filter(stem_past < 3) %>% 
  group_by(verkiezingen, stem_past) %>%
  reframe(n = n()) %>%
  group_by(verkiezingen) %>%
  mutate(prop = n / sum(n)) %>% 
  left_join(opkomst, by = c("verkiezingen" = "Jaar", "stem_past" = "stem_past")) %>% ##
  mutate(weights_stem = prop_opkomst / prop) %>%  
  select(verkiezingen, stem_past, weights_stem) %>% ## 
  left_join(cv_filtered_def,
            by = c("verkiezingen" = "verkiezingen", "stem_past" = "stem_past"))

## Survey weights maken op basis van het stemgedrag in het LISS-panel en het werkelijke stemgedrag en samenvoegen met opkomstdata

cv_weights <- cv_filtered_def %>%
  group_by(verkiezingen, vote_past) %>%
  filter(!is.na(vote_past)) %>%
  reframe(n = n()) %>%
  group_by(verkiezingen) %>%
  mutate(prop = n / sum(n)) %>%
  left_join(election, by = c("verkiezingen" = "year", "vote_past" = "partij")) %>%
  mutate(weights_vote = case_when(is.na(vote_share) ~ 1,
                                  TRUE ~ vote_share / prop)) %>%
  select(verkiezingen, vote_past, weights_vote) %>%
  right_join(
    cv_filtered_weights_stem,
    by = c("vote_past" = "vote_past", "verkiezingen" = "verkiezingen")
  ) %>%
  mutate(weights_vote = ifelse(is.na(weights_vote), 1, weights_vote),
         weights = weights_stem * weights_vote)


## MAAK DATA VOOR FIGUUR 1
cv_weights %>%
  mutate(
    partijkleur = case_when(
      vote_past == "PVV" ~ "Rechtspopulistisch",
      vote_past == "FvD" ~ "Rechtspopulistisch",
      vote_past == "JA21" ~ "Rechtspopulistisch",
      vote_past == "VVD" ~ "Rechts",
      vote_past == "CDA" ~ "Rechts",
      vote_past == "SGP" ~ "Rechts",
      vote_past == "CU" ~ "Links",
      vote_past == "D66" ~ "Links",
      vote_past == "PvdA" ~ "Links",
      vote_past == "GL" ~ "Links",
      vote_past == "SP" ~ "Links",
      vote_past == "Volt" ~ "Links",
      vote_past == "BIJ1" ~ "Links",
      vote_past == "DENK" ~ "Links",
      vote_past == "50Plus" ~ "Rechtspopulistisch",
      vote_past == "PvdD" ~ "Links",
      vote_past == "BBB" ~ "Rechtspopulistisch",
      vote_past == "Eén NL" ~ "Rechtspopulistisch",
      vote_past == "LPF" ~ "Rechtspopulistisch",
      stem_past == 2 ~ "Niet-stemmers",
      TRUE ~ "Andere partijen"
    )
  ) %>%
  
  mutate(
    stem_wilders = case_when(
      stem_past == 2 ~ "Niet-stemmers",
      partijkleur == "Rechtspopulistisch" ~ "Rechtspopulistisch",
      TRUE ~ "Andere kiezers"
    )
  ) %>%
  ## Arrange stem_wilders as factor "Niet-stemmers" first, then "Andere Kiezers", then "Rechtspopulistisch", then "PVV"
  mutate(stem_wilders = factor(
    stem_wilders,
    levels = c("Rechtspopulistisch", "Andere kiezers", "Niet-stemmers")
  )) %>%
  group_by(stem_wilders, verkiezingen, etnic_scale) %>%
  reframe(n = sum(weights)) %>%
  group_by(verkiezingen) %>%
  mutate(n = (n / sum(n)) * 100) %>%
  pivot_wider(names_from = stem_wilders, values_from = n) %>%
  arrange(etnic_scale) %>%
  ## replace NA with 0
  tidyr::replace_na(list(
    "Rechtspopulistisch" = 0,
    "Andere kiezers" = 0,
    "Niet-stemmers" = 0
  )) %>%
  write.csv(row.names = FALSE, file = "stem_wilders.csv")
