library(grattan)
library(tidyverse)
library(haven)
library(srvyr)
library(sjlabelled)


## Verkiezingsuitslagen van 1994 en 1998 inladen voor survey weights

election <- read.csv('datafiles/view_election.csv') %>%  
  filter(country_name == "Netherlands", 
         election_type == "parliament", 
         election_date > "1994-01-01" & election_date < "1999-01-01") 

election1994 <- election %>% 
  filter(election_date == "1994-05-03") %>% 
  select(partij = party_name_short, vote_share) %>% 
  mutate(vote_share = vote_share/100)

NKO <- read_sav("datafiles/NKO_1971_2006.sav")
NKO2003 <- read_por("datafiles/NKO_2003.por")
NKO2006 <- read_sav("datafiles/NKO_2006.sav")
NKO2010 <- read_sav("datafiles/NKO_2010.sav")
NKO2012 <- read_sav("datafiles/NKO_2012.sav")
NKO2017 <- read_sav("datafiles/NKO_2017.sav")
NKO2021 <- read_sav("datafiles/NKO_2021.sav")
NKO1994 <- read_por("datafiles/NKO_1994.por")
NKO1998 <- read_por("datafiles/NKO_1998.por")

NKO %>% 
  select(jaar =A1,
         stem = V15_1, #stemwelniet
         stemkeuze =V15_2, #stem
         opv_inkomen = V38_10, #inko dsmen
         opv_integratie = V41_10, #integratie
         opv_asielzoekers = V43_10) %>% 
  filter(jaar < 1994) %>% 
  mutate(partij = case_when(stemkeuze == 1 ~ "PvdA",
                            stemkeuze == 2 ~ "CDA",
                            stemkeuze == 3 ~ "VVD",
                            stemkeuze == 4 ~ "D66",
                            stemkeuze == 5 ~ "GL",
                            stemkeuze == 6 ~ "SGP",
                            stemkeuze == 7 ~ "GPV",
                            stemkeuze == 8 ~ "RPF",
                            stemkeuze == 9 ~ "SP",
                            stemkeuze == 10 ~ "CD",
                            stemkeuze == 11 ~ "CDA",
                            stemkeuze == 12 ~ "CDA",
                            stemkeuze == 13 ~ "CDA",
                            stemkeuze == 14 ~ "GPV",
                            stemkeuze == 15 ~ "RPF",
                            TRUE ~ NA_character_),
         weights = 1) %>% 
  select(jaar, stem, partij, opv_inkomen, opv_integratie, opv_asielzoekers, weights) -> j

NKO1994 %>% 
  select(stem = VAR280,
         stemkeuze = VAR281, 
         opv_integratie = VAR081,
         opv_inkomen = VAR071,
         asiel_1 = VAR140, 
         asiel_2 = VAR144,
  ) %>%  
  mutate(stem = ifelse(is.na(stem), 2, stem)) %>%
  filter(asiel_1 <6, 
         asiel_2 < 6) %>% 
  mutate(asiel_2 = case_when(
    asiel_2 == 1 ~ 5,
    asiel_2 == 2 ~ 4,
    asiel_2 == 3 ~ 3,
    asiel_2 == 4 ~ 2,
    asiel_2 == 5 ~ 1
  ), 
  opv_asielzoekers  = asiel_1 + asiel_2, 
  
  partij = case_when(
    stemkeuze == 1 ~ "PvdA",
    stemkeuze == 2 ~ "CDA",
    stemkeuze == 3 ~ "VVD",
    stemkeuze == 4 ~ "D66",
    stemkeuze == 5 ~ "GL",
    stemkeuze == 6 ~ "SGP",
    stemkeuze == 7 ~ "GPV",
    stemkeuze == 8 ~ "RPF",
    stemkeuze == 9 ~ "CD",
    stemkeuze == 10 ~ "AOV|VSP",
    stemkeuze == 11 ~ "SP",
    stemkeuze == 19 ~ "other party",
    stemkeuze == 92 ~ "refused to answer",
    stemkeuze == 97 ~ "DK",
    stemkeuze == 99 ~ "INAP",
  ),
  weights = 1,
  jaar = 1994
  ) %>% 
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> i

i %>% 
  group_by(jaar, stem) %>% 
  summarise(weights = sum(weights, na.rm=T)) %>%
  mutate(prop = weights/sum(weights)) %>%
  mutate(opkomst_1994 = c(0.7875, 0.2125)) %>% 
  mutate(weights_opkomst = opkomst_1994 / prop) %>%
  select(stem, weights_opkomst) %>% 
  left_join(i, by = c("stem", "jaar")) -> i_weightsopkomst

i_weightsopkomst %>% 
  filter(stem == 1) %>% 
  group_by(jaar, partij) %>% 
  mutate(weights = case_when(NA ~ 1,
                             TRUE ~ weights)) %>% 
  summarise(weights = sum(weights, na.rm=T)) %>%
  group_by(jaar) %>% 
  mutate(weights = weights/sum(weights)) %>%
  filter(jaar == "1994") %>% 
  ungroup() %>% 
  select(!jaar) %>% 
  left_join(election1994, by = "partij") %>%
  mutate(weights_vote = vote_share / weights) %>% 
  select(weights_vote, partij) %>% 
  right_join(i_weightsopkomst, by = "partij") %>% 
  mutate(weights_vote = ifelse(is.na(weights_vote), 1, weights_vote)) %>%
  mutate(new_weights = weights_opkomst * weights_vote) %>%
  ## Weights is new_weights if NA then 1
  mutate(jaar = 1994) %>% 
  select(weights = new_weights, jaar, stem, partij, opv_integratie, opv_asielzoekers,opv_inkomen) -> i


NKO1998 %>% 
  select(weights = V0025,
         stem = V0610, #stemwelniet
         stemkeuze = V0611, #stem
         opv_integratie = V0144, #integratie
         opv_asielzoekers =V0130,
         opv_inkomen = V0123,
         
  ) %>%
  mutate(jaar = 1998) %>% 
  mutate(partij = case_when(
    stemkeuze == 1 ~ "PvdA",
    stemkeuze == 2 ~ "CDA",
    stemkeuze == 3 ~ "VVD",
    stemkeuze == 4 ~ "D66",
    stemkeuze == 5 ~ "GL",
    stemkeuze == 6 ~ "SGP",
    stemkeuze == 7 ~ "GPV",
    stemkeuze == 8 ~ "RPF",
    stemkeuze == 9 ~ "CD",
    stemkeuze == 10 ~ "AOV|VSP",
    stemkeuze == 11 ~ "AOV|VSP",
    stemkeuze == 12 ~ "SP",
    stemkeuze >= 13 ~ "Anders")
  ) %>% 
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> h



#NKO2002
NKO2003 %>% 
  select(weights = SDELM02, 
         stem = V0646,
         stem_2 = V0155,
         stemkeuze =V0647,
         stemkeuze_2 = V0156, 
         opv_inkomen = V0215,
         opv_asielzoekers = V0221,
         opv_integratie = V0882) %>% 
  ## IFELSE VOOR STEMKEUZE IF STEMKEUZE IS NA THEN FIND IN STEMKEUZE_2
  mutate(stem = ifelse(is.na(stem), stem_2, stem),
         stemkeuze = ifelse(is.na(stemkeuze), stemkeuze_2, stemkeuze),
         partij = case_when(
           stemkeuze == 1 ~ "PvdA",
           stemkeuze == 2 ~ "CDA",
           stemkeuze == 3 ~ "VVD",
           stemkeuze == 4 ~ "D66",
           stemkeuze == 5 ~ "GL",
           stemkeuze == 6 ~ "SGP",
           stemkeuze == 7 ~ "CU",
           stemkeuze == 8 ~ "Anders",
           stemkeuze == 9 ~ "LPF",
           stemkeuze == 10 ~ "SP",
           stemkeuze > 10 & stemkeuze <20 ~ "Anders",
           TRUE ~ NA_character_),
         jaar = 2002) %>%
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> a


NKO2003 %>% 
  select(weights = SDELM03,  
         stemkeuze = X0196,
         stem = X0195,
         opv_integratie = X0149,
         opv_asielzoekers = X0133,
         opv_inkomen = X0125) %>% 
  mutate(jaar = 2003,
         partij = case_when(
           stemkeuze == 1 ~ "CDA",
           stemkeuze == 2 ~ "LPF",
           stemkeuze == 3 ~ "VVD",
           stemkeuze == 4 ~ "PvdA",
           stemkeuze == 5 ~ "GL",
           stemkeuze == 6 ~ "SP",
           stemkeuze == 7 ~ "D66",
           stemkeuze == 8 ~ "CU",
           stemkeuze == 9 ~ "SGP",
           stemkeuze > 9 & stemkeuze <19 ~ "Anders",
           TRUE ~ NA_character_)) %>%
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> b


#NKO2006  

NKO2006 %>% 
  select(weights = wgt4,
         stem = V510,
         stemkeuze = V512,
         opv_integratie = V185,
         opv_asielzoekers = V155,
         opv_inkomen = V145) %>% 
  mutate(partij = case_when(
    stemkeuze == 1 ~ "CDA",
    stemkeuze == 2 ~ "PvdA",
    stemkeuze == 3 ~ "VVD",
    stemkeuze == 4 ~ "GL",
    stemkeuze == 5 ~ "SP",
    stemkeuze == 6 ~ "D66",
    stemkeuze == 7 ~ "CU",
    stemkeuze == 8 ~ "SGP",
    stemkeuze == 9 ~ "LPF",
    stemkeuze == 10 ~ "PVV",
    stemkeuze == 11 ~ "PvdD",
    stemkeuze == 12 ~ "EenNL",
    stemkeuze == 13 ~ "PVV",
    stemkeuze == 14 ~ "PVN",
    stemkeuze >= 15 ~ "Anders"),
    jaar = "2006") %>%
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> c

#NKO 2010 

NKO2010 %>%
  select(weights = wgt4,
         stem =v510, #stemwelniet
         stemkeuze =v512,#partij
         opv_integratie = v176,
         opv_asielzoekers = v156,
         opv_inkomen = v146) %>%
  mutate(stem = ifelse(stem == 0, 2, 1)) %>% 
  mutate(
    partij = case_when(
      stemkeuze == 1 ~ "CDA",
      stemkeuze == 2 ~ "PvdA",
      stemkeuze == 3 ~ "VVD",
      stemkeuze == 4 ~ "GL",
      stemkeuze == 5 ~ "SP",
      stemkeuze == 6 ~ "D66",
      stemkeuze == 7 ~ "CU",
      stemkeuze == 8 ~ "SGP",
      stemkeuze == 9 ~ "PVV",
      stemkeuze == 10 ~ "PvdD",
      stemkeuze == 11 ~ "TON",
      stemkeuze >= 11 ~ "Anders"),
    jaar = "2010") %>% 
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> d


#NKO 2012
NKO2012 %>%
  select(weights = Wgt1c,
         stem = V210, #stemwelniet
         V212, #partij
         V359, #inkomen
         opv_integratie = V115,
         opv_inkomen = V106,
         opv_asielzoekers = V525) %>%
  mutate(
    partij = case_when(
      V212 == 1 ~ "CDA",
      V212 == 2 ~ "PvdA",
      V212 == 3 ~ "VVD",
      V212 == 4 ~ "GL",
      V212 == 5 ~ "SP",
      V212 == 6 ~ "D66",
      V212 == 7 ~ "CU",
      V212 == 8 ~ "SGP",
      V212 == 9 ~ "PVV",
      V212 == 10 ~ "PvdD",
      V212 == 11 ~ "DPK",
      V212 == 12 ~ "50+",
      V212 >= 13 ~ "Anders"),
    jaar = "2012") %>% 
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers, opv_inkomen) -> e

##NKO2017

NKO2017 %>% 
  select(weights =WgtK,
         stem = V160,
         V163,
         opv_integratie = V118, 
         opv_asielzoekers = S128,
         opv_inkomen = V098,
         opv_europa = V108
  ) %>%
  mutate(
    partij = case_when(
      V163 == 1 ~ "CDA",
      V163 == 2 ~ "PvdA",
      V163 == 3 ~ "VVD",
      V163 == 4 ~ "GL",
      V163 == 5 ~ "SP",
      V163 == 6 ~ "D66",
      V163 == 7 ~ "CU",
      V163 == 8 ~ "SGP",
      V163 == 9 ~ "PVV",
      V163 == 10 ~ "PvdD",
      V163 == 11 ~ "50+",
      V163 == 12 ~ "Denk",
      V163 == 13 ~ "VNL",
      V163 == 14 ~ "FvD",
      V163 >= 15 ~ "Anders"),
    jaar = "2017") %>%
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers,opv_inkomen) -> f

NKO2017 %>% 
  select(weights = WgtO, 
         stem = V160,
         V163,
         opv_integratie = V118, 
         opv_asielzoekers = S128,
         opv_inkomen = V098
  ) %>% 
  mutate(
    partij = case_when(
      V163 == 1 ~ "CDA",
      V163 == 2 ~ "PvdA",
      V163 == 3 ~ "VVD",
      V163 == 4 ~ "GL",
      V163 == 5 ~ "SP",
      V163 == 6 ~ "D66",
      V163 == 7 ~ "CU",
      V163 == 8 ~ "SGP",
      V163 == 9 ~ "PVV",
      V163 == 10 ~ "PvdD",
      V163 == 11 ~ "50+",
      V163 == 12 ~ "Denk",
      V163 == 13 ~ "VNL",
      V163 == 14 ~ "FvD",
      V163 >= 15 ~ "Anders"),
    jaar = "2017") %>%
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers,opv_inkomen) -> altweights_NKO2017



#NKO2021

NKO2021 %>% 
  select(weights = Wght_Vote,
         stem = V160,
         N92,
         V163,
         opv_integratie = V118, 
         opv_asielzoekers = S128,
         opv_inkomen = V098) %>%
  mutate(
    partij = case_when(
      V163 == 1 ~ "VVD",
      V163 == 2 ~ "PVV",
      V163 == 3 ~ "CDA",
      V163 == 4 ~ "D66",
      V163 == 5 ~ "GL",
      V163 == 6 ~ "SP",
      V163 == 7 ~ "PvdA",
      V163 == 8 ~ "CU",
      V163 == 9 ~ "PvdD",
      V163 == 10 ~ "50+",
      V163 == 11 ~ "SGP",
      V163 == 12 ~ "Denk",
      V163 == 13 ~ "FvD",
      V163 == 14 ~ "JA21",
      V163 == 15 ~ "Volt",
      V163 == 16 ~ "BBB",
      V163 == 17 ~ "BIJ1",
      V163 >= 18 ~ "Anders"),
    jaar = "2021") %>%
  select(weights, jaar, stem, partij, opv_integratie, opv_asielzoekers,opv_inkomen) -> g


remove_all_labels(a) -> a
remove_all_labels(b) -> b
remove_all_labels(c) -> c
remove_all_labels(d) -> d
remove_all_labels(e) -> e
remove_all_labels(f) -> f
remove_all_labels(g) -> g
remove_all_labels(h) -> h
remove_all_labels(i) -> i
remove_all_labels(j) -> j

all_NKO <- rbind(a, b, c, d, e, f, g ,h, i, j) 
all_NKO_altweights <- rbind(a,b,c,d,e,altweights_NKO2017,g,h,i,j) ## Voor 2017 is de vraag over asielzoekers alleen in post-election survey gesteld, dus moeten er andere weights worden gebruikt voor asielzoekersvraag
