library(tidyverse)
library(haven)
library(labelled)
library(srvyr)
library(sjlabelled)

rm(list = ls())
setwd('/Users/jessefrederik/Documents/GitHub/LISS Panel nativisme/')
source('corriethema.R')

# List all .sav files in the directory, excluding subdirectories
files <-
  list.files('datafiles/', full.names = TRUE, pattern = "\\.sav$")

# Loop through each file
for (i in files) {
  # Extract the base file name
  base_name <- basename(i)
  
  # Use the first four characters of the base file name as the variable name
  var_name <- substr(base_name, 1, 4)
  
  # Read the .sav file and assign it to the new variable name
  assign(var_name, read_sav(i))
}


## put a "cv_background_" in front of all dataframes
for (i in list.files('datafiles/background')) {
  new_varname <- paste0('cv_background_', substr(i, 7, 10))
  assign(new_varname, read_sav(paste0('datafiles/background/', i)))
}

cv08 %>%
  merge(cv_background_2007, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    stem_past = cv08a053,
    vote_past = cv08a054,
    vote_now = cv08a058,
    symp_wilders = cv08a085,
    opv_integratie = cv08a104,
    opv_verschilculturen = cv08a116,
    opv_asielzoekers = cv08a118,
    opv_socialezekerheid = cv08a119,
    opv_buitenlanders = cv08a120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2006,
    vote_past = case_when(
      vote_past == 1 ~ "CDA",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "VVD",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "LPF",
      vote_past == 7 ~ "D66",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "Eén NL",
      vote_past == 11 ~ "PVV",
      vote_past == 12 ~ "PvdD",
      vote_past == 13 ~ "Andere partij",
      vote_past == 14 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "CDA",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "VVD",
      vote_now == 6 ~ "SP",
      vote_now == 7 ~ "GL",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "SGP",
      vote_now == 11 ~ "TON",
      vote_now == 12 ~ "PVV",
      vote_now == 13 ~ "PvdD",
      vote_now == 14 ~ "Andere partij",
      vote_now == 15 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2008)  -> cv08_filtered


cv09 %>%
  merge(cv_background_2008, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    stem_past = cv09b053,
    vote_past = cv09b054,
    vote_now = cv09b058,
    symp_wilders = cv09b085,
    opv_integratie = cv09b104,
    opv_verschilculturen = cv09b116,
    opv_asielzoekers = cv09b118,
    opv_socialezekerheid = cv09b119,
    opv_buitenlanders = cv09b120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2006,
    vote_past = case_when(
      vote_past == 1 ~ "CDA",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "VVD",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "LPF",
      vote_past == 7 ~ "D66",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "Eén NL",
      vote_past == 11 ~ "PVV",
      vote_past == 12 ~ "PvdD",
      vote_past == 13 ~ "Andere partij",
      vote_past == 14 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "CDA",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "VVD",
      vote_now == 6 ~ "SP",
      vote_now == 7 ~ "GL",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "SGP",
      vote_now == 11 ~ "TON",
      vote_now == 12 ~ "PVV",
      vote_now == 13 ~ "PvdD",
      vote_now == 14 ~ "Andere partij",
      vote_now == 15 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2009)   -> cv09_filtered

cv10  %>%
  merge(cv_background_2009, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    stem_past = cv10c053,
    vote_past = cv10c054,
    vote_now = cv10c058,
    symp_wilders = cv10c085,
    opv_integratie = cv10c104,
    opv_verschilculturen = cv10c116,
    opv_asielzoekers = cv10c118,
    opv_socialezekerheid = cv10c119,
    opv_buitenlanders = cv10c120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2006,
    vote_past = case_when(
      vote_past == 1 ~ "CDA",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "VVD",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "LPF",
      vote_past == 7 ~ "D66",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "Eén NL",
      vote_past == 11 ~ "PVV",
      vote_past == 12 ~ "PvdD",
      vote_past == 13 ~ "Andere partij",
      vote_past == 14 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "CDA",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "VVD",
      vote_now == 6 ~ "SP",
      vote_now == 7 ~ "GL",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "SGP",
      vote_now == 11 ~ "TON",
      vote_now == 12 ~ "PVV",
      vote_now == 13 ~ "PvdD",
      vote_now == 14 ~ "Andere partij",
      vote_now == 15 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2010) -> cv10_filtered

cv11  %>%
  merge(cv_background_2010, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv11d171,
    stem_past = cv11d053,
    vote_past = cv11d169,
    symp_wilders = cv11d176,
    opv_integratie = cv11d104,
    opv_verschilculturen = cv11d116,
    opv_asielzoekers = cv11d118,
    opv_socialezekerheid = cv11d119,
    opv_buitenlanders = cv11d120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2010,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "PVV",
      vote_past == 4 ~ "CDA",
      vote_past == 5 ~ "SP",
      vote_past == 6 ~ "D66",
      vote_past == 7 ~ "GL",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "PvdD",
      vote_past == 11 ~ "Andere partij",
      vote_past == 12 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "VVD",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "PVV",
      vote_now == 6 ~ "CDA",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "GL",
      vote_now == 10 ~ "CU",
      vote_now == 11 ~ "SGP",
      vote_now == 12 ~ "PvdD",
      vote_now == 13 ~ "Andere partij",
      vote_now == 14 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2011)  -> cv11_filtered

cv12 %>%
  merge(cv_background_2011, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv12e171,
    vote_past = cv12e169,
    stem_past = cv12e053,
    symp_wilders = cv12e176,
    opv_integratie = cv12e104,
    opv_verschilculturen = cv12e116,
    opv_asielzoekers = cv12e118,
    opv_socialezekerheid = cv12e119,
    opv_buitenlanders = cv12e120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2010,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "PVV",
      vote_past == 4 ~ "CDA",
      vote_past == 5 ~ "SP",
      vote_past == 6 ~ "D66",
      vote_past == 7 ~ "GL",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "PvdD",
      vote_past == 11 ~ "Andere partij",
      vote_past == 12 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "VVD",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "PVV",
      vote_now == 6 ~ "CDA",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "GL",
      vote_now == 10 ~ "CU",
      vote_now == 11 ~ "SGP",
      vote_now == 12 ~ "PvdD",
      vote_now == 13 ~ "Andere partij",
      vote_now == 14 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2012)  -> cv12_filtered


cv13 %>%
  merge(cv_background_2012, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv13f209,
    vote_past = cv13f207,
    stem_past = cv13f053,
    symp_wilders = cv13f213,
    opv_integratie = cv13f104,
    opv_verschilculturen = cv13f116,
    opv_asielzoekers = cv13f118,
    opv_socialezekerheid = cv13f119,
    opv_buitenlanders = cv13f120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2012,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "PVV",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "CDA",
      vote_past == 6 ~ "D66",
      vote_past == 7 ~ "GL",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "PvdD",
      vote_past == 11 ~ "50+",
      vote_past == 12 ~ "Andere partij",
      vote_past == 13 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "VVD",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "PVV",
      vote_now == 6 ~ "SP",
      vote_now == 7 ~ "CDA",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "GL",
      vote_now == 11 ~ "SGP",
      vote_now == 12 ~ "PvdD",
      vote_now == 13 ~ "50+",
      vote_now == 14 ~ "Andere partij",
      vote_now == 15 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2013)  -> cv13_filtered

cv14 %>%
  merge(cv_background_2013, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv14g209,
    vote_past = cv14g207,
    stem_past = cv14g053,
    symp_wilders = cv14g213,
    opv_integratie = cv14g104,
    opv_verschilculturen = cv14g116,
    opv_asielzoekers = cv14g118,
    opv_socialezekerheid = cv14g119,
    opv_buitenlanders = cv14g120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2012,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "PVV",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "CDA",
      vote_past == 6 ~ "D66",
      vote_past == 7 ~ "GL",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "PvdD",
      vote_past == 11 ~ "50+",
      vote_past == 12 ~ "Andere partij",
      vote_past == 13 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "VVD",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "PVV",
      vote_now == 6 ~ "SP",
      vote_now == 7 ~ "CDA",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "GL",
      vote_now == 11 ~ "SGP",
      vote_now == 12 ~ "PvdD",
      vote_now == 13 ~ "50+",
      vote_now == 14 ~ "Andere partij",
      vote_now == 15 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2014)   -> cv14_filtered

cv16 %>%
  merge(cv_background_2015, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv16h209,
    vote_past = cv16h207,
    stem_past = cv16h053,
    symp_wilders = cv16h213,
    opv_integratie = cv16h104,
    opv_verschilculturen = cv16h116,
    opv_asielzoekers = cv16h118,
    opv_socialezekerheid = cv16h119,
    opv_buitenlanders = cv16h120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2012,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "PVV",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "CDA",
      vote_past == 6 ~ "D66",
      vote_past == 7 ~ "GL",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "PvdD",
      vote_past == 11 ~ "50+",
      vote_past == 12 ~ "Andere partij",
      vote_past == 13 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Geen stem",
      vote_now == 2 ~ "Niet stemgerechtigd",
      vote_now == 3 ~ "VVD",
      vote_now == 4 ~ "PvdA",
      vote_now == 5 ~ "PVV",
      vote_now == 6 ~ "SP",
      vote_now == 7 ~ "CDA",
      vote_now == 8 ~ "D66",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "GL",
      vote_now == 11 ~ "SGP",
      vote_now == 12 ~ "PvdD",
      vote_now == 13 ~ "50+",
      vote_now == 14 ~ "Andere partij",
      vote_now == 15 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2016)   -> cv16_filtered

cv17 %>%
  merge(cv_background_2016, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv17i244,
    stem_past = cv17i053,
    vote_past = cv17i207,
    symp_wilders = cv17i213,
    opv_integratie = cv17i104,
    opv_verschilculturen = cv17i116,
    opv_asielzoekers = cv17i118,
    opv_socialezekerheid = cv17i119,
    opv_buitenlanders = cv17i120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2012,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PvdA",
      vote_past == 3 ~ "PVV",
      vote_past == 4 ~ "SP",
      vote_past == 5 ~ "CDA",
      vote_past == 6 ~ "D66",
      vote_past == 7 ~ "GL",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "SGP",
      vote_past == 10 ~ "PvdD",
      vote_past == 11 ~ "50+",
      vote_past == 12 ~ "Andere partij",
      vote_past == 13 ~ "Blanco",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PvdA",
      vote_now == 4 ~ "PVV",
      vote_now == 5 ~ "SP",
      vote_now == 6 ~ "CDA",
      vote_now == 7 ~ "D66",
      vote_now == 8 ~ "CU",
      vote_now == 9 ~ "GL",
      vote_now == 10 ~ "SGP",
      vote_now == 11 ~ "PvdD",
      vote_now == 12 ~ "50+",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "VNL",
      vote_now == 15 ~ "Andere partij",
      vote_now == 16 ~ "Blanco",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
      
    )
  ) %>%
  mutate(jaar = 2017)   -> cv17_filtered



cv18 %>%
  merge(cv_background_2017, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv18j308,
    stem_past = cv18j053,
    vote_past = cv18j307,
    symp_wilders = cv18j213,
    opv_integratie = cv18j104,
    opv_verschilculturen = cv18j116,
    opv_asielzoekers = cv18j118,
    opv_socialezekerheid = cv18j119,
    opv_buitenlanders = cv18j120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2017,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PVV",
      vote_past == 3 ~ "CDA",
      vote_past == 4 ~ "D66",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "SP",
      vote_past == 7 ~ "PvdA",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "PvdD",
      vote_past == 10 ~ "50+",
      vote_past == 11 ~ "SGP",
      vote_past == 12 ~ "DENK",
      vote_past == 13 ~ "FvD",
      vote_past == 14 ~ "Blanco",
      vote_past == 15 ~ "Andere partij",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PVV",
      vote_now == 4 ~ "CDA",
      vote_now == 5 ~ "D66",
      vote_now == 6 ~ "GL",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "PvdA",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "PvdD",
      vote_now == 11 ~ "50+",
      vote_now == 12 ~ "SGP",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "FvD",
      vote_now == 15 ~ "Blanco",
      vote_now == 16 ~ "Andere partij",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
      
    )
  ) %>%
  mutate(jaar = 2018)   -> cv18_filtered

cv19 %>%
  merge(cv_background_2018, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv19k308,
    vote_past = cv19k307,
    stem_past = cv19k053,
    symp_wilders = cv19k213,
    opv_integratie = cv19k104,
    opv_verschilculturen = cv19k116,
    opv_asielzoekers = cv19k118,
    opv_socialezekerheid = cv19k119,
    opv_buitenlanders = cv19k120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2017,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PVV",
      vote_past == 3 ~ "CDA",
      vote_past == 4 ~ "D66",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "SP",
      vote_past == 7 ~ "PvdA",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "PvdD",
      vote_past == 10 ~ "50+",
      vote_past == 11 ~ "SGP",
      vote_past == 12 ~ "DENK",
      vote_past == 13 ~ "FvD",
      vote_past == 14 ~ "Blanco",
      vote_past == 15 ~ "Andere partij",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PVV",
      vote_now == 4 ~ "CDA",
      vote_now == 5 ~ "D66",
      vote_now == 6 ~ "GL",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "PvdA",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "PvdD",
      vote_now == 11 ~ "50+",
      vote_now == 12 ~ "SGP",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "FvD",
      vote_now == 15 ~ "Blanco",
      vote_now == 16 ~ "Andere partij",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
      
    )
  ) %>%
  mutate(jaar = 2019) -> cv19_filtered

cv20 %>%
  merge(cv_background_2019, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv20l308,
    vote_past = cv20l307,
    stem_past = cv20l053,
    symp_wilders = cv20l213,
    opv_integratie = cv20l104,
    opv_verschilculturen = cv20l116,
    opv_asielzoekers = cv20l118,
    opv_socialezekerheid = cv20l119,
    opv_buitenlanders = cv20l120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2017,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PVV",
      vote_past == 3 ~ "CDA",
      vote_past == 4 ~ "D66",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "SP",
      vote_past == 7 ~ "PvdA",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "PvdD",
      vote_past == 10 ~ "50+",
      vote_past == 11 ~ "SGP",
      vote_past == 12 ~ "DENK",
      vote_past == 13 ~ "FvD",
      vote_past == 14 ~ "Blanco",
      vote_past == 15 ~ "Andere partij",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PVV",
      vote_now == 4 ~ "CDA",
      vote_now == 5 ~ "D66",
      vote_now == 6 ~ "GL",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "PvdA",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "PvdD",
      vote_now == 11 ~ "50+",
      vote_now == 12 ~ "SGP",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "FvD",
      vote_now == 15 ~ "Blanco",
      vote_now == 16 ~ "Andere partij",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
      
    )
  ) %>%
  mutate(jaar = 2020)   -> cv20_filtered



cv21 %>%
  merge(cv_background_2020, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv21m308,
    vote_past = cv21m307,
    stem_past = cv21m053,
    symp_wilders = cv21m213,
    opv_integratie = cv21m104,
    opv_verschilculturen = cv21m116,
    opv_asielzoekers = cv21m118,
    opv_socialezekerheid = cv21m119,
    opv_buitenlanders = cv21m120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2017,
    vote_past = case_when(
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PVV",
      vote_past == 3 ~ "CDA",
      vote_past == 4 ~ "D66",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "SP",
      vote_past == 7 ~ "PvdA",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "PvdD",
      vote_past == 10 ~ "50+",
      vote_past == 11 ~ "SGP",
      vote_past == 12 ~ "DENK",
      vote_past == 13 ~ "FvD",
      vote_past == 14 ~ "Blanco",
      vote_past == 15 ~ "Andere partij",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PVV",
      vote_now == 4 ~ "CDA",
      vote_now == 5 ~ "D66",
      vote_now == 6 ~ "GL",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "PvdA",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "PvdD",
      vote_now == 11 ~ "50+",
      vote_now == 12 ~ "SGP",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "FvD",
      vote_now == 15 ~ "Blanco",
      vote_now == 16 ~ "Andere partij",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2021)   -> cv21_filtered


cv22 %>%
  merge(cv_background_2021, by = 'nomem_encr')  %>%
  select(
    nomem_encr,
    vote_now = cv22n308,
    vote_past = cv22n307,
    stem_past = cv22n053,
    symp_wilders = cv22n213,
    opv_integratie = cv22n104,
    opv_verschilculturen = cv22n116,
    opv_asielzoekers = cv22n118,
    opv_socialezekerheid = cv22n119,
    opv_buitenlanders = cv22n120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2021,
    vote_past = case_when(
      vote_past == -9 ~ "Weet niet",
      vote_past == -8 ~ "Geen antwoord",
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PVV",
      vote_past == 3 ~ "CDA",
      vote_past == 4 ~ "D66",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "SP",
      vote_past == 7 ~ "PvdA",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "PvdD",
      vote_past == 10 ~ "50+",
      vote_past == 11 ~ "SGP",
      vote_past == 12 ~ "DENK",
      vote_past == 13 ~ "FvD",
      vote_past == 14 ~ "Blanco",
      vote_past == 15 ~ "Andere partij",
      vote_past == 16 ~ "Volt",
      vote_past == 17 ~ "JA21",
      vote_past == 18 ~ "BBB",
      vote_past == 19 ~ "BIJ1",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PVV",
      vote_now == 4 ~ "CDA",
      vote_now == 5 ~ "D66",
      vote_now == 6 ~ "GL",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "PvdA",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "PvdD",
      vote_now == 11 ~ "50+",
      vote_now == 12 ~ "SGP",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "FvD",
      vote_now == 15 ~ "Blanco",
      vote_now == 16 ~ "Andere partij",
      vote_now == 17 ~ "Volt",
      vote_now == 18 ~ "JA21",
      vote_now == 19 ~ "BBB",
      vote_now == 20 ~ "BIJ1",
      vote_now == 998 ~ "Geen antwoord",
      vote_now == 999 ~ "Weet niet"
      
    )
  ) %>%
  mutate(jaar = 2022) -> cv22_filtered


cv23 %>%
  merge(cv_background_2022, by = 'nomem_encr') %>%
  select(
    nomem_encr,
    vote_now = cv23o308,
    vote_past = cv23o307,
    stem_past = cv23o053,
    symp_wilders = cv23o213,
    opv_integratie = cv23o104,
    opv_verschilculturen = cv23o116,
    opv_asielzoekers = cv23o118,
    opv_socialezekerheid = cv23o119,
    opv_buitenlanders = cv23o120,
    belbezig,
    brutoink,
    nettoink,
    oplzon,
    oplmet,
    oplcat,
    geslacht,
    leeftijd
  ) %>%
  mutate(
    verkiezingen = 2021,
    vote_past = case_when(
      vote_past == -9 ~ "Weet niet",
      vote_past == -8 ~ "Geen antwoord",
      vote_past == 1 ~ "VVD",
      vote_past == 2 ~ "PVV",
      vote_past == 3 ~ "CDA",
      vote_past == 4 ~ "D66",
      vote_past == 5 ~ "GL",
      vote_past == 6 ~ "SP",
      vote_past == 7 ~ "PvdA",
      vote_past == 8 ~ "CU",
      vote_past == 9 ~ "PvdD",
      vote_past == 10 ~ "50+",
      vote_past == 11 ~ "SGP",
      vote_past == 12 ~ "DENK",
      vote_past == 13 ~ "FvD",
      vote_past == 14 ~ "Blanco",
      vote_past == 15 ~ "Andere partij",
      vote_past == 16 ~ "Volt",
      vote_past == 17 ~ "JA21",
      vote_past == 18 ~ "BBB",
      vote_past == 19 ~ "BIJ1",
      vote_past == 998 ~ "Geen antwoord",
      vote_past == 999 ~ "Weet niet"
    ),
    vote_now = case_when(
      vote_now == 1 ~ "Niet stemmen",
      vote_now == 2 ~ "VVD",
      vote_now == 3 ~ "PVV",
      vote_now == 4 ~ "CDA",
      vote_now == 5 ~ "D66",
      vote_now == 6 ~ "GL",
      vote_now == 7 ~ "SP",
      vote_now == 8 ~ "PvdA",
      vote_now == 9 ~ "CU",
      vote_now == 10 ~ "PvdD",
      vote_now == 11 ~ "50+",
      vote_now == 12 ~ "SGP",
      vote_now == 13 ~ "DENK",
      vote_now == 14 ~ "FvD",
      vote_now == 15 ~ "Blanco",
      vote_now == 16 ~ "Andere partij",
      vote_now == 17 ~ "VN",
      vote_now == 18 ~ "JA21",
      vote_now == 19 ~ "BBB",
      vote_now == 20 ~ "BIJ1",
      vote_now == -8 ~ "Geen antwoord",
      vote_now == -9 ~ "Weet niet"
    )
  ) %>%
  mutate(jaar = 2023)  -> cv23_filtered


df_names <-
  c(
    paste0("cv0", 8:9, "_filtered"),
    paste0("cv", 10:14, "_filtered"),
    paste0("cv", 16:23, "_filtered")
  )

# Remove labels and update dataframes
for (name in df_names) {
  if (exists(name)) {
    assign(name, remove_all_labels(get(name)))
  }
}

# Merge all dataframes, only including those that exist
cv_filtered <-
  Reduce(function(x, y)
    merge(x, y, all = TRUE), mget(df_names[df_names %in% ls()]))


#change the order of the variables opv_asielzoekers, opv_socialezekerheid and opv_verschilculturen so that 1 = 5, 2 =4 and 3 = 3
cv_filtered_def <- cv_filtered %>%
  mutate(
    opv_asielzoekers = recode(
      opv_asielzoekers,
      `1` = 5,
      `2` = 4,
      `3` = 3,
      `4` = 2,
      `5` = 1
    ),
    opv_socialezekerheid = recode(
      opv_socialezekerheid,
      `1` = 5,
      `2` = 4,
      `3` = 3,
      `4` = 2,
      `5` = 1
    ),
    opv_verschilculturen = recode(
      opv_verschilculturen,
      `1` = 5,
      `2` = 4,
      `3` = 3,
      `4` = 2,
      `5` = 1
    )
  ) %>%
  
  filter(
    !is.na(opv_asielzoekers),
    !is.na(opv_socialezekerheid),
    !is.na(opv_verschilculturen),
    !is.na(opv_buitenlanders),
    !is.na(opv_verschilculturen),
    !is.na(opv_integratie)
  ) %>%
  #create a new variable that is the average of the 5 variables
  mutate(
    etnic_scale = (((
      opv_verschilculturen + opv_asielzoekers + opv_socialezekerheid + opv_buitenlanders + opv_integratie
    ) / 5))) %>% 
  #sort by nomem_encr and then ascending year
  arrange(nomem_encr, jaar) %>%
  mutate(rowid = row_number()) 