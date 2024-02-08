source('NKO_inladen.R')
source('corriethema.R')

all_NKO_altweights %>%
  filter(stem == 1) %>%
  group_by(jaar) %>%
  
  mutate(
    partijkleur = case_when(
      partij == "CDA" ~ "CDA",
      partij == "RPF" ~ "Links",
      partij == "GPV" ~ "Links",
      partij == "AOV, Unie 55+" ~ "Anders",
      partij == "other party" ~ "Anders",
      partij == "PvdA" ~ "Links",
      partij == "VVD"  ~ "VVD",
      partij == "GL"  ~ "Links",
      partij == "SP"  ~ "Links",
      partij == "D66"  ~ "Links",
      partij == "CU"  ~ "Links",
      partij == "SGP"  ~ "Anders",
      partij == "TON"  ~ "Rechtspopulistisch",
      partij == "PVV"  ~ "Rechtspopulistisch",
      partij == "PvdD"  ~ "Links",
      partij == "LPF"  ~ "Rechtspopulistisch",
      partij == "FvD"  ~ "Rechtspopulistisch",
      partij == "JA21"  ~ "Rechtspopulistisch",
      partij == "BBB"  ~ "Rechtspopulistisch",
      partij == "BIJ1"  ~ "Links",
      partij == "Volt"  ~ "Links",
      partij == "50+" ~ "Rechtspopulistisch",
      partij == "Denk"  ~ "Links",
      partij == "VNL"  ~ "Rechtspopulistisch",
      partij == "DPK"  ~ "Rechtspopulistisch",
      partij == "CD"  ~ "Rechtspopulistisch",
      partij == "Anders"  ~ "Anders",
      TRUE ~ "Anders"
    )
  ) %>%
  mutate(
    partij_2 = case_when(
      partij == "PvdA" ~ "PvdA",
      partij == "CDA" ~ "CDA",
      partij == "LPF" ~ "LPF",
      partij == "SP" ~ "SP",
      partij == "GL" ~ "GroenLinks",
      partij == "D66" ~ "D66",
      partij == "VVD" ~ "VVD",
      partij == "PVV" ~ "PVV",
      partij == "CD" ~ "CD",
      partij == "FvD" ~ "FvD",
      partij == "SGP" ~ "SGP",
      TRUE ~ "Anders"
      
    )
  ) %>%
  group_by(jaar) %>%
  filter(!is.na(opv_asielzoekers),
         weights > 0) %>%
  mutate(opv_asielzoekers = weighted_ntile(opv_asielzoekers, weights = weights, n = 4)) %>%
  group_by(jaar, partijkleur, partij_2, opv_asielzoekers) %>%
  summarise(weights = sum(weights, na.rm = T)) %>%
  group_by(jaar, opv_asielzoekers) %>%
  mutate(prop = weights / sum(weights)) %>%
  arrange(jaar, opv_asielzoekers) %>%
  filter(partijkleur != "Anders",
         jaar > 1980) %>%
  mutate(prop = round(prop * 100, 2)) %>%
  filter(partijkleur == "VVD") %>%
  #pivot_wider(names_from = partij_2, values_from = weights) %>%
  ggplot(aes(x = opv_asielzoekers, y = prop, fill = partij_2)) +
  geom_col() +
  corriethema +
  facet_wrap(~ jaar) +
  # theme(legend.position = "none") +
  ## fill in the party colors of partijkleur
  scale_fill_manual(values = corriepallet) +
  labs(title = "Wie wint de anti-asielzoekersstem (en wie niet)?",
       subtitle = "Het aandeel van de stemmen, per kwintiel van asielzoekersaversie",
       caption = "Bron: Nationaal Kiezersonderzoek (1994-2021)")


## Bonus: Hetzelfde alleen dan voor het standpunt over integratie (aanpassen of behoud van eigen cultuur)


all_NKO %>%
  filter(stem == 1) %>%
  group_by(jaar) %>%
  mutate(
    partijkleur = case_when(
      partij == "CDA" ~ "CDA",
      partij == "RPF" ~ "Links",
      partij == "GPV" ~ "Links",
      partij == "AOV, Unie 55+" ~ "Anders",
      partij == "other party" ~ "Anders",
      partij == "PvdA" ~ "Links",
      partij == "VVD"  ~ "VVD",
      partij == "GL"  ~ "Links",
      partij == "SP"  ~ "Links",
      partij == "D66"  ~ "Links",
      partij == "CU"  ~ "Links",
      partij == "SGP"  ~ "Anders",
      partij == "TON"  ~ "Rechtspopulistisch",
      partij == "PVV"  ~ "Rechtspopulistisch",
      partij == "PvdD"  ~ "Links",
      partij == "LPF"  ~ "Rechtspopulistisch",
      partij == "FvD"  ~ "Rechtspopulistisch",
      partij == "JA21"  ~ "Rechtspopulistisch",
      partij == "BBB"  ~ "Rechtspopulistisch",
      partij == "BIJ1"  ~ "Links",
      partij == "Volt"  ~ "Links",
      partij == "50+" ~ "Rechtspopulistisch",
      partij == "Denk"  ~ "Links",
      partij == "VNL"  ~ "Rechtspopulistisch",
      partij == "DPK"  ~ "Rechtspopulistisch",
      partij == "CD"  ~ "Rechtspopulistisch",
      partij == "Anders"  ~ "Anders",
      TRUE ~ "Anders"
    )
  ) %>%
  mutate(
    partij_2 = case_when(
      partij == "PvdA" ~ "PvdA",
      partij == "CDA" ~ "CDA",
      partij == "LPF" ~ "LPF",
      partij == "SP" ~ "SP",
      partij == "GL" ~ "GroenLinks",
      partij == "D66" ~ "D66",
      partij == "VVD" ~ "VVD",
      partij == "PVV" ~ "PVV",
      partij == "CD" ~ "CD",
      partij == "FvD" ~ "FvD",
      partij == "SGP" ~ "SGP",
      TRUE ~ "Anders"
      
    )
  ) %>%
  group_by(jaar) %>%
  filter(!is.na(opv_integratie),
         weights > 0) %>%
  mutate(opv_integratie = weighted_ntile(opv_integratie, weights = weights, n = 4)) %>%
  group_by(jaar, partijkleur, partij_2, opv_integratie) %>%
  summarise(weights = sum(weights, na.rm = T)) %>%
  group_by(jaar, opv_integratie) %>%
  mutate(prop = weights / sum(weights)) %>%
  arrange(jaar, opv_integratie) %>%
  filter(partijkleur != "Anders",
         jaar > 1980) %>%
  mutate(prop = round(prop * 100, 2)) %>%
  filter(partijkleur == "VVD") %>%
  #pivot_wider(names_from = partij_2, values_from = weights) %>%
  ggplot(aes(x = opv_integratie, y = prop, fill = partij_2)) +
  geom_col() +
  corriethema +
  facet_wrap(~ jaar) +
  # theme(legend.position = "none") +
  ## fill in the party colors of partijkleur
  scale_fill_manual(values = corriepallet) +
  labs(title = "Voor aanpassing of behoud van eigen cultuur, wie wint de nativistische stem",
       subtitle = "Het aandeel van de stemmen, per kwintiel van integratie",
       caption = "Bron: Nationaal Kiezersonderzoek (1994-2021)")


## Bonus: Standpunt over nivellering

all_NKO %>%
  filter(stem == 1) %>%
  group_by(jaar) %>%
  mutate(
    partijkleur = case_when(
      partij == "CDA" ~ "CDA",
      partij == "RPF" ~ "Links",
      partij == "GPV" ~ "Links",
      partij == "AOV, Unie 55+" ~ "Anders",
      partij == "other party" ~ "Anders",
      partij == "PvdA" ~ "Links",
      partij == "VVD"  ~ "VVD",
      partij == "GL"  ~ "Links",
      partij == "SP"  ~ "Links",
      partij == "D66"  ~ "Links",
      partij == "CU"  ~ "Links",
      partij == "SGP"  ~ "Anders",
      partij == "TON"  ~ "Rechtspopulistisch",
      partij == "PVV"  ~ "Rechtspopulistisch",
      partij == "PvdD"  ~ "Links",
      partij == "LPF"  ~ "Rechtspopulistisch",
      partij == "FvD"  ~ "Rechtspopulistisch",
      partij == "JA21"  ~ "Rechtspopulistisch",
      partij == "BBB"  ~ "Rechtspopulistisch",
      partij == "BIJ1"  ~ "Links",
      partij == "Volt"  ~ "Links",
      partij == "50+" ~ "Rechtspopulistisch",
      partij == "Denk"  ~ "Links",
      partij == "VNL"  ~ "Rechtspopulistisch",
      partij == "DPK"  ~ "Rechtspopulistisch",
      partij == "CD"  ~ "Rechtspopulistisch",
      partij == "Anders"  ~ "Anders",
      TRUE ~ "Anders"
    )
  ) %>%
  mutate(
    partij_2 = case_when(
      partij == "PvdA" ~ "PvdA",
      partij == "CDA" ~ "CDA",
      partij == "LPF" ~ "LPF",
      partij == "SP" ~ "SP",
      partij == "GL" ~ "GroenLinks",
      partij == "D66" ~ "D66",
      partij == "VVD" ~ "VVD",
      partij == "PVV" ~ "PVV",
      partij == "CD" ~ "CD",
      partij == "FvD" ~ "FvD",
      partij == "SGP" ~ "SGP",
      TRUE ~ "Anders"
      
    )
  ) %>%
  group_by(jaar) %>%
  filter(!is.na(opv_inkomen),
         weights > 0) %>%
  mutate(opv_inkomen = weighted_ntile(opv_inkomen, weights = weights, n = 4)) %>%
  group_by(jaar, partijkleur, partij_2, opv_inkomen) %>%
  summarise(weights = sum(weights, na.rm = T)) %>%
  group_by(jaar, opv_inkomen) %>%
  mutate(prop = weights / sum(weights)) %>%
  arrange(jaar, opv_inkomen) %>%
  filter(partijkleur != "Anders",
         jaar > 1980) %>%
  mutate(prop = round(prop * 100, 2)) %>%
  filter(partijkleur == "VVD") %>%
  #pivot_wider(names_from = partij_2, values_from = weights) %>%
  ggplot(aes(x = opv_inkomen, y = prop, fill = partij_2)) +
  geom_col() +
  corriethema +
  facet_wrap(~ jaar) +
  # theme(legend.position = "none") +
  ## fill in the party colors of partijkleur
  scale_fill_manual(values = corriepallet) +
  labs(title = "Wie wint de stem van mensen die voor herverdeling zijn?",
       subtitle = "Het aandeel van de stemmen, per kwintiel van integratie",
       caption = "Bron: Nationaal Kiezersonderzoek (1994-2021)")