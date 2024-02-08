setwd('/Users/jessefrederik/Documents/GitHub/Rechtspopulisme')

source('NKO_inladen.R')
source('corriethema.R')

## Main grafiek in het stuk: opvattingen over asielzoekers verdeeld

all_NKO_altweights %>%
  mutate(partij = ifelse(stem == 2, "Niet gestemd", partij)) %>%
  filter(!is.na(opv_asielzoekers),
         weights > 0,!is.na(partij)) %>%
  group_by(jaar) %>%
  mutate(
    partijkleur = case_when(
      partij == "Niet gestemd" ~ "Niet gestemd",
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
    ),
    opv_asielzoekers = weighted_ntile(opv_asielzoekers, weights = weights, n = 4)
  ) %>%
  group_by(jaar, opv_asielzoekers, partijkleur) %>%
  summarise(weights = sum(weights, na.rm = T)) %>%
  group_by(jaar, opv_asielzoekers) %>%
  mutate(prop = round(weights / sum(weights) * 100, 2)) %>%
  arrange(jaar, opv_asielzoekers) %>%
  #Arrange levels: Niet gestemd	Rechtspopulistisch	VVD	Links	CDA	Anders
  mutate(partijkleur = factor(partijkleur, levels = c("Niet gestemd", "Rechtspopulistisch", "VVD", "Links", "CDA", "Anders"))) %>%
  ggplot(aes(x = opv_asielzoekers, y = prop, fill = partijkleur)) +
  geom_col() +
  corriethema +
  facet_wrap( ~ jaar) +
  # theme(legend.position = "none") +
  ## fill in the party colors of partijkleur
  scale_fill_manual(values = corriepallet) +
  labs(title = "Wie wint de anti-asielzoekersstem?",
       subtitle = "Het aandeel van de stemmen, per kwintiel van anti-asielzoekersentiment",
       caption = "Bron: Nationaal Kiezersonderzoek (1994-2021)")


## Bonus: zelfde grafiek voor nivelleringsstandpunt

all_NKO %>%
  mutate(partij = ifelse(stem == 2, "Niet gestemd", partij)) %>%
  filter(!is.na(opv_inkomen),
         weights > 0,!is.na(partij)) %>%
  group_by(jaar) %>%
  mutate(
    partijkleur = case_when(
      partij == "Niet gestemd" ~ "Niet gestemd",
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
    ),
    opv_inkomen = weighted_ntile(opv_inkomen, weights = weights, n = 4)
  ) %>%
  group_by(jaar, opv_inkomen, partijkleur) %>%
  summarise(weights = sum(weights, na.rm = T)) %>%
  group_by(jaar, opv_inkomen) %>%
  mutate(prop = round(weights / sum(weights) * 100, 2)) %>%
  arrange(jaar, opv_inkomen) %>%
  #Arrange levels: Niet gestemd	Rechtspopulistisch	VVD	Links	CDA	Anders
  mutate(partijkleur = factor(partijkleur, levels = c("Niet gestemd", "Rechtspopulistisch", "VVD", "Links", "CDA", "Anders"))) %>%
  ggplot(aes(x = opv_inkomen, y = prop, fill = partijkleur)) +
  geom_col() +
  corriethema +
  facet_wrap( ~ jaar) +
  # theme(legend.position = "none") +
  ## fill in the party colors of partijkleur
  scale_fill_manual(values = corriepallet) +
  labs(title = "Wie wint de stem van mensen die voor herverdeling zijn?",
       subtitle = "Het aandeel van de stemmen, per kwintiel van integratie",
       caption = "Bron: Nationaal Kiezersonderzoek (1994-2021)")