library(tidyverse)

# source: https://github.com/engganolang/holle-list-enggano-1895/blob/main/code/Enggano-Holle-List-with-NBL.R
# Rajeg, Gede Primahadi Wijaya (2023). CLDF dataset of the Enggano word list from 1895 in Stokhof and Almanar’s (1987) Holle List. University of Oxford. Dataset. https://doi.org/10.25446/oxford.23515788.v2

# read the NBL table from GitHub =====
holle_tb <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/data/digitised-holle-list-in-stokhof-1980.tsv") |> 
  mutate(across(where(is.character), ~replace_na(., "")))

# read the Concepticon table for the NBL of the Holle List from GitHub ====
concepticon <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/main/data/concepticon-mapping.tsv") |> 
  rename(Index = NUMBER, 
         English = GLOSS,
         Concepticon_Gloss = CONCEPTICON_GLOSS) |> 
  select(-SIMILARITY) |> 
  mutate(concept_url = paste("https://concepticon.clld.org/parameters/",
                             CONCEPTICON_ID,
                             sep = ""))
concepticon_checked <- concepticon |> 
  filter(CHECKED == "y") |> 
  select(English, Index, Concepticon_Gloss, Concepticon_ID = CONCEPTICON_ID, concept_url) |> 
  mutate(Index = as.character(Index))
