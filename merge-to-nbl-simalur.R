# The code to join Simalur list with New Basic List (NBL)
# DOI for digitised NBL: https://doi.org/10.25446/oxford.23205173

library(tidyverse)
library(readxl)

source("merge-NBL-call.R") # run codes to retrieve the NBL and NBL's Concepticon mapping

simalur_words <- read_lines("plaintexts/simalur.txt")

# Processing the word list =====
wlist_tags <- str_which(simalur_words, "wlist\\>")
simalur_wlist <- simalur_words[(wlist_tags[1]+1):(wlist_tags[2]-1)]
simalur_words_df <- tibble(simalur = simalur_wlist)
simalur_words_df <- simalur_words_df |> 
  mutate(Index = str_extract(simalur, "^[^ ]+(?=\\.\\s)"),
         Forms = str_replace_all(simalur, "(^[^ ]+\\.\\s|\\s?\\<[^>]+\\>)", ""),
         Notes_id = str_extract_all(simalur, "(?<=\\<)([^>]+?)(?=\\>)")) |> 
  unnest_longer(Notes_id, keep_empty = TRUE) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) # |> 
# mutate(Forms = str_split(Forms, "\\,\\s?")) |> 
# unnest_longer(Forms)

# check which Index in Simalur is not in the Index of the NBL
id_simalur_not_in_nbl <- which(!simalur_words_df$Index %in% holle_tb$Index)
simalur_words_df$Index[id_simalur_not_in_nbl]
# [1] "742/744"   "1080/1081" (the correct IDs in NBL are "742-744" & "1080-1081)
## fix the index
simalur_words_df <- simalur_words_df |> 
  mutate(Index = replace(Index, Index == "742/744", "742-744"),
         Index = replace(Index, Index == "1080/1081", "1080-1081"))
## again check which Index in Simalur is not in the Index of the NBL
id_simalur_not_in_nbl <- which(!simalur_words_df$Index %in% holle_tb$Index)
simalur_words_df$Index[id_simalur_not_in_nbl]
# character(0)

# Processing the notes ====
notes_tags <- str_which(simalur_words, "\\<\\/?notes\\>")
simalur_notes <- simalur_words[(notes_tags[1]+1):(notes_tags[2]-1)]
# check the notes tag available
simalur_notes |> str_extract_all("\\<[^<\\/]+?\\>") |> unlist() |> unique()
# [1] "<n>"       "<eng>"     "<form>"    "<comment>"
simalur_notes_df <- tibble(simalur_notes)
simalur_notes_df <- simalur_notes_df |> 
  mutate(Notes_id = str_extract(simalur_notes, "^[^ ]+?(?=\\.\\s)")) |> 
  mutate(notes = str_replace(simalur_notes, "^[^ ]+\\.\\s", "")) |> 
  mutate(notes = str_split(notes, "\\s?\\;\\s")) |> 
  unnest_longer(notes, keep_empty = TRUE) |> 
  select(-simalur_notes) |> 
  mutate(nt_form = str_extract(notes, "(?<=\\<form\\>)([^<]+?)(?=\\<\\/form\\>)"),
         nt_english = str_extract(notes, "(?<=\\<eng\\>)([^<]+?)(?=\\<\\/eng\\>)"),
         nt_comment = str_extract(notes, "(?<=\\<comment\\>)([^<]+?)(?=\\<\\/comment\\>)")) |> 
  select(-notes) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

# join the NBL =====
tb <- simalur_words_df |> 
  left_join(holle_tb)

# join word list and the notes =====
tb <- tb |> 
  left_join(simalur_notes_df) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

# read the translated data & integrate with the main table ====
now_translated <- readxl::read_xlsx("data-raw/no-translation-now-translated.xlsx") |> 
  rename(Index = id,
         English_add = English,
         Indonesian_add = Indonesian) |> 
  select(-form,
         -Dutch)
no_english_id <- pull(filter(tb, English == ""), Index)
no_indonesian_id <- pull(filter(tb, Indonesian == ""), Index)
now_translated_eng <- now_translated |> 
  filter(Index %in% no_english_id) |> 
  select(-Indonesian_add)
now_translated_idn <- now_translated |> 
  filter(Index %in% no_indonesian_id) |> 
  select(-English_add)

# join the main database with the matching now-translated data ====
tb <- tb |> 
  left_join(now_translated_eng) |> 
  left_join(now_translated_idn) |> 
  mutate(English = if_else(!is.na(English_add),
                           English_add,
                           English),
         Indonesian = if_else(!is.na(Indonesian_add),
                              Indonesian_add,
                              Indonesian)) |> 
  select(-English_add, -Indonesian_add)

# Join Concepticon ====
tb <- tb |> 
  left_join(concepticon_checked) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

# Matching notes and forms for multiple forms and split forms in notes ====
## Highly customised, on a case-by-case basis!
tb <- tb |> 
  mutate(Forms = if_else(str_detect(Forms, "\\,\\s") & 
                           Notes_id != "" & 
                           str_detect(nt_form, "^toemoed"),
                         str_replace(Forms, "\\,\\sachoen$", ""),
                         Forms),
         Forms = if_else(str_detect(Forms, "\\,\\s") & 
                           Notes_id != "" & 
                           str_detect(nt_form, "^achoen"),
                         str_replace(Forms, "^toemoe.+\\,\\s", ""),
                         Forms),
         Forms = if_else(str_detect(Forms, "\\,\\s") & 
                           Notes_id != "" & 
                           str_detect(nt_form, "^nanga²"),
                         str_replace(Forms, "\\,\\snandong²", ""),
                         Forms),
         Forms = if_else(str_detect(Forms, "\\,\\s") & 
                           Notes_id != "" & 
                           str_detect(nt_form, "^nandong²"),
                         str_replace(Forms, "^.+\\,\\s", ""),
                         Forms)) |> 
  # add from note form the main Form that originally is empty/given note ID only
  mutate(Forms = if_else(Forms == "" & nt_form != "",
                         nt_form,
                         Forms)) |> 
  distinct()

write_tsv(tb, "data-output/simalur_tb.tsv")