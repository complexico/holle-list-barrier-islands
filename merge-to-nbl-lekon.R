# The code to join Lekon list with New Basic List (NBL)
# DOI for digitised NBL: https://doi.org/10.25446/oxford.23205173

library(tidyverse)
library(readxl)

source("merge-NBL-call.R") # run codes to retrieve the NBL and NBL's Concepticon mapping

lekon_words <- read_lines("plaintexts/lekon.txt")

# Processing the word list =====
wlist_tags <- str_which(lekon_words, "wordlist\\>")
lekon_wlist <- lekon_words[(wlist_tags[1]+1):(wlist_tags[2]-1)]
lekon_words_df <- tibble(lekon = lekon_wlist)
lekon_words_df <- lekon_words_df |> 
  mutate(Index = str_extract(lekon, "^[^ ]+(?=\\.\\s)"),
         Forms = str_replace_all(lekon, "(^[^ ]+\\.\\s|\\s?\\<[^>]+\\>)", ""),
         Notes_id = str_extract_all(lekon, "(?<=\\<)([^>]+?)(?=\\>)")) |> 
  unnest_longer(Notes_id, keep_empty = TRUE) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) # |> 
  # mutate(Forms = str_split(Forms, "\\,\\s?")) |> 
  # unnest_longer(Forms)

# check which Index in Lekon is not in the Index of the NBL
id_lekon_not_in_nbl <- which(!lekon_words_df$Index %in% holle_tb$Index)
lekon_words_df$Index[id_lekon_not_in_nbl]
# [1] "1080/1081" (the correct id in NBL is "1080-1081")
## fix the index
lekon_words_df <- lekon_words_df |> 
  mutate(Index = replace(Index, Index == lekon_words_df$Index[id_lekon_not_in_nbl], "1080-1081"))
## again check which Index in Lekon is not in the Index of the NBL
id_lekon_not_in_nbl <- which(!lekon_words_df$Index %in% holle_tb$Index)
lekon_words_df$Index[id_lekon_not_in_nbl]
# character(0)

# Processing the notes ====
notes_tags <- str_which(lekon_words, "\\<\\/?notes\\>")
lekon_notes <- lekon_words[(notes_tags[1]+1):(notes_tags[2]-1)]
# check the notes tag available
lekon_notes |> str_extract_all("\\<[^<\\/]+?\\>") |> unlist() |> unique()
# [1] "<n>"       "<eng>"     "<form>"    "<comment>" "<idn>"     "<xr>"      "<ptr>"     "<tapah>"
lekon_notes_df <- tibble(lekon_notes)
lekon_notes_df <- lekon_notes_df |> 
  mutate(Notes_id = str_extract(lekon_notes, "^[^ ]+?(?=\\.\\s)")) |> 
  mutate(notes = str_replace(lekon_notes, "^[^ ]+\\.\\s", "")) |> 
  mutate(notes = str_split(notes, "\\s?\\;\\s")) |> 
  unnest_longer(notes, keep_empty = TRUE) |> 
  select(-lekon_notes) |> 
  mutate(nt_form = str_extract(notes, "(?<=\\<form\\>)([^<]+?)(?=\\<\\/form\\>)"),
         nt_english = str_extract(notes, "(?<=\\<eng\\>)([^<]+?)(?=\\<\\/eng\\>)"),
         nt_idn = str_extract(notes, "(?<=\\<idn\\>)([^<]+?)(?=\\<\\/idn\\>)"),
         nt_tapah = str_extract(notes, "(?<=\\<tapah\\>)([^<]+?)(?=\\<\\/tapah\\>)"),
         nt_tapah = str_replace(nt_tapah, "^T\\s", ""),
         nt_comment = str_extract(notes, "(?<=\\<comment\\>)([^<]+?)(?=\\<\\/comment\\>)"),
         nt_xref = str_extract(notes, "(?<=\\<xr\\>)(.+?)(?=\\<\\/xr\\>)"),
         nt_xref = str_replace_all(nt_xref, "\\<\\/?ptr\\>", "")) |> 
  select(-notes) |> 
  mutate(across(where(is.character), ~replace_na(., "")))

# join the NBL =====
tb <- lekon_words_df |> 
  left_join(holle_tb)

# join word list and the notes =====
tb <- tb |> 
  left_join(lekon_notes_df) |> 
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
  mutate(Forms = if_else(str_detect(nt_form, "^inang.+tiri"),
                         str_replace(Forms, "^among.+?(?=inang)", ""),
                         Forms),
         Forms = if_else(str_detect(nt_form, "^amang.+tiri"),
                         str_replace(Forms, "\\,\\sinang.+tiri", ""),
                         Forms),
         Forms = if_else(str_detect(nt_form, "bolêm ònding"),
                         str_replace(Forms, "bòlêm itêm\\,\\s", ""),
                         Forms),
         Forms = if_else(str_detect(nt_form, "bolêm itêm"),
                         str_replace(Forms, ",\\sbòlêm ònding", ""),
                         Forms)) |> 
  # add from note form the main Form that originally is empty/given note ID only
  mutate(Forms = if_else(Forms == "" & nt_form != "",
                         nt_form,
                         Forms)) |> 
  distinct()

write_tsv(tb, "data-output/lekon_tb.tsv")
