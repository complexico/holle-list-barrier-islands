# The code to join Mentawai (1933) list with New Basic List (NBL)
# DOI for digitised NBL: https://doi.org/10.25446/oxford.23205173

library(tidyverse)
library(readxl)

source("merge-NBL-call.R") # run codes to retrieve the NBL and NBL's Concepticon mapping
holle_1931 <- read_tsv("https://raw.githubusercontent.com/engganolang/digitised-holle-list/refs/heads/main/data/digitised-holle-list-in-stokhof-1980-add-1931.tsv") |> 
  mutate(across(where(is.character), ~replace_na(., "")))

mtw33_words <- read_lines("plaintexts/mentawai1933.txt")
source("gloss-English-and-Indonesian-added-to-unsp-Dutch.R")

# Processing metadata ====
metadata_tag <- str_which(mtw33_words, "metadata\\>")
metadata <- mtw33_words[(metadata_tag[1]+1):(metadata_tag[2]-1)]
gen_info <- metadata |> 
  tibble() |> 
  separate_wider_delim(metadata, 
                       delim = "\\t", 
                       names = c("query", "response")) |> 
  separate_longer_delim(response, regex("\\s?\\;\\s?")) |> 
  mutate(gen_info_type = "Basic Data",
         gen_info_sectnum = "1.1.")
write_delim(gen_info, "data-output/mentawai1933_general-info.csv",
            delim = "\t")

# Processing the word list =====
wlist_tags <- str_which(mtw33_words, "wordlist\\>")
wlist_1931_tags <- str_which(mtw33_words, "<\\/?wlist")

mtw33_wlist <- mtw33_words[(wlist_tags[1]+1):(wlist_tags[2]-1)]
mtw33_wlist_1931 <- mtw33_words[(wlist_1931_tags[1]+1):(wlist_1931_tags[2]-1)]

mtw33_words_1931_df <- tibble(mtw33 = mtw33_wlist_1931) |> 
  mutate(Index = str_extract(mtw33, "^[^ ]+(?=\\.\\s+)"),
         Forms = str_replace_all(mtw33, "(^[^ ]+\\.\\s|\\s?\\<[^>]+\\>)", ""),
         Notes_id = str_extract_all(mtw33, "(?<=\\<)([^>]+?)(?=\\>)")) |> 
  unnest_longer(Notes_id, keep_empty = TRUE) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  separate_longer_delim(cols = Forms, delim = regex("\\s*\\,\\s*"))
  

mtw33_words_df <- tibble(mtw33 = mtw33_wlist)
mtw33_words_df <- mtw33_words_df |> 
  mutate(Index = str_extract(mtw33, "^[^ ]+(?=\\.\\s+)"),
         Forms = str_replace_all(mtw33, "(^[^ ]+\\.\\s|\\s?\\<[^>]+\\>)", ""),
         Notes_id = str_extract_all(mtw33, "(?<=\\<)([^>]+?)(?=\\>)")) |> 
  unnest_longer(Notes_id, keep_empty = TRUE) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) # |> 
# mutate(Forms = str_split(Forms, "\\,\\s?")) |> 
# unnest_longer(Forms)

# check which Index in Mentawai (1933) is not in the Index of the NBL
id_mtw33_not_in_nbl <- which(!mtw33_words_df$Index %in% holle_tb$Index)
mtw33_words_df$Index[id_mtw33_not_in_nbl]
# character(0)
## fix the index : irrelevant for Mentawai 1933
# mtw33_words_df <- mtw33_words_df |> 
#   mutate(Index = replace(Index, Index == "l229", "1229"),
#          Index = replace(Index, Index == "1080/1081", "1080-1081"),
#          Index = replace(Index, Index == "1445/14458-1450", "1445/1448-1450"))
## again check which Index in Mentawai (1933) is not in the Index of the NBL
id_mtw33_not_in_nbl <- which(!mtw33_words_df$Index %in% holle_tb$Index)
mtw33_words_df$Index[id_mtw33_not_in_nbl]
# character(0)
# check which Index in Mentawai (1933) (for NBL ed. 1931) is not in the Index of the NBL
id_mtw33_not_in_nbl <- which(!mtw33_words_1931_df$Index %in% as.character(holle_1931$Index))
mtw33_words_1931_df$Index[id_mtw33_not_in_nbl]
# character(0)

# Processing the notes ====
notes_tags <- str_which(mtw33_words, "\\<\\/?notes\\>")
mtw33_notes <- mtw33_words[(notes_tags[1]+1):(notes_tags[2]-1)]
# check the notes tag available
mtw33_notes |> str_extract_all("\\<[^<\\/]+?\\>") |> unlist() |> unique()
# [1] "<n>"            "<form>"         "<form_variant>" "<comment>"      "<idn>"
mtw33_notes_df <- tibble(mtw33_notes)
mtw33_notes_df <- mtw33_notes_df |> 
  mutate(Notes_id = str_extract(mtw33_notes, "^[^ ]+?(?=\\.\\s+)")) |> 
  mutate(notes = str_replace(mtw33_notes, "^[^ ]+\\.\\s+", "")) |> 
  mutate(notes = str_split(notes, "\\s?\\;\\s")) |> 
  unnest_longer(notes, keep_empty = TRUE) |> 
  select(-mtw33_notes) |> 
  mutate(nt_form = str_extract(notes, "(?<=\\<form\\>)([^<]+?)(?=\\<\\/form\\>)"),
         nt_formVariant = str_extract(notes, "(?<=\\<form_variant\\>)([^<]+?)(?=\\<\\/form_variant\\>)"),
         nt_idn = str_extract(notes, "(?<=\\<idn\\>)([^<]+?)(?=\\<\\/idn\\>)"),
         nt_comment = str_extract(notes, "(?<=\\<comment\\>)([^<]+?)(?=\\<\\/comment\\>)")) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  separate_longer_delim(nt_form, regex("\\s?\\,\\s")) |> 
  select(-notes)

# join the NBL =====
tb <- mtw33_words_df |> 
  left_join(holle_tb, by = join_by(Index))

# join word list and the notes =====
## IMPORTANT after joining, check the nt_comment for "tida ada nama" 'no name'
tb <- tb |> 
  left_join(mtw33_notes_df) |> 
  mutate(across(where(is.character), ~replace_na(., ""))) |> 
  ## EXCLUDE row with comments "tida ada nama
  filter(str_detect(nt_comment, "^tida ada", negate = TRUE))

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
  # filter and fix multiple "Forms" and those which have "Notes_id", with nt_form refers to one of the multiple "Forms"
  mutate(Forms = if_else(str_detect(Forms, ", ") & Forms == "lalep, oema" & nt_form == "lalep",
                         "lalep",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "lalep, oema" & nt_form == "oema",
                         "oema",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "simasoesoera, oeraget" & nt_form == "simasoesoera" & English == "animal, beast",
                         "simasoesoera",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "simasoesoera, oeraget" & nt_form == "oeraget" & English == "animal, beast",
                         "oeraget",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "sikatai, simaligo" & nt_form == "sikatai" & English == "mad",
                         "sikatai",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "sikatai, simaligo" & nt_form == "simaligo" & English == "mad",
                         "simaligo",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "djoeroet, menoeroet" & nt_form == "djoeroet" & English == "to taste",
                         "djoeroet",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "djoeroet, menoeroet" & nt_form == "menoeroet" & English == "to taste",
                         "menoeroet",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "djoeroet, menoeroet" & nt_form == "djoeroet" & English == "to slurp",
                         "djoeroet",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "djoeroet, menoeroet" & nt_form == "menoeroet" & English == "to slurp",
                         "menoeroet",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "bo\"bo, pasibobo" & nt_form == "bo\"bo",
                         "bo\"bo",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "bo\"bo, pasibobo" & nt_form == "pasibobo",
                         "pasibobo",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "koilip , koiloep" & nt_form == "pasikoilip",
                         "koilip",
                         Forms),
         Forms = if_else(str_detect(Forms, ", ") & Forms == "koilip , koiloep" & nt_form == "pasikoiloep",
                         "koiloep",
                         Forms)
         ) |> 
  separate_longer_delim(Forms, stringr::regex("(\\s?, )")) |> 
  # add from note form the main Form that originally is empty/given note ID only
  ## the FormsAll column stores all forms (from the main list and the note form)
  mutate(FormsAll = if_else(Forms == "" & nt_form != "",
                            nt_form,
                            Forms)) |> 
  distinct() |> 
  mutate(across(matches("^v[0-9]"), ~str_replace(., "-", ""))) |> 
  left_join(nogloss3) |> # add the gloss for \ and - IDs
  mutate(English = if_else(!is.na(English3), 
                           English3, 
                           English)) |>
  mutate(Indonesian = if_else(!is.na(Indonesian3), 
                              Indonesian3, 
                              Indonesian)) |> 
  select(-English3, -Indonesian3)


write_tsv(tb, "data-output/mentawai1933_tb.tsv")
