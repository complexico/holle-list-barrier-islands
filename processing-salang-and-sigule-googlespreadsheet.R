# this code deals with the manually checked and edited output of the digitisation by the student in the following repository:
# https://github.com/complexico/lexico-holle-list-barrier-islands

library(tidyverse)
library(googlesheets4)

source("merge-NBL-call.R") # run codes to retrieve the NBL and NBL's Concepticon mapping
source("gloss-English-and-Indonesian-added-to-unsp-Dutch.R")

maindb <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1P-JontDvH4MjKZ4pdqxthjTovSajJLKX6y2rtpuO5sc/edit?usp=sharing",
                                          col_types = c("cccccccccccicccccccccccccccllcccccccccccccc"),
                                          na = "NA")

salang_sigule1920 <- maindb |> 
  filter(lang_name == "salangsigule") |> 
  mutate(ID = if_else(ID_correct == "", ID, ID_correct)) |> 
  mutate(lx_all = if_else(lx_all_correct == "", lx_all, lx_all_correct)) |> 
  mutate(nt_form = if_else(nt_form_correct == "", nt_form, nt_form_correct)) |> 
  mutate(de = if_else(de_correct == "", de, de_correct),
         dv = if_else(dv_correct == "", dv, dv_correct),
         nt_eng = if_else(nt_eng_correct == "", nt_eng, nt_eng_correct),
         nt_idn = if_else(nt_idn_correct == "", nt_idn, nt_idn_correct),
         nt_comment = if_else(nt_comment_correct == "", nt_comment, nt_comment_correct)) |> 
  # split multiple forms in a cell
  separate_longer_delim(lx_all, "/") |> 
  # add codes to determine the types of the list based on the Index
  mutate(list_type = "NBL",
         list_type = if_else(ID %in% holle_1904_1911$Index,
                             "added_list_1904_1911",
                             list_type),
         list_type = if_else(ID %in% holle_1931$Index,
                             "added_list_1931",
                             list_type),
         list_type = if_else(str_detect(ID, "^add_"),
                             "added_data",
                             list_type))

salang_sigule1920_tb <- salang_sigule1920 |> 
  select(-Index, -Dutch, -English, -Indonesian, -lx) |> 
  distinct() |> 
  rename(Index = ID, 
         Notes_id = nt) |> 
  left_join(holle_tb_all) |> 
  select(!matches("_correct")) |> 
  left_join(concepticon_checked) |> 
  rename(Forms = lx_all) |> 
  relocate(Notes_id, .after = Forms) |> 
  relocate(Dutch, .after = Notes_id) |> 
  relocate(English, .after = Dutch) |> 
  relocate(Indonesian, .after = English) |> 
  relocate(v1894, .after = Indonesian) |> 
  relocate(`v1904/1911`, .after = v1894) |> 
  relocate(v1931, .after = `v1904/1911`) |> 
  relocate(Swadesh, .after = v1931) |> 
  relocate(Swadesh_orig, .after = Swadesh) |> 
  relocate(remark, .after = Swadesh_orig) |> 
  # If there is empty translation from the original English (e.g., due to Dutch ... <unsp.>), get the English and Indonesian from dv and de columns
  # mutate(English_add2 = if_else(English == "" & str_detect(Dutch, "(unsp\\.)"), de, ""),
  #        Indonesian_add2 = if_else(Indonesian == "" & str_detect(Dutch, "(unsp\\.)"), dv, "")) |> 
  # NFD transformation so that the diacritics can be searched via Keyman combining keystroke
  mutate(across(matches("(^Forms$|^remark$|^nt_)"), ~stringi::stri_trans_nfd(.)))

# no_glosses_id <- unique(pull(filter(salang_sigule1920_tb, English == ""), Index))
# 
# additional_glosses1 <- filter(additional_glosses, Index %in% no_glosses_id)
# 
# setdiff(no_glosses_id, additional_glosses$Index)

salang_sigule1920_tb_out <- salang_sigule1920_tb |> 
  select(cats, 2:13, 
         #English_add, 
         #Indonesian_add, 
         matches("^nt_"), matches("oncept(icon)?_"),
         list_type) |> 
  select(-nt_lekon, -nt_simalur) |>  # remove because they are empty
  left_join(nogloss3) |> # add the gloss for \ and - IDs
  mutate(English = if_else(!is.na(English3), 
                           English3, 
                           English)) |>
  mutate(Indonesian = if_else(!is.na(Indonesian3), 
                              Indonesian3, 
                              Indonesian)) |> 
  select(-English3, -Indonesian3)
salang_sigule1920_tb_out
salang_sigule1920_tb_out |> write_tsv("data-output/salang_sigule1920.tsv")
salang_sigule1920_tb_out |> write_tsv("../SalangSigule1920-holle-list/raw/salang_sigule1920.tsv")
