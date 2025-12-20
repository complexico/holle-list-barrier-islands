# this code deals with the manually checked and edited output of the digitisation by the student in the following repository:
# https://github.com/complexico/lexico-holle-list-barrier-islands

library(tidyverse)
library(googlesheets4)

source("merge-NBL-call.R") # run codes to retrieve the NBL and NBL's Concepticon mapping

maindb <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1P-JontDvH4MjKZ4pdqxthjTovSajJLKX6y2rtpuO5sc/edit?usp=sharing",
                                    col_types = c("cccccccccccicccccccccccccccllcccccccccccccc"),
                                    na = "NA")

mtw_nd <- maindb |> 
  filter(lang_name == "mentawai_nd") |> 
  mutate(ID = if_else(ID_correct == "", ID, ID_correct)) |> 
  mutate(lx_all = if_else(lx_all_correct == "", lx_all, lx_all_correct)) |> 
  mutate(nt_form = if_else(nt_form_correct == "", nt_form, nt_form_correct)) |> 
  mutate(de = if_else(de_correct == "", de, de_correct),
         dv = if_else(dv_correct == "", dv, dv_correct),
         nt_eng = if_else(nt_eng_correct == "", nt_eng, nt_eng_correct),
         nt_idn = if_else(nt_idn_correct == "", nt_idn, nt_idn_correct),
         nt_comment = if_else(nt_comment_correct == "", nt_comment, nt_comment_correct))

mtw_nd_tb <- mtw_nd |> 
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
  # add the English and Indonesian glosses for the additional data
  mutate(English = if_else(is.na(English) & str_detect(Index, "^add_"), de, English),
         Indonesian = if_else(is.na(Indonesian) & str_detect(Index, "^add_"), dv, Indonesian)) |> 
  mutate(nt_comment = str_replace_all(nt_comment, "\"([^\"]+?)\"", "“\\1”")) |> 
  mutate(across(where(is.character), ~str_replace_all(., "'", "ˈ"))) |> 
  # NFD transformation so that the diacritics can be searched via Keyman combining keystroke
  mutate(across(matches("(^Forms$|^remark$|^nt_)"), ~stringi::stri_trans_nfd(.)))

mentawai_nd_tb_out <- mtw_nd_tb |> 
  select(cats, 2:13, matches("^nt_"), matches("oncept(icon)?_")) |> 
  mutate(cats = "the Mentawai (Pagai & Sipora) list (no date)")
mentawai_nd_tb_out
mentawai_nd_tb_out |> write_tsv("data-output/mentawai_nd.tsv")
mentawai_nd_tb_out |> write_tsv("../mentawai-nd-holle-list/raw/mentawai_nd.tsv")
