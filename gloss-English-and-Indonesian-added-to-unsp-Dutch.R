# Script to prepare a subset dataframe for English and Indonesian glosses for Index that has "/" and/or "-".
# the script file `merge-NBL-call.R` needs to be called first!

nogloss <- holle_tb_all |> 
  filter(str_detect(Index, "\\/|\\-")) |> 
  mutate(Index2 = Index) |>  
  separate_longer_delim(Index2, "/") |> 
  mutate(Index2 = str_replace(Index2, "-", ":"))

seq_gloss <- str_which(nogloss$Index2, ":")

for (i in seq_along(seq_gloss)) {
  
  gloss_range <- eval(parse(text = nogloss$Index2[seq_gloss[i]]))
  
  nogloss$Index2[seq_gloss[i]] <- str_c(gloss_range, collapse = ",")
  
  cat(seq_gloss[i], sep = "\n")
  
}

nogloss1 <- nogloss |> 
  separate_longer_delim(Index2, ",")

holle_tb_all2 <- holle_tb_all |> 
  rename(Index2 = Index,
         English2 = English,
         Indonesian2 = Indonesian) |> 
  select(Index2, English2, Indonesian2)

nogloss2 <- nogloss1 |> 
  left_join(holle_tb_all2) |> 
  group_by(Index) |> 
  mutate(English = str_c(str_c(English2, " [ID_", Index2, "]", sep = ""), collapse = "; "), 
         Indonesian = str_c(str_c(Indonesian2, " [ID_", Index2, "]", sep = ""), collapse = "; ")) |> 
  ungroup()
rm(holle_tb_all2)

nogloss3 <- nogloss2 |> 
  select(Index, English3 = English, Indonesian3 = Indonesian) |> 
  distinct()

# add the missing < unsp. > gloss
# holle_tb_all <- holle_tb_all |> 
#   left_join(nogloss3) |> 
#   mutate(English = if_else(!is.na(English3), 
#                            English3, 
#                            English)) |>
#   mutate(Indonesian = if_else(!is.na(Indonesian3), 
#                               Indonesian3, 
#                               Indonesian))