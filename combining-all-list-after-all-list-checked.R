wl_files <- dir("data-output", pattern = ".tsv", full.names = TRUE)

lang_name <- str_replace_all(basename(wl_files), "(\\.tsv|_tb)", "")

db <- map(wl_files, read_tsv) |> 
  map(~select(., -1)) |> 
  map2(.y = lang_name, \(x, y) mutate(x, lang_name = y)) |> 
  map(~relocate(., lang_name, .before = Index))

names(db) <- lang_name

factor_order <- c("lekon", "tapah", "simalur", "seumalur1912", "sigule_salang1912", "salang_sigule1920", "mentawai_nd", "mentawai1933", "nias1905", "nias1911")

# test filtering data
db |> 
  map(~filter(., str_detect(English, "fire"))) |> 
  map(~select(., lang_name, Index, Forms, Dutch, English, Indonesian)) |> 
  list_rbind() |> 
  mutate(lang_name = factor(lang_name, levels = factor_order)) |> 
  arrange(lang_name, Forms)

# test checking multiple forms in a cell
db |> 
  map(~filter(., str_detect(Forms, ","))) |> 
  map(~select(., lang_name, Index, Forms, Dutch, English, Indonesian)) |> 
  list_rbind() |> 
  mutate(lang_name = factor(lang_name, levels = factor_order)) |> 
  arrange(lang_name, Forms)

db |> 
  map(~filter(., str_detect(Forms, ","))) |> 
  map(~select(., lang_name, Index, Forms, Dutch, English, Indonesian)) |> 
  list_rbind() |> 
  mutate(lang_name = factor(lang_name, levels = factor_order)) |> 
  arrange(lang_name, Forms) |> 
  count(lang_name)

# A tibble: 5 × 2
# lang_name             n
# <fct>             <int>
#   1 tapah                47
# 2 seumalur1912          8
# 3 sigule_salang1912    10
# 4 mentawai_nd           4
# 5 nias1911              5