wl_files <- dir("data-output", pattern = ".tsv", full.names = TRUE)

lang_name <- str_replace_all(basename(wl_files), "(\\.tsv|_tb)", "")

db <- map(wl_files, read_tsv) |> 
  map(~select(., -1)) |> 
  map2(.y = lang_name, \(x, y) mutate(x, lang_name = y)) |> 
  map(~relocate(., lang_name, .before = Index))

names(db) <- lang_name


db |> map(colnames)
