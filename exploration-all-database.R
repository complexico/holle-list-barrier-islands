library(tidyverse)

lekon <- read_tsv("data-output/lekon_tb.tsv") |> 
  select(-1) |> 
  mutate(lang_name = "lekon") |> 
  mutate(across(matches("^v[0-9]"), ~as.character(.)))
simalur <- read_tsv("data-output/simalur_tb.tsv") |> 
  select(-1) |> 
  mutate(lang_name = "simalur") |> 
  mutate(across(matches("^v[0-9]"), ~as.character(.)))
mentawai <- read_tsv("data-output/mentawai1933_tb.tsv") |> 
  select(-1) |> 
  mutate(lang_name = "mentawai_1933") |> 
  mutate(across(matches("^v[0-9]"), ~as.character(.)))
enggano <- readr::read_csv("C:/Users/GRajeg/OneDrive - Nexus365/Documents/Research/holle-list-enggano-1895/cldf/forms.csv")

db <- bind_rows(lekon, simalur, mentawai) |> 
  # select(Index, Forms, English, Indonesian, lang_name) |> 
  bind_rows(enggano |> 
              select(Index = Holle_ID,
                     Forms = Value,
                     English, Indonesian) |> 
              mutate(lang_name = "enggano"))

barrier_islands <- dir("C:/Users/GRajeg/iCloudDrive/Documents/research/Lexiology_Lexicography_Even-2024_Holle_List_Barrier_Islands/output",
    full.names = TRUE)
barrier_islands_basename <- basename(barrier_islands)[-1] |> 
  str_replace("_tb.+$", "")
barrier_islands_db <- map(barrier_islands[-1],
                          readr::read_rds)
db2 <- barrier_islands_db |> 
  map2(.y = barrier_islands_basename,
       ~mutate(.x,
               lang_name = .y))
db3 <- db2 |> 
  list_rbind() |> 
  rename(Index = ID,
         Forms = lx,
         English = de,
         Indonesian = dv)

db_merge <- db |> bind_rows(db3) |> 
  mutate(lang_name = factor(lang_name,
                            levels = c("lekon", "simalur", "semalur",
                                       "salangsigule", "sigulesalang",
                                       "nias1905", "nias1911", "mentawai",
                                       "mentawai_1933", "enggano")))
