library(tidyverse)

infos <- dir("data-output",
             pattern = "info",
             full.names = TRUE)
infosname <- basename(infos)
the_csv <- infos[1:2] |> 
  purrr::map(readr::read_csv) |> 
  purrr::map2(.y = str_replace_all(infosname[1:2],
                                   "_general.+csv$",
                                   ""), 
              \(x = .x, y = .y) dplyr::mutate(x, lang_name = y))

the_tsv <- infos[3:length(infos)] |> 
  purrr::map(readr::read_tsv) |> 
  purrr::map2(.y = str_replace_all(infosname[3:length(infos)],
                                   "_general.+csv$",
                                   ""), 
              \(x = .x, y = .y) dplyr::mutate(x, lang_name = y)) |> 
  purrr::map(~mutate(.x, query = replace_na(query, "--")))

the_infos <- c(the_csv, the_tsv)

purrr::walk2(the_infos,
             infos,
             \(df, filename) write_csv(df, filename))

