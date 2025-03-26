# Gede Primahadi W. Rajeg (2025)
# CIRHSS, CompLexico, Udayana University

library(tidyverse)
# library(pdftools)
# library(tesseract)

pdfs <- dir("pdf", full.names = TRUE)
pdfs <- str_subset(pdfs, "indonesian-index")
pdfs

# dir.create("../digitised-holle-list/data/indonesian-index")

# Indonesian Index ======
## ==== the following codes have been executed ==== ##
# pngs <- pdf_convert(pdfs,
#                     pages = NULL,
#                     dpi = 600)
# txts <- ocr(pngs)
# unlink(dir(pattern = "\\.png$"))
# saveRDS(txts, file = "ocr/indonesian-index.rds")
## ==== the preceding codes have been executed ==== ##

idn <- readRDS("ocr/indonesian-index.rds")


# first page =====

idn[[1]] |> 
  str_split("\\n") |> 
  unlist() |> 
  str_replace("^A ", "") |>
  str_subset("^(b\\]|3$)", negate = TRUE) |>
  (\(x) x[-c(1:4)])() |>
  (\(x) x[nzchar(x)])() |> 
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |> 
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-01.txt")


# second page =====

idn[[2]] |>
  str_replace("^19\\n\\nB\\s", "") |>
  str_split("\\n") |>
  unlist() |>
  (\(x) x[nzchar(x)])() |>
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |> 
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-02.txt")


# third page =====

idn[[3]] |>
  str_replace("^80\\n", "") |>
  str_split("\\n") |>
  unlist() |>
  str_replace("berbicara dengan artikulasi ", "") |>
  str_replace("^(uvular)", "berbicara dengan artikulasi \\1") |>
  (\(x) x[nzchar(x)])() |>
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |>
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-03.txt")


# fourth page =====

idn[[4]] |>
  str_replace("^81\\n", "") |>
  str_split("\\n") |>
  unlist() |> 
  str_replace("(?<=88)\\sC$", "") |>
  str_replace("e210", "210") |>
  str_replace("(?<=79\\s)_\\s(?=burung)", "") |>
  (\(x) x[nzchar(x)])() |>
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |>
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-04.txt")


# fifth page =====

idn[[5]] |>
  str_replace("^82\\n", "") |>
  str_split("\\n") |>
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace("^7 (?=delapan belas)", "") |>
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |>
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-05.txt")


# sixth page =====

idn[[6]] |>
  str_replace("^83\\n", "") |>
  str_replace("\\s+\\,\\s+G\\n(?=1389)", ", ") |> 
  str_replace("\\:\\s(?=gemuk)", "") |> 
  str_replace("F\\s(?=guling\\,)", "") |> 
  str_replace("a\\s(?=gunung\\,)", "") |> 
  str_split("\\n") |>
  unlist() |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |>
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-06.txt")


# seventh page =====

idn[[7]] |> 
  str_replace("H\\s+(?=ibu kota)", "") |> 
  # str_replace("(?<=\\n)adia\\n’", "hadiah, 991") |> 
  str_split("\\n") |>
  unlist() |> 
  str_replace("^nadian,", "hadiah,") |> 
  str_replace("(\\’ |^adia$|^\\.\\stoh\\.\\s110\\s|^g\\s7\\s\\>\\s3\\s|^rf\\s|^AS\\s+oe\\s+rm\\s(?=ini\\,))|^ar\\snm\\s+2\\s(?=injak)", "") |> 
  str_replace("^anga$", "hangat, 1104, 1105") |> 
  str_replace("^anya,\\s+oy\\s", "") |> 
  str_replace("^h$", "hanya, 1477") |> 
  str_replace("^\"\\sre\\sZ\\s7\\s(?=indah)", "") |> 
  str_replace("^ar$", "hari, 1394") |> 
  str_replace("^(harga)$", "\\1, 1007") |> 
  str_replace("\\»\\s139\\s", "") |> 
  str_replace("^h 132$", "haus, 132") |> 
  str_replace("^hari 1", "harimau, 815") |> 
  str_replace("^hati$", "hati, 56") |> 
  str_replace("(^(i|2)$|^\\»\\s9\\s\\,\\s+(?=ipar))", "") |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace("^aus\\,\\s13\\s\\,\\s", "") |> 
  str_replace("^(hendak)\\s\\,\\s\\-\\s3", "\\1, 1243") |> 
  str_replace("^hidup\\,\\s1", "hidup, 182") |> 
  str_replace("^(hidung\\,\\s)\\;", "\\118") |> 
  str_replace("(ee\\.)", "1447") |> 
  str_replace("(?<=hijau\\,\\s)aa", "1173") |> 
  str_replace("(?<=381)\\s4$", "") |> 
  str_replace("(?<=352\\s)janat", "jahat") |> 
  str_replace("^I\\s(?=jalan\\, 959)", "") |> 
  str_replace("Jamur", "jamur") |> 
  str_replace("Jambu, fle", "jambu, 712") |> 
  str_replace("'\\s+cat\\s+\\-\\ske\\s+TTT\\s(?=jam)", "") |> 
  str_replace("^\\(laki\\-laki\\)\\,\\s1363", "ia, kata ganti orang ke III (laki-laki), 1363") |> 
  str_replace("^a\\,\\s+kata\\sganti\\sorang\\ske$", "") |> 
  str_replace("\\s(?=jamur\\,)", " (perempuan), 1364 ") |> 
  str_replace("^\\(perempuan\\)\\,\\s1364\\s", "") |> 
  str_replace("(?<=^hitam\\,\\s)11", "1168") |> 
  (\(x) x[nzchar(x)])() |> 
  str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
  str_split("\\n") |>
  unlist() |>
  write_lines("../digitised-holle-list/data/indonesian-index/sheet-07.txt")
