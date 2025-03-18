# Gede Primahadi W. Rajeg (2025)
# CIRHSS, CompLexico, Udayana University

library(tidyverse)
library(pdftools)
library(tesseract)

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
