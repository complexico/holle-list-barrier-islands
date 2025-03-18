library(tidyverse)
library(pdftools)
library(tesseract)

pdfs <- dir("pdf", full.names = TRUE)
pdfs <- str_subset(pdfs, "indonesian-index")
pdfs

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

# first page
txt <- idn[[1]] |> 
str_split("\\n") |> 
unlist() |> 
str_replace("^A ", "") |>
str_subset("^(b\\]|3$)", negate = TRUE) |>
(\(x) x[-c(1:4)])() |>
(\(x) x[nzchar(x)])()

txt1 <- txt |> 
str_replace("(?<=\\,)(\\s[0-9]+)\\s([^0-9])", "\\1\n\\2") |>
str_split("\\n") |>
unlist()
