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