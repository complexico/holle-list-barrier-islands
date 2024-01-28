library(tidyverse)

pdfs <- dir("pdf", full.names = TRUE)
pdfs <- stringr::str_subset(pdfs, 
                            "(lekon|tapah|simalur|coverpage)", 
                            negate = TRUE)
pdfs

# Mentawai 1933 ======
## ==== the following codes have been executed ==== ##
# pngs <- pdftools::pdf_convert(pdfs[pdfs == "pdf/mentawai-1-1933.pdf"], 
#                               pages = NULL, 
#                               dpi = 600)
# txts <- tesseract::ocr(pngs)
# unlink(dir(pattern = "\\.png$"))
# saveRDS(txts, file = "ocr/mentawai1933.rds")
## ==== the preceding codes have been executed ==== ##

## 1. fixing =====
txts <- readRDS("ocr/mentawai1933.rds")
txts1 <- txts |> 
  map(str_split, "\\\n") |> 
  map(unlist)
### p. 1 =====
txts1[[1]] <- txts1[[1]] |> 
  (\(.) .[nzchar(.)])() |> 
  (\(.) .[.!="155"])() |> 
  (\(.) .[.!="Ngangan Sakalagan"])() |> 
  str_replace("\\s(\\>;?|;|;)\\s", " : ") |> 
  str_replace("\\bcalled$", "called Ngangan Sakalagan") |> 
  str_replace("(?<=place\\slie\\s)in", "in the area where the language is spoken? : -") |> 
  (\(.) .[.!="the area where the"])() |> 
  (\(.) .[.!="language is spoken? - ="])() |> 
  str_replace("^YeX", "Sex") |> 
  str_replace("J.-H\\.", "J.H.") |> 
  str_replace("(?<=Commander of )the$", "the Mentawai islands (1st Lieutenant of the Infantry) ; Enos Tamboenan - clerk 1st grade in the local government office (Batak)") |> 
  (\(.) .[!. %in% c("Mentawai islands (lst Lieutenant of", "the Infantry)", "Fnos Tamboenan - clerk lst grade in", "the local government office (Batak)")])() |> 
  (\(.) .[-c(1:3)])() |> 
  (\(.) tibble(items = .))() |> 
  separate(items, 
           into = c("query", "value"), 
           sep = "\\s\\:\\s", 
           remove = TRUE)
  
### p. 2 =====
txts1[[2]] <- txts1[[2]] |> # page 2
  str_replace_all("80\\s\\/$", "") |> 
  str_replace_all("S1(?=\\. pakin)", "80/81") |> 
  str_replace_all("^eo(?=\\.)", "2") |> 
  str_replace_all("^of(?=\\. boeloe)", "27") |>
  str_replace_all("^el(?=\\. bakala\\, bailat)", "21") |>
  str_replace_all("^26(?=\\. loette)", "36") |> 
  str_replace_all("^WO(?=\\. tetekat)", "40") |> 
  str_replace_all("^WW(?=\\. tottot\\sJe\\.)", "44") |> 
  str_replace_all("\\bJe(?=\\.\\stetaro)", "92") |> 
  str_replace_all("\\b19(?=\\.\\s\\<3\\>)", "79") |> 
  str_replace_all("\\bTt(?=\\.\\s\\<2\\>)", "77") |> 
  str_replace_all("\\b53(?=\\.\\skia)", "83") |> 
  str_replace_all("\\b59(?=\\.\\standa)", "89") |> 
  str_replace_all("\\b70(?=\\.\\sbakapat)", "90") |> 
  str_replace_all("\\b93\\-\\s", "93. ") |> 
  str_replace_all("\\(3(?=\\. moetanal)", "73") |> 
  str_trim(side = "both") |> 
  (\(.) .[nzchar(.)])() |> 
  str_replace("\\s(?=[0-9]+([\\/\\-][0-9]+)?\\.)", "__") |> 
  str_replace_all("^2\\.\\s(?=THE MENTAWAI)", "") |> 
  str_replace_all("(?<=^7\\.\\s)poengilo", "poenglo") |> 
  str_replace_all("(?<=^14\\.\\s)arépet", "arèpet") |> 
  str_replace_all("(?<=^16\\.\\s)[^_]+", "eloĕ") |> 
  str_replace_all("(?<=_73\\.\\s).+", "moetanai") |> 
  str_replace_all("(?<=_74\\.\\s).+", "ètoet") |> 
  str_replace_all("(?<=_92\\.\\s).+", "teiaro") |> 
  str_replace_all("(?<=_96\\.\\s).+", "kabei") |> 
  (\(.) .[-c(1:2)])() |> 
  str_split("__") |> 
  unlist() |> 
  str_replace("^(\\d)(?=\\.\\s)", "0\\1") |> 
  sort() |> 
  str_replace("^0(?=[1-9]\\.)", "")
  # str_c("__156__THE MENTAWAI_LIST") |> 
  # (\(.) tibble(items = .))() |> 
  # separate(col = "items",
  #          into = c("lcol", "rcol"),
  #          sep = "__",
  #          fill = "right") |> 
  # mutate(page = 156, listname = "mentawai-1933")
### p. 3 ======
txts1[[3]] <- txts1[[3]] |> 
  (\(.) .[nzchar(.)])() |> 
  str_replace("(?<=^133\\.\\s)(\\<\\>)", "<7>") |> 
  str_replace("(?<=singongai)\\s(?=162\\.\\smoegati)", " dere) ") |> 
  str_replace("^amie\\s+\\)\\s+(?=163)", "") |> 
  str_replace("(?<=singongai)\\s(?=164\\.\\ssoibi)", " dere) ") |> 
  str_replace("^dere\\s+\\)", "") |> 
  (\(.) .[nzchar(.)])() |> 
  str_replace("Le5\\.", "165.") |> 
  str_replace("5(0[12])\\.", "2\\1.") |> 
  str_replace("ie matet$", "183/185. matei") |> 
  str_replace("moedOnggooe", "moeónggōoe") |> 
  str_replace("hi9\\.", "179.") |> 
  str_replace("or “he$", "173. <12>") |> 
  str_replace("boo eee$", "169. paisi") |> 
  str_replace("(?<=152\\.\\s)reggeoce", "reggeoe") |> 
  str_replace("(?<=^113\\.\\sinan )deré", "derĕ") |> 
  str_replace("(?<=^116\\.\\s)logace", "logaoe") |> 
  str_replace("(?<=^124\\.\\sattinan)e", "g") |> 
  str_replace("(?<=^127\\.\\s)[^ ]+?(?=\\s)", "moeēnga") |> 
  str_replace("(?<=moeloea)\\'tan$", "'an") |> 
  str_replace("^129,", "129.") |> 
  str_replace("(?<=^141\\.\\s)moelaoceldoe", "moelāöelāoe") |> 
  str_replace("\\s(?=[0-9]+([\\/\\-][0-9]+)?\\.)", "__") |> 
  str_subset("^MENTAWAI 157", negate = TRUE) |> 
  str_split("__") |> 
  unlist() |> 
  sort()
  # (\(.) tibble(items = .))() |> 
  # separate(col = "items",
  #          into = c("lcol", "rcol"),
  #          sep = "__",
  #          fill = "right") |> 
  # mutate(page = 157, listname = "mentawai-1933")

### p. 4 ======
txts1[[4]] <- txts1[[4]] |> 
  (\(.) .[-1])() |> 
  str_replace("\\bse\\!(?=\\sbagi$)", "272/273.") |> 
  str_replace("^ci0", "210") |> 
  str_replace("cle", "212") |> 
  str_replace("(?<=moego\\s)7\\s", "") |> 
  str_replace("(?<=^216\\.\\slotso\\,\\s)toék", "toēk") |> 
  str_replace("21\\.\\sinte\\,", "281. kameman") |> 
  str_replace("^eit\\. tlevmper", "217. tiettiet") |> 
  str_replace("(\\s587\\-|\\se9gl\\-|\\s308\\/$|\\s311\\-$)", "") |> 
  str_replace("^20\\.\\s", "220. ") |> 
  str_replace("290\\.", "287-290.") |> 
  str_replace("Oocl", "221") |> 
  str_replace("294\\.", "291-294.") |> 
  str_replace("^ced\\. Sageoe 59e", "225. sageoe 295/296") |> 
  str_replace("^cel\\.(?=\\sappoe)", "227.") |> 
  str_replace("^pee mire Re", "228. takkep") |> 
  str_replace("\\s\\b306\\/$", "") |> 
  str_replace("^030\\.", "230.") |> 
  str_replace("307\\.", "306/307.") |> 
  str_replace("(?<=^232\\.\\s)male", "māle") |> 
  str_replace("^233\\/\\s", "308/") |> 
  str_replace("(?<=309\\.\\s)Llakoet", "lakoet") |> 
  str_replace("^234\\.", "233/234.") |> 
  str_replace("^236\\-\\s(?=314)", "311-") |> 
  str_replace("^238\\.", "236-238.") |> 
  str_replace("(?<=^239\\.\\s)onl", "oni") |> 
  str_replace("oekoel(?=\\s348)", "oekoei") |> 
  str_replace("^eo\\/", "257") |> 
  str_replace("(?<=^259\\.\\s)bela", "beia") |> 
  str_replace("259\\/\\s371\\/", "") |> 
  (\(.) .[nzchar(.)])() |> 
  str_replace("260\\.\\s(?=tasoele)", "259/260. ") |> 
  str_replace("372(?=\\.\\ssikemboekat)", "371/372") |> 
  str_replace("^270\\/", "270/271. kemboe") |> 
  (\(.) .[.!="ofl. kemboe"])() |> 
  str_replace("\\s(?=[0-9]+([\\/\\-][0-9]+)?\\.)", "__") |> 
  str_split("__") |> 
  unlist() |> 
  sort()

### p. 5 ========
txts1[[5]] <- txts1[[5]] |> 
  (\(.) .[-1])() |> 
  str_replace("(\\s456\\/$|\\s458\\s\\/$)", "") |> 
  str_replace("\\s{2,}", " ") |> 
  str_replace("^5\\see\\sCh\\s", "") |> 
  str_replace("(?<=\\stimata\\s)459\\.", "458/459.") |> 
  str_replace("(?<=\\bpoenen)\\s(?=(473))", " simamatei ") |> 
  str_replace("(?<=preman\\ssioko\\s)457.+", "456/457. bachan lalep, bachan lelengan") |> 
  str_replace("pasitalimaoce", "pasitalimaoe") |> 
  str_replace("pangoeredi", "pangoerei") |> 
  str_replace("(?<=^420\\.\\s)pipiaoce", "pipiaoe") |> 
  str_replace("(\\bpasiragat)$", "pasiragat (lalep)") |> 
  str_replace("(?<=poepoeiloe\\s)\\(lalep\\)$", "") |> 
  str_replace("501\\/", "") |> 
  str_replace("\\bMet\\s\\#", "501/502. s") |> 
  str_replace("(?<=^432)\\,", ".") |> 
  str_replace("205(?=\\.\\sbalioegoet)", "505") |> 
  str_replace("balioegoet$", "baloegoei") |> 
  str_replace("906", "506") |> 
  str_replace("\\bms\\sPoke$", "509. baliok") |> 
  str_replace("44d\\s\\/\\ser$", "510. <29>") |> 
  str_replace("^445(?=\\.\\stobat)", "441/445") |> 
  str_replace("913", "513") |> 
  str_replace("(?<=^448)\\,", ".") |> 
  str_replace("(?<=\\b517\\.\\s)dong(?=kisooe)", "ōōng") |> 
  str_replace("^453\\/\\s", "") |> 
  str_replace("(?<=518\\.\\s)péti", "pėti") |> 
  str_replace("^454", "453/454") |> 
  (\(.) .[nzchar(.)])() |> 
  str_replace("\\s(?=[0-9]+([\\/\\-][0-9]+)?\\.)", "__") |> 
  str_split("__") |> 
  unlist() |> 
  str_replace("^457\\..+$", "456/457. bachan lalep, bachan lelengan") |> 
  sort()

### p. 6 ======
txts1[[6]] |> 
  (\(l) l[-1])() |> 
  (\(l) l[nzchar(l)])() |> 
  str_replace("\\s605\\/", "") |> 
  str_replace("^525\\-\\s", "605/") |> 
  str_replace("^527", "525-527") |> 
  str_replace("^\\,\\s", "") |> 
  str_replace("\\spanoenggoeloe$", "") |> 
  str_replace("(?<=641\\.\\smoegalai polak),", ", panoenggoeloe") |> 
  str_replace("^mi\\.\\sSeenee\\sa\\'s\\sbat\\soinan", "569. leppei 657/658. bat oinan") |> 
  str_replace("^B03\\!", "571/572.") |> 
  str_replace("^9\\(", "57") |> 
  str_replace("(?<=letjoet\\s)kabel", "kabei") |> 
  str_replace("(?<=takat\\s)kabel", "kabei") |> 
  str_replace("<3\\/>", "<37>") |> 
  str_replace("\\s(?=[0-9]+([\\/\\-][0-9]+)?\\.)", "__") |> 
  str_split("__") |> 
  unlist() |> 
  sort()
