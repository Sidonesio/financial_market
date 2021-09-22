
# clean work space
rm(list = ls())

# unload packages previously loaded
lapply(names(sessionInfo()$otherPkgs), function(pkgs)
  detach(
    paste0('package:', pkgs),
    character.only = T,
    unload = T,
    force = T))

# install pacman if it is not installed
if (!require("pacman")) install.packages("pacman")

# install packages if they are not installed
# and load them
p_load(downloader, here, utils, readxl, tidyverse)

# store url
url_zip <- "https://bvmf.bmfbovespa.com.br/InstDados/InformacoesEmpresas/ClassifSetorial.zip"

# download zipped file
download(url_zip, dest = here("brazil", "data", "classif_setor.zip"), mode="wb") 

# unzip file
unzip(zipfile = here("brazil", "data", "classif_setor.zip"), 
      exdir = here("brazil", "data"))

# read file into R
setor <- read_excel(
  here("brazil", "data", "Setorial B3 15-09-2021 (portuguˆs).xlsx"))

# take a look at data
setor %>%
  print(n = 30)
setor %>%
  tail(n = 30) %>%
  print(n = Inf)

# change data set column names
colnames <- c("setor_economico", "subsetor", "segmento", "symbol", "listagem")
colnames(setor) <- colnames

# delete first rows
setor <- setor[-c(1:7),]

# return positions of rows to be excluded
match("(DR1) BDR Nível 1", setor$setor_economico) - 1
nrow(setor)

# delete last rows
setor <- setor[-c(599:616),]

# fill in missing values with previous value
setor <- setor %>%
  fill(setor_economico, subsetor)

# remove rows that do not bring new information
setor <- setor %>%
  filter(setor_economico != "SETOR ECONÔMICO")

# remove rows where all three columns below are NAs
setor <- setor %>%
  filter(!(is.na(segmento) & is.na(symbol) & is.na(listagem)))

# fix variable "segmento"
setor <- setor %>%
  mutate(segmento2 = case_when(
    str_ends(segmento, "[:lower:]") ~ segmento)) %>%
  fill(segmento2) %>%
  rename(nome = segmento,
         segmento = segmento2)

# remove rows where symbol is equal to NA
setor <- setor %>%
  filter(!is.na(symbol))

# reorder columns
setor <- setor %>%
  select(setor_economico, subsetor, segmento, nome, symbol, listagem)

# save object
save(setor, file = here("brazil", "data", "04-setor.rda"))

