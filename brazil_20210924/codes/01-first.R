
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
p_load(downloader, here, tidyverse, lubridate, readxl)

# store url
url_1968_1997 <- "https://sistemasweb.b3.com.br/indices/IBOVDIA.zip"

# download zipped file
download(url_1968_1997, 
         dest = here("brazil_20210924", "data", "ibov_1968_1997.zip"), 
         mode="wb") 


# unzip file
unzip(zipfile = here("brazil_20210924", "data", "ibov_1968_1997.zip"), 
      exdir = here("brazil_20210924", "data"))

# read data into R
path <- here("brazil_20210924", "data", "IBOVDIA.xls")
ibov1 <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x, range = "A2:M33"), .id = "sheet")

# take a look at data
head(ibov1)
glimpse(ibov1)

# rename columns
names(ibov1)[1] <- "ano"
names(ibov1)[2] <- "dia"

# convert variable "ano" into numeric
ibov1 <- ibov1 %>%
  mutate(ano = as.numeric(ano))

# convert wide to long data set
ibov1 <- ibov1 %>%
  pivot_longer(cols = c("JAN":"DEZ"), names_to = "mes", values_to = "ibov")

# create variable "date"
ibov1 <- ibov1 %>%
  mutate(mes = recode(mes, 
                      'JAN' = 1,
                      'FEV' = 2,
                      'MAR' = 3,
                      'ABR' = 4,
                      'MAIO' =5 ,
                      'JUN' = 6,
                      'JUL' = 7,
                      'AGO' = 8,
                      'SET' = 9,
                      'OUT' = 10,
                      'NOV' = 11,
                      'DEZ' = 12),
         date = make_date(ano, mes, dia)) %>%
  select(date, ibov) %>%
  arrange(date)

# drop rows where both columns are NAs
ibov1 <- ibov1 %>%
  filter(!(is.na(date) & is.na(ibov)))

# save object
save(ibov1, file = here("brazil_20210924", "data", "ibov_1968_1997_tidy.rda"))
