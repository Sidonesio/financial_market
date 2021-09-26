
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
p_load(here, tidyverse, data.table, lubridate)

# read data
ibov2 <- 
  list.files(here("brazil_20210924", "data"),
             pattern="*.csv",
             full.names = T) %>% 
  map_df(~fread(., encoding="Latin-1", nrows = 31, na.strings = "")) %>%
  as_tibble()

# take a look at data set
ibov2
glimpse(ibov2)

# rename first column
ibov2 <- ibov2 %>%
  rename(dia = V1)

# create variable "ano"
ibov2 <- ibov2 %>%
  mutate(ano = rep(1998:2020, each = 31))

# convert data set from wide to long
ibov2 <- ibov2 %>%
  pivot_longer(cols = c("V2":"V13"), names_to = "mes", values_to = "ibov")

# recode variable "mes"
ibov2 <- ibov2 %>%
  mutate(mes = recode(mes,
                      'V2' = 1,
                      'V3' = 2,
                      'V4' = 3,
                      'V5' = 4,
                      'V6' = 5,
                      'V7' = 6,
                      'V8' = 7,
                      'V9' = 8,
                      'V10' = 9,
                      'V11' = 10,
                      'V12' = 11,
                      'V13' = 12))

# take a look at data
ibov2
glimpse(ibov2)

# convert column "ibov" to numeric
ibov2 <- ibov2 %>%
  mutate(ibov = as.numeric(gsub(",", ".", gsub("\\.", "", ibov2$ibov))))

# convert other columns to numeric
ibov2 <- ibov2 %>% 
  mutate_if(is.character,as.numeric)

# create variable "date"
ibov2 <- ibov2 %>%
  mutate(date = make_date(ano, mes, dia)) %>%
  select(date, ibov) %>%
  arrange(date)

# remove NAs on both columns
ibov2 <- ibov2 %>%
  filter(!(is.na(date) & is.na(ibov)))

# save object
save(ibov2, file = here("brazil_20210924", "data", "ibov_1998_2020_tidy.rda"))

