
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
p_load(here, tidyverse, fuzzyjoin)

# load data
load(here("brazil", "data", "01-bonds_10y_real_return.rda"))
load(here("brazil", "data", "02-tickers_today.rda"))
load(here("brazil", "data", "03-stocks.rda"))
load(here("brazil", "data", "04-setor.rda"))

# join data sets "daily returns" and "bond_return"
df <- daily_returns %>%
  left_join(bond_return, by = "date") %>%
  drop_na()

# join data sets "df" and "setor"
df <-  df %>% 
  regex_inner_join(setor, by = "symbol") %>% 
  select(date, nome, symbol.x, setor_economico, subsetor, segmento, 
         stocks_returns, bonds_returns, -symbol.y, -listagem) %>% 
  rename(symbol = symbol.x)
  
# save object
save(df, file = here("brazil", "data", "05-df_final.rda"))

################################################################################





