
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
p_load(tidyquant, tidyverse, here)

# get stocks that were present in the beginning of the study
stocks_start <-  tq_get(tickers,
                        get = "stock.prices", 
                        complete_cases = TRUE, 
                        from = "2018-09-13",  
                        to = "2018-09-17")

# get stocks that were present in the end of the study
stocks_end <-  tq_get(tickers,
                      get = "stock.prices", 
                      complete_cases = TRUE, 
                      from = "2021-09-16",  
                      to = "2021-09-20")

# select stocks that were present in both beginning and end of study 
stocks_start_end <- stocks_start %>%
  inner_join(stocks_end, by = "symbol") %>%
  select(symbol) %>%
  distinct(symbol) %>%
  pull()

# get data about stocks that were present 
# in the beginning and in the end of the study
stocks_both <-  tq_get(stocks_start_end,
                       get = "stock.prices", 
                       complete_cases = TRUE, 
                       from = "2018-09-13", 
                       to = "2021-09-20")

# filter stocks that were traded at least once a week
stocks_subset <- stocks_both %>%
  group_by(symbol, yr = year(date), mon = month(date), week = week(date)) %>%
  summarise(volume_avg = mean(volume, na.rm = TRUE)) %>%
  group_by(symbol)  %>%
  summarise(volume_min = min(volume_avg, na.rm = TRUE)) %>%
  filter(volume_min > 0) %>%
  select(symbol) %>%
  pull()

# filter stocks that were present in the beginning and in the end of the study
# and that were traded at least once a month 
stocks_filtered <- stocks_both %>%
  filter(symbol %in% stocks_subset)

# we don't need ".SA" anymore 
stocks_final <- stocks_filtered %>%
  mutate(symbol = gsub("\\..*","", stocks_filtered$symbol))

# calculate daily returns
daily_returns <- stocks_final %>%
  group_by(symbol) %>%                          
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'stocks_returns')

# save objects
save(stocks_final, daily_returns, file = here("brazil", "data", "03-stocks.rda"))

################################################################################


