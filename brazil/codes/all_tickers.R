
########################### PREPARE WORK ENVIRONMENT ########################### 

# clean work space
rm(list = ls())

# load packages
library(rvest)
library(tidyverse)
library(tidyquant)
library(janitor)
library(lubridate)
library(readxl)
library(fuzzyjoin)

############################### SELECT ALL TICKERS #############################

# provide URL
url <- read_html("http://www.fundamentus.com.br/detalhes.php?papel=")

# get the information that we want
tickers <- url %>% 
  html_nodes("a") %>%
  html_text() 

# understand vector
length(tickers)
head(tickers, 15)
tail(tickers, 50)

# position of first and last tickers
match("AALR3 ", tickers)
match("YDUQ3", tickers)

# subset vector
tickers <- tickers[match("AALR3 ", tickers):match("YDUQ3", tickers)]
head(tickers)

# eliminate white space
tickers <- str_replace_all(tickers, " ", "")

# inspect vector
head(tickers)
table(nchar(tickers), exclude = NULL)
tickers %>%
  as_tibble() %>%
  mutate(nchar = nchar(tickers)) %>%
  filter(nchar == 6) %>%
  head()

# add identification of Brazilian stocks
tickers <- paste0(tickers, sep = ".", "SA")
head(tickers)

################################ STOCKS RETURNS ################################

# get stocks that were present in the beginning of the study
stocks_start <-  tq_get(tickers,
                        get = "stock.prices", 
                        complete_cases = TRUE, 
                        from = "2018-09-16",  
                        to = "2018-09-18")

# get stocks that were present in the end of the study
stocks_end <-  tq_get(tickers,
                      get = "stock.prices", 
                      complete_cases = TRUE, 
                      from = "2021-09-16",  
                      to = "2021-09-18")

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
                       from = "2018-09-16", 
                       to = "2021-09-18")

# filter stocks that were traded at least once a week
stocks_subset <- stocks_both %>%
  group_by(symbol, yr = year(date), mon = month(date), week = week(date)) %>%
  summarise(volume_avg = mean(volume, na.rm = TRUE)) %>%
  group_by(symbol)  %>%
  summarise(volume_min = min(volume_avg, na.rm = TRUE)) %>%
  filter(volume_min > 0) %>%
  select(symbol) %>%
  pull()

# get data about stocks that were present 
# in the beginning and in the end of the study
# and that were traded at least once a month 
stocks_filtered <- stocks_both %>%
  filter(symbol %in% stocks_subset)

# we don't need ".SA" anymore 
stocks_final <- stocks_filtered %>%
  mutate(symbol = gsub("\\..*","", stocks_filtered$symbol))

# save object
wd <- "C:/Users/Dell/OneDrive/R"
setwd(wd)
getwd()
save(stocks_final, file = "./raw_data.rda")

# calculate daily returns
daily_returns <- stocks_final %>%
  group_by(symbol) %>%                          
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'stocks_returns')

daily_returns

# save object
save(daily_returns, file = "./daily_returns.rda")

#################################### BONDS #####################################

# read Brazilian bonds data
br_bonds_10y <- read_csv("./br_bonds_10y.csv")
br_bonds_10y
tail(br_bonds_10y)

# convert column names to machine readable format
br_bonds_10y <- br_bonds_10y %>%
  clean_names()
br_bonds_10y

# convert variable "data" to date format
br_bonds_10y <- br_bonds_10y %>%
  mutate(data = dmy(data))
br_bonds_10y

# plot br_bonds_10y
ggplot(br_bonds_10y, aes(x = data, y = ultimo)) + 
  geom_line()

# remove symbol "%"
br_bonds_10y <- br_bonds_10y %>%
  mutate(var_percent = str_sub(var_percent, 1, nchar(var_percent) - 1))
br_bonds_10y

# convert comma by dot
br_bonds_10y <- br_bonds_10y %>%
  mutate(var_percent = gsub(",", "\\.", br_bonds_10y$var_percent))
br_bonds_10y

# convert character class to numerical
br_bonds_10y$var_percent <- as.numeric(br_bonds_10y$var_percent)
br_bonds_10y

# rename column
br_bonds_10y <- br_bonds_10y %>%
  rename(date = data, 
         bonds_returns = var_percent)
br_bonds_10y 

############################## JOIN ALL DATA SETS ##############################

# join both data sets
df <- daily_returns %>%
  left_join(br_bonds_10y, by = "date") %>%
  select(date, symbol, stocks_returns, bonds_returns)
head(df)
tail(df)

# load data
setor <- read_excel("./classif_setor.xlsx", sheet = "Plan2")

# convert column names to machine readable format
setor <- setor %>%
  clean_names()
setor

# rename column
setor <- setor %>%
  rename(symbol = codigo)
setor

# merge data sets with partial matching
df <-  df %>% 
  regex_inner_join(setor, by = "symbol")

# rename column
df <- df %>%
  rename(symbol = symbol.x)

# remove column
df$symbol.y <- NULL
df

# save object
save(df, file = "./df_final.rda")

############################# COMPUTE CORRELATIONS #############################

# correlation by economic sector
cor_sector <- df %>%
  group_by(setor_economico) %>%
  summarise(cor = cor(stocks_returns, bonds_returns, use = "complete.obs"))

# correlation by sub sector
cor_subsector <- df %>%
  group_by(setor_economico, subsetor) %>%
  summarise(cor = cor(stocks_returns, bonds_returns, use = "complete.obs"))

# correlation by segment
cor_segment <- df %>%
  group_by(setor_economico, subsetor, segmento) %>%
  summarise(cor = cor(stocks_returns, bonds_returns, use = "complete.obs"))

# correlation by individual stocks
cor_stocks <- df %>%
  group_by(symbol) %>%
  summarise(cor = cor(stocks_returns, bonds_returns, use = "complete.obs"))

# save objects
save(cor_sector, cor_subsector, cor_segment, cor_stocks, 
     file = "./correlations.rda")

########################### EXPLORATORY DATA ANALYSIS ##########################

################################ ECONOMIC SECTOR ###############################

# view highest negative correlations
cor_sector %>%
  arrange(cor)

# view lowest negative correlations
cor_sector %>%
  arrange(desc(cor))

# convert "setor" into a data frame that matches with data frame "df"
short_tickers <- df %>%
  mutate(symbol = substr(symbol, start = 1, stop = 4)) %>%
  distinct(symbol) %>%
  pull()
setor_filtered <- setor %>%
  filter(symbol %in% short_tickers)

# how many sub sectors, segments and individual stocks, by sector?
eda_sector <- setor_filtered %>%
  group_by(setor_economico) %>%
  summarise(subsetores = length(unique(subsetor)),
            segmentos = length(unique(segmento)),
            stocks = length(unique(nome)))

# merge data sets
cor_sector <- cor_sector %>%
  left_join(eda_sector, by = "setor_economico")

# is there an association between sector correlation and number of sub sectors?
ggplot(data = cor_sector, aes(x = subsetores, y = cor)) + 
  geom_point() + 
  geom_smooth()

# is there an association between sector correlation and number of segments?
ggplot(data = cor_sector, aes(x = segmentos, y = cor)) + 
  geom_point() + 
  geom_smooth()

# is there an association between sector correlation and number of stocks?
ggplot(data = cor_sector, aes(x = stocks, y = cor)) + 
  geom_point() + 
  geom_smooth()

############################## ECONOMIC SUB SECTOR #############################

# view highest negative correlations
cor_subsector %>%
  arrange(cor)

# view highest positive correlations
cor_subsector %>%
  arrange(desc(cor))

# how many segments and individual stocks, by sub sector?
eda_subsector <- setor_filtered %>%
  group_by(setor_economico, subsetor) %>%
  summarise(segmentos = length(unique(segmento)),
            stocks = length(unique(nome)))
eda_subsector

# merge data sets
cor_subsector <- cor_subsector %>%
  left_join(eda_subsector, by = c("setor_economico", "subsetor"))
cor_subsector

# is there an association between sub sector correlation and number of segments?
ggplot(data = cor_subsector, aes(x = segmentos, y = cor)) + 
  geom_point() + 
  geom_smooth()

# is there an association between sub sector correlation and number of stocks?
ggplot(data = cor_subsector, aes(x = stocks, y = cor)) + 
  geom_point() + 
  geom_smooth()

############################### ECONOMIC SEGMENT ###############################

# view highest negative correlations
cor_segment %>%
  arrange(cor) %>%
  print(n = 20)

# view highest positive correlations
cor_segment %>%
  arrange(desc(cor)) %>%
  print(n = 20)


# how many segments and individual stocks, by sub sector?
eda_segment <- setor_filtered %>%
  group_by(setor_economico, subsetor, segmento) %>%
  summarise(stocks = length(unique(nome)))
eda_segment

# merge data sets
cor_segment <- cor_segment %>%
  left_join(eda_segment, by = c("setor_economico", "subsetor", "segmento"))
cor_segment

# what are the 3 companies from "saúde / comércio / medicamentos"?
cor_segment %>%
  filter(setor_economico == "Saúde", 
         subsetor == "Comércio e Distribuição",
         segmento == "Medicamentos e Outros Produtos")
df %>%
  filter(setor_economico == "Saúde", 
         subsetor == "Comércio e Distribuição",
         segmento == "Medicamentos e Outros Produtos") %>%
  distinct(nome) %>%
  select(nome)

# is there an association between segment correlation and number of stocks?
ggplot(data = cor_segment, aes(x = stocks, y = cor)) + 
  geom_point() + 
  geom_smooth()

# who is the outlier?
cor_segment %>%
  arrange(desc(stocks))

# same plot as above without outlier
cor_segment %>%
  filter(segmento != "Energia Elétrica") %>%
  ggplot(aes(x = stocks, y = cor)) + 
  geom_point() + 
  geom_smooth()

# same plot as above without 3 outliers
cor_segment %>%
  filter(!segmento %in% c("Energia Elétrica","Incorporações","Bancos")) %>%
  ggplot(aes(x = stocks, y = cor)) + 
  geom_point() + 
  geom_smooth()

############################## INDIVIDUAL STOCKS ###############################

# view highest negative correlations
cor_stocks %>%
  arrange(cor) %>%
  print(n = 30)

# view highest positive correlations
cor_stocks %>%
  arrange(desc(cor)) %>%
  print(n = 50)

# summary statistics
summary(cor_stocks$cor)

# stocks with the lowest correlation to brazilian 10 years bonds
cor_stocks %>%
  filter(cor > quantile(cor_stocks$cor, .75)) %>%
  arrange(desc(cor)) %>%
  print(n = Inf)

# save all objects
save(url,
     tickers,
     stocks_start,
     stocks_end, 
     stocks_start_end,
     stocks_both,
     stocks_subset,
     stocks_filtered,
     stocks_final,
     daily_returns,
     br_bonds_10y,
     df,
     setor,
     cor_sector,
     cor_subsector,
     cor_segment,
     cor_stocks,
     short_tickers,
     setor_filtered,
     eda_sector,
     eda_subsector,
     eda_segment,
     file = "./all_objects.rda")










