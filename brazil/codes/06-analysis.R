
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
p_load(here, tidyverse)

# load data
load(here("brazil", "data", "05-df_final.rda"))
load(here("brazil", "data", "04-setor.rda"))

#################### COMPUTE CORRELATIONS AND ADD DATA SETS ####################

# correlation by economic sector
cor_sector <- df %>%
  group_by(setor_economico) %>%
  summarise(cor = cor(stocks_returns, bonds_returns))

# count number of sub sectors, segments and stocks, by sector
counts_sector <- df %>%
  group_by(setor_economico) %>%
  summarise(subsetores_n = length(unique(subsetor)),
            segmentos_n = length(unique(segmento)),
            stocks_n = length(unique(symbol)))

# merge data sets "correlation" and "counts" (based on economic sector)
cor_sector <- cor_sector %>%
  left_join(counts_sector, by = "setor_economico")

# correlation by sub sector
cor_subsector <- df %>%
  group_by(setor_economico, subsetor) %>%
  summarise(cor = cor(stocks_returns, bonds_returns))

# count number of segments and stocks, by sub sector
counts_subsector <- df %>%
  group_by(setor_economico, subsetor) %>%
  summarise(segmentos_n = length(unique(segmento)),
            stocks_n = length(unique(symbol)))

# merge data sets "correlation" and "counts" (based on sub sector)
cor_subsector <- cor_subsector %>%
  left_join(counts_subsector, by = c("setor_economico", "subsetor"))

# correlation by segment
cor_segment <- df %>%
  group_by(setor_economico, subsetor, segmento) %>%
  summarise(cor = cor(stocks_returns, bonds_returns))

# count number of stocks, by segment
counts_segment <- df %>%
  group_by(setor_economico, subsetor, segmento) %>%
  summarise(stocks_n = length(unique(symbol)))

# merge data sets "correlation" and "counts" (based on segment)
cor_segment <- cor_segment %>%
  left_join(counts_segment, by = c("setor_economico", "subsetor", "segmento"))

# correlation by individual stocks
cor_stocks <- df %>%
  group_by(symbol) %>%
  mutate(cor = cor(stocks_returns, bonds_returns)) %>%
  select(nome, symbol, setor_economico, subsetor, segmento, cor) %>%
  distinct(symbol, .keep_all = TRUE)

# save objects
save(cor_sector, cor_subsector, cor_segment, cor_stocks, 
     file = here("brazil", "data", "06-correlations.rda"))

########################### EXPLORATORY DATA ANALYSIS ##########################

################################ ECONOMIC SECTOR ###############################

# view highest negative correlations
cor_sector %>%
  arrange(cor)

# view lowest negative correlations
cor_sector %>%
  arrange(desc(cor))

# is there an association between sector correlation and number of sub sectors?
ggplot(data = cor_sector, aes(x = subsetores_n, y = cor)) + 
  geom_point() + 
  geom_smooth()

# is there an association between sector correlation and number of segments?
ggplot(data = cor_sector, aes(x = segmentos_n, y = cor)) + 
  geom_point() + 
  geom_smooth()

# is there an association between sector correlation and number of stocks?
ggplot(data = cor_sector, aes(x = stocks_n, y = cor)) + 
  geom_point() + 
  geom_smooth()

############################## ECONOMIC SUB SECTOR #############################

# view highest negative correlations
cor_subsector %>%
  arrange(cor)

# view highest positive correlations
cor_subsector %>%
  arrange(desc(cor))

# is there an association between sub sector correlation and number of segments?
ggplot(data = cor_subsector, aes(x = segmentos_n, y = cor)) + 
  geom_point() + 
  geom_smooth()

# is there an association between sub sector correlation and number of stocks?
ggplot(data = cor_subsector, aes(x = stocks_n, y = cor)) + 
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

# what are the 3 companies from "saúde / comércio / medicamentos"?
df %>%
  ungroup() %>%
  filter(setor_economico == "Saúde", 
         subsetor == "Comércio e Distribuição",
         segmento == "Medicamentos e Outros Produtos") %>%
  distinct(nome) %>%
  select(nome)

# is there an association between segment correlation and number of stocks?
ggplot(data = cor_segment, aes(x = stocks_n, y = cor)) + 
  geom_point() + 
  geom_smooth()

# who are the outliers?
cor_segment %>%
  arrange(desc(stocks_n))

# same plot as above without outlier "Energia Elétrica"
cor_segment %>%
  filter(segmento != "Energia Elétrica") %>%
  ggplot(aes(x = stocks_n, y = cor)) + 
  geom_point() + 
  geom_smooth()

# same plot as above without 3 outliers
cor_segment %>%
  filter(!segmento %in% c("Energia Elétrica","Incorporações","Bancos")) %>%
  ggplot(aes(x = stocks_n, y = cor)) + 
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

# stocks with the highest correlation to brazilian 10 years bonds
cor_stocks %>%
  filter(cor < quantile(cor_stocks$cor, .25)) %>%
  arrange(cor) %>%
  print(n = Inf)






