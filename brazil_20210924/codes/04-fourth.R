
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

# install packages if they are not installed and load them
p_load(here, tidyverse, lubridate, tidyquant)

# load data
load(here("brazil_20210924", "data", "ibov_1968_2020_tidy.rda"))

# build data set with year dropdawn
df <- ibov %>%
  group_by(year(date)) %>%
  summarise(min = min(ibov, na.rm = TRUE),
            max = max(ibov, na.rm = TRUE),
            dropdawn = min / max - 1) %>%
  rename(year = 'year(date)')

# build data set with annual returns
annual_returns <- ibov %>%
    tq_transmute(select = ibov,          
               mutate_fun = periodReturn,   
               period = "annually",     
               col_rename = "returns") %>%
  mutate(year = seq(1968, 2020)) %>%
  select(year, returns)

# merge dropdawn and annual returns
all <- df %>%
  left_join(annual_returns, by = "year") %>%
  filter(!is.na(returns))

# plot graph
cols <- c("Retornos anuais"="royalblue3")
all %>% 
  filter(year > 1994) %>%
  ggplot(aes(x = year)) + 
  geom_col(aes(y = returns, fill = "Retornos anuais")) +
  geom_point(aes(y = dropdawn, shape = "Dropdawns"), color = "red") + 
  labs(x = "Ano",
       y = "",
       title = "Retornos e dropdawns anuais do Ibovespa, de 1995 a 2020",
       caption = "Fonte: B3") + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(1995, 2020, 5)) + 
  scale_y_continuous(labels = scales::percent) + 
  scale_fill_manual(values=cols) + 
  scale_color_manual(values=cols) + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

# average dropdawn
all %>%
  filter(year > 1994) %>%
  summarise(min = min(dropdawn),
            q1 = quantile(dropdawn, .25),
            median = median(dropdawn),
            mean = mean(dropdawn),
            q3 = quantile(dropdawn, .75),
            max = max(dropdawn))

# plot dropdawn histogram
all %>%
  filter(year > 1994) %>%
  ggplot(aes(x = dropdawn)) + 
  geom_histogram(bins=9, fill = "wheat", color= "black") + 
  scale_x_continuous(breaks = seq(0, -.8, by = -.1),
                     labels = scales::percent_format(accuracy = 1L)) + 
  labs(x = "Dropdawn",
       y = "Frequência",
       title = "Distribuição de frequência (histograma) dos dropdawns anuais \ndo Ibovespa, de 1994 a 2020",
       caption = "Fonte: B3")
 

