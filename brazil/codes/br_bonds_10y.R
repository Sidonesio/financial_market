
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
p_load(tidyverse, here, janitor, lubridate)

# read Brazilian bonds data
br_bonds_10y <- read_csv(here("brazil", "data", "br_bonds_10y.csv"))

# convert column names to machine readable format
br_bonds_10y <- br_bonds_10y %>%
  clean_names()

# understand data
glimpse(br_bonds_10y)

# fix variable "data"
# convert variable "data" to date format
br_bonds_10y <- br_bonds_10y %>%
  mutate(data = dmy(data))

# remove symbol "%"
br_bonds_10y <- br_bonds_10y %>%
  mutate(var_percent = str_sub(var_percent, 1, nchar(var_percent) - 1))

# convert comma by dot
br_bonds_10y <- br_bonds_10y %>%
  mutate(var_percent = gsub(",", "\\.", br_bonds_10y$var_percent))

# convert character class to numerical
br_bonds_10y$var_percent <- as.numeric(br_bonds_10y$var_percent)

# rename columns
br_bonds_10y <- br_bonds_10y %>%
  rename(date = data, 
         bonds_returns = var_percent)

# plot br_bonds_10y
ggplot(br_bonds_10y, aes(x = date, y = ultimo)) + 
  geom_line(color = "royalblue4") + 
  labs(x = "Date",
       y = "Price",
       title = "Price of 10 years Brazilian Bonds") + 
  theme_classic()



