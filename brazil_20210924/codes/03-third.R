
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
p_load(here, tidyverse, lubridate)

# load data
load(here("brazil_20210924", "data", "ibov_1968_1997_tidy.rda"))
load(here("brazil_20210924", "data", "ibov_1998_2020_tidy.rda"))

# join both data sets
ibov <- rbind(ibov1, ibov2)

# save object
save(ibov, file = here("brazil_20210924", "data", "ibov_1968_2020_tidy.rda"))

# plot "ibov" since year 1995
ibov %>%
  drop_na() %>%
  filter(year(date) > 1994) %>%
  ggplot(aes(x = date, y = ibov)) + 
  geom_line()

