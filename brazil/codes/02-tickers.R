
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
p_load(rvest, tidyverse, here)

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

# save object
save(tickers, file = here("brazil", "data", "02-tickers_today.rda"))

################################################################################

