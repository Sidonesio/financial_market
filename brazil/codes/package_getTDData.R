
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
p_load(GetTDData, tidyverse, janitor, here)

################################## YIELD CURVE #################################

# get yield curve
yield_2021_09_20 <- get.yield.curve() %>%
  as_tibble()

# convert column names to machine readable format
yield_2021_09_20 <- yield_2021_09_20 %>%
  clean_names()

# take a look at data
str(yield_2021_09_20)

# plot graph
yield_2021_09_20 %>%
  mutate(type = recode(type,
                       'real_return' = "Real return",
                       'nominal_return' = "Nominal return",
                       'implicit_inflation' = "Implicit Inflation")) %>%
  ggplot(aes(x=ref_date, y = value) ) +
  geom_line(color = "royalblue4") + 
  geom_point(color = "royalblue4") + 
  facet_grid(~type, scales = 'free') + 
  labs(title = paste0("The current Brazilian Yield Curve "),
       subtitle = paste0("Date: ", yield_2021_09_20$current_date[1]),
       x = "Reference Date",
       y = "Value") + 
  theme_bw()

################################ DOWNLOAD DATA #################################

# download excel files from website "Tesouro Direto"
download.TD.data(asset.codes = 
                  c("LFT", "LTN", "NTN-C", "NTN-B", "NTN-B Principal", "NTN-F"), 
                  dl.folder = here("brazil", "data", "tesouro_direto"))

################################### READ DATA ##################################

# read data
ntnb <- read.TD.files(dl.folder = here("brazil", "data", "tesouro_direto"), 
                      asset.codes = "NTN-B") %>%
  as_tibble()

# convert column names into a machine readable format
ntnb <- ntnb %>%
  clean_names()

# understand data
glimpse(ntnb)
head(ntnb)
tail(ntnb)

# view bonds and maturity year
ntnb %>%
  group_by(asset_code) %>%
  summarise(maturity_year = year(matur_date)) %>%
  distinct(asset_code, maturity_year) %>%
  arrange(maturity_year) %>%
  print(n = Inf)

# subset just "NTN-B"
ntnbs <- ntnb %>%
  filter(!grepl("Principal", asset_code))

# create variable that expresses the difference in years
# between reference date and maturity date
ntnbs <- ntnbs %>%
  mutate(diff_years = interval(ref_date, matur_date) %>% as.numeric("years"),
         diff_years_round = round(diff_years, 0))

# plot graph for the whole period
ntnbs %>%
  filter(diff_years_round %in% c(7:13)) %>%
  arrange(ref_date) %>%
  distinct(ref_date, .keep_all = TRUE) %>%
  ggplot(aes(x = ref_date, y = yield_bid)) + 
  geom_line()

# data set that comprises the period that I am interested in (3 years)
real_return <- ntnbs %>%
  filter(ref_date >= "2018-09-16",
         diff_years_round %in% c(7:13)) %>%
  arrange(ref_date) %>%
  distinct(ref_date, .keep_all = TRUE)

# plot graph for the period that I am interested in
ggplot(real_return, aes(x = ref_date, y = yield_bid)) + 
  geom_line(color = "royalblue4") + 
  labs(x = "Reference Date",
       y = "Yield",
       title = "Real returns for 10 years Brazilian bonds") + 
  scale_y_continuous(labels=scales::percent) + 
  theme_classic()

# save object
save(real_return, file = here("brazil", "data", "bonds_10y_real_return.rda"))

################################################################################

