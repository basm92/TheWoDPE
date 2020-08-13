library(readxl)
library(tidyverse)
library(lubridate)
library(janitor)
library(cowplot)
source("./Code/classify.R")
source("./Code/aux_get_parl_chars.R")
source("./Code/aux_get_parl_chars_uh.R")

# Clean the data
lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

# Put political party in lh_parliaments
lh_parliaments <- classify(lh_parliaments)

lh_parliaments <- lh_parliaments %>%
  relocate(name, b1_nummer, parliament, 
           achternaam, voorletters, begin_periode,
           einde_periode, class) %>%
  as_tibble()

# Put wealth in lh_parliaments
#tomorrow

# get the summary per parliament
get_parl_chars() -> parlovtime

# create a figure with parliamentary comp over time
#absolute
parlovtime %>%
  group_by(date) %>%
  mutate(total = sum(count)) %>%
  pivot_wider(names_from = "class", values_from = "count") %>%
  pivot_longer(total:neutral, 
               names_to = "class", 
               values_to = "count", 
               values_drop_na = T) %>% 
  ggplot(aes(x = date, 
             y = count, 
             group = class, 
             color = class)) + 
  geom_smooth(span = 0.2, se = F) + 
  ggtitle("Political Color of the Lower House") +
  ylab("Member count") +xlab("Date") + theme_minimal()

#relative
p1 <- parlovtime %>%
  group_by(date) %>%
  mutate(total = sum(count), count = count/total) %>%
  select(-total) %>%
  ggplot(aes(x = date, 
             y = count, 
             group = class, 
             color = class)) + 
  geom_line() +
  theme_minimal() +
  ggtitle("Political Color of the Lower House") +
  ylab("Proportion") +xlab("Date") + theme_minimal()

# create same for upper house
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

# Put political party in lh_parliaments
uh_parliaments <- classify(uh_parliaments)

uh_parliaments <- uh_parliaments %>%
  relocate(name, b1_nummer, parliament, 
           achternaam, voorletters, begin_periode,
           einde_periode, class) %>%
  as_tibble()

parlovtime_uh <- get_parl_chars_uh()

#relative
p2 <- parlovtime_uh %>%
  group_by(date) %>%
  mutate(total = sum(count), count = count/total) %>%
  select(-total) %>%
  ggplot(aes(x = date, 
             y = count, 
             group = class, 
             color = class)) + 
  geom_line() +
  theme_minimal() +
  ggtitle("Political Color of the Upper House") +
  ylab("Proportion") +xlab("Date") + theme_minimal()

cowplot::plot_grid(p1, p2, nrow = 1)
