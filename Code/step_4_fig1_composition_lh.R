library(readxl)
library(tidyverse)
library(lubridate)


# Clean the data
polparty <- read.csv("./Data/key_politicalparty_category.csv") %>%
  select(-1)

lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

# Put political party in lh_parliaments
lh_parliaments <- merge(lh_parliaments, polparty, 
      by.x = "partij_en_fractie_s",
      by.y = "partys")

lh_parliaments <- lh_parliaments %>%
  relocate(name, b1_nummer, parliament, 
           achternaam, voorletters, begin_periode,
           einde_periode, class) %>%
  as_tibble()

# Put wealth in lh_parliaments
#tomorrow

# get the summary per parliament
source("./Code/aux_get_parl_chars.R")

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
  ggtitle("Political Color of the Lower House (1870-1922)") +
  ylab("Member count") +xlab("Date") + theme_minimal()

#relative
parlovtime %>%
  group_by(date) %>%
  mutate(total = sum(count), count = count/total) %>%
  select(-total) %>%
  ggplot(aes(x = date, 
             y = count, 
             group = class, 
             color = class)) + 
  geom_line() +
  theme_minimal() +
  ggtitle("Political Color of the Lower House (1870-1922)") +
  ylab("Proportion") +xlab("Date") + theme_minimal()
  

