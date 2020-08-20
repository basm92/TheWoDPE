# richestpols table

library(readxl)
library(tidyverse)
library(xtable)
library(janitor)
source("./Code/classify.R")

#import wealth
wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

#### First lower house
lh <- read_csv("./Data/lh_parliaments.csv") %>%
  select(-1) %>%
  distinct(b1_nummer, .keep_all = TRUE)

richest_lh <- left_join(lh, wealth, 
          by = c("b1_nummer" = "indexnummer")) %>%
  filter(w_deflated >= 0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(1:10) %>%
  select(Name, w_deflated)

### Now upper house
uh <- read_csv("./Data/uh_parliaments.csv") %>%
  select(-1) %>%
  distinct(b1_nummer, .keep_all = TRUE)

richest_uh <- left_join(uh, wealth,
          by = c("b1_nummer" = "indexnummer")) %>%
  filter(w_deflated >=0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(1:10) %>%
  select(Name, w_deflated)

### Now ministers
min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names() %>%
  distinct()

richest_min <- left_join(min, wealth,
          by = c("b1_nummer" = "indexnummer")) %>%
  filter(w_deflated >= 0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(2:11) %>%
  select(voorna_a_m_en, achternaam, w_deflated) %>%
  unite("Name", c(voorna_a_m_en, achternaam), 
        sep = " ", 
        na.rm = TRUE)

### Now provincials
dep <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "RegistrationFile") %>%
  janitor::clean_names() %>%
  filter(grepl("G|C", index_nummer) | grepl("gedeputeerde|commissaris", functie))

richest_dep <- left_join(dep, wealth,
          by = c("index_nummer" = "indexnummer")) %>%
  filter(w_deflated >= 0) %>%
  .[order(.$w_deflated, decreasing = TRUE),] %>%
  slice(1:10) %>%
  select(voorletters, achternaam, w_deflated) %>%
  unite("Name", c(voorletters, achternaam), sep = " ", na.rm = TRUE)


## Make a table
kinds <- list(richest_lh, richest_uh, richest_min, richest_dep) %>%
  lapply(rename, Wealth = w_deflated)

attr(kinds, "subheadings") <- paste0("Panel ", 
                                     c("A", "B", "C", "D"), 
                                     ": ", 
                                     c("Lower House", "Upper House", "Ministers", "Provincial Executives"))

kinds <- xtableList(kinds, 
                    caption = "10 Richest Politicians in each Function",
                    label = "tab:richestpols")

print.xtableList(kinds,
                 size = "footnotesize",
                 file = "./Tables/richestpols.tex",
                 colnames.format = "multiple", 
                 include.rownames = F)
