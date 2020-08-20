#Gini coeff
# read wealth and lh
library(readxl)
library(tidyverse)
library(janitor)
library(xtable)
library(cowplot)

wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

#Create gini
gini_lh <- lh_parliaments %>%
  group_by(parliament) %>%
  filter(w_deflated > 0) %>%
  summarize(min = min(w_deflated, na.rm = TRUE),
            p25 = quantile(w_deflated, 0.25, na.rm = TRUE),
            p75 = quantile(w_deflated, 0.75, na.rm = TRUE),
            max = max(w_deflated, na.rm = TRUE),
            gini = Gini(w_deflated, na.rm = TRUE))

#now, read uh
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

#Create gini
gini_uh <- uh_parliaments %>%
  group_by(parliament) %>%
  filter(w_deflated > 0) %>%
  summarize(min = min(w_deflated, na.rm = TRUE),
            p25 = quantile(w_deflated, 0.25, na.rm = TRUE),
            p75 = quantile(w_deflated, 0.75, na.rm = TRUE),
            max = max(w_deflated, na.rm = TRUE),
            gini = Gini(w_deflated, na.rm = TRUE))


#Make the tables
kinds <- list(gini_lh, gini_uh)
attr(kinds, "subheadings") <- paste0("Panel ", c("A", "B"), ":", c("Lower House", "Upper House"))

kinds <- xtableList(kinds, 
                    caption = "Wealth Distribution over time",
                    label = "tab:ginicoef")

print.xtableList(kinds, 
                 file = "./Tables/ginicoef.tex",
                 colnames.format = "multiple", 
                 include.rownames = F)

