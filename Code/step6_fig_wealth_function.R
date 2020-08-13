#welath naar affil: eerste en tweede kamer
library(readxl)
library(tidyverse)
library(gridExtra)
library(cowplot)   
source("./Code/classify.R")
# Read data
polparty <- read_csv("Data/key_politicalparty_category.csv") %>%
  select(-1)

wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

#LH and clean
lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

lh_parliaments <- classify(lh_parliaments)

#UH and clean
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

uh_parliaments <- classify(uh_parliaments)

#Min and clean
ministers <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names()

ministers <- merge(ministers, wealth, 
      by.x = "b1_nummer", 
      by.y = "indexnummer") %>%
  as_tibble()

ministers <- classify(ministers)

#Dep and clean
gedeputeerden <- wealth %>%
  filter(grepl("G|C", indexnummer))

# Now, the graphs

p_lh <- lh_parliaments %>%
  mutate(w_deflated = log(w_deflated)) %>%
  distinct(name, .keep_all = T) %>%
  ggplot(aes(x = class, y = w_deflated)) + 
  geom_boxplot() +
  coord_flip() +
  ggtitle("Lower House") +
  xlab("Political orientaiton") + 
  ylab("log(Wealth)") + 
  theme_classic() +
  ylim(7,16)

p_uh <- uh_parliaments %>%
  mutate(w_deflated = log(w_deflated)) %>%
  distinct(name, .keep_all = T) %>%
  ggplot(aes(x = class, y = w_deflated)) + 
  geom_boxplot() +
  coord_flip() +
  ggtitle("Upper House") +
  xlab("") + 
  ylab("log(Wealth)") + 
  theme_classic() +
  ylim(7,16)

p_min <- ministers %>%
  filter(class == "confessional" | class == "liberal") %>%
  mutate(w_deflated = log(w_deflated)) %>%
  ggplot(aes(x = class, y = w_deflated)) + 
  geom_boxplot() +
  coord_flip() +
  ggtitle("Ministers") +
  xlab("Political orientaiton") + 
  ylab("log(Wealth)") + 
  theme_classic() +
  ylim(7,16)
  

p_dep <- gedeputeerden %>%
  mutate(w_deflated = log(w_deflated)) %>%
  ggplot(aes(y = w_deflated)) + 
  geom_boxplot(width=0.5) +
  coord_flip() +
  ggtitle("Gedeputeerden") +
  #xlab("Political orientaiton") + 
  ylab("log(Wealth)") + 
  theme_classic() +
  ylim(7,16) + 
  scale_x_continuous(
    limits = c(-0.75, 0.75), 
    breaks = NULL)

#grid.arrange(p_lh, p_uh, p_min, p_dep, ncol = 2)
plot_grid(p_lh, p_uh, p_min, p_dep, ncol = 2, align = "v")
