# read wealth and lh
library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)
source("./Code/classify.R")


wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

lh_parliaments <- classify(lh_parliaments)

meanmedw_lh <- lh_parliaments %>%
  group_by(parliament, class) %>%
  summarize(median = median(w_deflated, na.rm = T))

p1 <- meanmedw_lh %>%
  filter(class != "neutral") %>%
  pivot_longer(median,
               names_to = "Statistic", 
               values_to = "Wealth") %>%
  ggplot(aes(x = parliament, 
             y = Wealth, 
             group = class, 
             color = class)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Parliament") +
  ylab("Wealth (guilders)") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Lower House", "Median Wealth per Standing") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,8e5)) +
  scale_color_manual(values = c("blue", "orange", "red"))

p1 <- p1 + theme(legend.position = c(0.9, 0.9))

#now, read uh
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))


uh_parliaments <- classify(uh_parliaments)


meanmedw_uh <- uh_parliaments %>%
  group_by(parliament, class) %>%
  summarize(median = median(w_deflated, na.rm = T))

p2 <- meanmedw_uh %>%
  pivot_longer(median,
               names_to = "Statistic", 
               values_to = "Wealth") %>%
  ggplot(aes(x = parliament, 
             y = Wealth, 
             group = class, 
             color = class)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Parliament") +
  ylab("Wealth (guilders)") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Upper House", "Median Wealth per Standing") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits = c(0, 8e5)) +
  theme(legend.position = c(0.9, 0.9)) + 
  scale_color_manual(values = c("blue", "orange", "red"))


fig <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(45,55))

ggsave("./Figures/step8fig2wealthperparlperparty.png", fig, width = 7.41, height = 13)

