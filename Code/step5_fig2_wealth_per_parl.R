# read wealth and lh
library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)


wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

lh_parliaments <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

lh_parliaments <- left_join(lh_parliaments, wealth,
          by = c("b1_nummer" = "indexnummer"))

meanmedw_lh <- lh_parliaments %>%
  group_by(parliament) %>%
  summarize(mean = mean(w_deflated, na.rm = T), 
            median = median(w_deflated, na.rm = T),
            count = sum(!is.na(w_deflated)))

p1 <- meanmedw_lh %>%
 pivot_longer(mean:median, 
              names_to = "Statistic", 
              values_to = "Wealth") %>%
  ggplot(aes(x = parliament, 
             y = Wealth, 
             group = Statistic, 
             color = Statistic)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Parliament") +
  ylab("Wealth (guilders)") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Lower House", "Avg. and Median Wealth per Standing") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,8e5))

p1 <- p1 + theme(legend.position = "none")

#now, read uh
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

meanmedw_uh <- uh_parliaments %>%
  group_by(parliament) %>%
  summarize(mean = mean(w_deflated, na.rm = T), 
            median = median(w_deflated, na.rm = T),
            count = sum(!is.na(w_deflated)))

p2 <- meanmedw_uh %>%
  pivot_longer(mean:median, 
               names_to = "Statistic", 
               values_to = "Wealth") %>%
  ggplot(aes(x = parliament, 
             y = Wealth, 
             group = Statistic, 
             color = Statistic)) + 
  geom_line() + 
  theme_minimal() + 
  xlab("Parliament") +
  ylab("Wealth (guilders)") +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("Upper House", "Avg. and Median Wealth per Standing") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits = c(0, 8e5))


fig <- cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(45,55))

ggsave("./Figures/step5fig2wealthperparl.png", fig)

