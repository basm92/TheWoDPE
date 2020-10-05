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
  summarize(median = median(w_deflated, na.rm = T),
            p25 = quantile(w_deflated, 0.25, na.rm = T),
            p75 = quantile(w_deflated, 0.75, na.rm = T),
            p90 = quantile(w_deflated, 0.90, na.rm = T),
            count = sum(!is.na(w_deflated)))

p1 <- meanmedw_lh %>%
 pivot_longer(c(median, p25, p75, p90),
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
  theme(axis.text.x = element_text(angle = 45), legend.position = c(0.9, 0.9)) +
  ggtitle("Lower House", "Avg. and Median Wealth per Standing") + 
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(0,12e5))

#now, read uh
uh_parliaments <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  select(-1)

uh_parliaments <- left_join(uh_parliaments, wealth,
                            by = c("b1_nummer" = "indexnummer"))

meanmedw_uh <- uh_parliaments %>%
  group_by(parliament) %>%
  summarize(median = median(w_deflated, na.rm = T),     
            p25 = quantile(w_deflated, 0.25, na.rm = T),
            p75 = quantile(w_deflated, 0.75, na.rm = T),
            p90 = quantile(w_deflated, 0.90, na.rm = T),
            count = sum(!is.na(w_deflated)))

p2 <- meanmedw_uh %>%
  pivot_longer(c(median,p25, p75, p90),
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
                     limits = c(0, 20e5)) +
theme(legend.position = "none")


fig <- cowplot::plot_grid(p1, p2, nrow = 2, rel_heights = c(45,55))

ggsave("./Figures/step5fig2wealthperparl.png", fig, width = 7.41, height = 10)


## Histogram of both (before 1900, after 1900)
hist_lh <- lh_parliaments %>%
  mutate(period = as.numeric(str_extract(parliament, "\\d{4}$")) > 1901) %>%
  mutate(period = ifelse(period == TRUE, "After 1900", "Before 1900")) %>%
  mutate(period = factor(period, levels = c("Before 1900", "After 1900"))) %>%
  group_by(period) %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  ggplot(aes(x = log(w_deflated))) + 
  geom_histogram() + 
  facet_grid(. ~ period) +
  xlab("Log(Wealth)") +
  ylab("Frequency") +
  xlim(5,16)+
  ggtitle("Distribution of Wealth at Death", subtitle = "Lower House") +
  theme_light()

hist_uh <- uh_parliaments %>%
  mutate(period = as.numeric(str_extract(parliament, "\\d{4}$")) > 1901) %>%
  mutate(period = ifelse(period == TRUE, "After 1900", "Before 1900")) %>%
  mutate(period = factor(period, levels = c("Before 1900", "After 1900"))) %>%
  group_by(period) %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  ggplot(aes(x = log(w_deflated))) + 
  geom_histogram() + 
  facet_grid(. ~ period) +
  xlab("Log(Wealth)") +
  ylab("Frequency") +
  ylim(0,25)+
  xlim(5, 16)+
  ggtitle("Distribution of Wealth at Death", subtitle = "Upper House") +
  theme_light()

p3 <- cowplot::plot_grid(hist_lh, hist_uh, nrow = 2)

ggsave(p3, filename = "Figures/Histogram_wealth_per_parl.png")

