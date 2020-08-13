#Wealth_dep
library(cowplot)

wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
    janitor::clean_names() %>%
    filter(grepl("G|C", indexnummer))

demog <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "RegistrationFile") %>%
    janitor::clean_names() %>%
    filter(grepl("G|C", index_nummer))

data <- left_join(demog, wealth, by = c("index_nummer" = "indexnummer")) %>%
    filter(!is.na(w_deflated))

p2 <- data %>%
    ggplot(aes(x = as.numeric(jaar_van_overlijden), 
               y = log(w_deflated))) + 
    geom_point() +
    theme_classic() + 
    ylab("Log(Wealth)")+
    xlab("Year of Death") + 
    ggtitle("Provincial Executives", "Wealth over time")

p2

ggsave("./Figures/wealth_dep.png", p2)

