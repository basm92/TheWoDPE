#Wealth_dep
library(cowplot)
library(readxl)

wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
    janitor::clean_names()

demog <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "RegistrationFile") %>%
    janitor::clean_names() %>%
    filter(grepl("G|C", index_nummer) | grepl("gedeputeerde|commissaris", functie))

demog <- demog %>%
    mutate(tweedekamer = ifelse(grepl("2e kamerlid", functie), "yes", "no"),
           eerstekamer = ifelse(grepl("1e kamerlid", functie), "yes", "no"),
           minister = ifelse(grepl("minister", functie), "yes", "no"))

data <- left_join(demog, wealth, by = c("index_nummer" = "indexnummer")) %>%
    filter(!is.na(w_deflated))

#change the options
options(scipen = 1e8)

p2 <- data %>%
    ggplot(aes(x = as.numeric(jaar_van_overlijden), 
               y = log(w_deflated),
               color = tweedekamer,
               shape = eerstekamer,
               size = minister)) +
    geom_point() +
    theme_classic() + 
    scale_y_continuous(labels = exp,
        breaks = c(log(1000), log(10000), log(50000), log(100000), log(1000000)),
        sec.axis = sec_axis( ~.*1, 
                             name = "Log (Wealth)")) +
    scale_shape_manual(values = c(16,18))+
    scale_size_manual(values = c(3,7))+
    scale_color_manual(values = c("black", "grey")) +
    ylab("Wealth (guilders)")+
    xlab("Year of Death") + 
    ggtitle("Provincial Executives", "Wealth over time")

p2 <- p2 + guides(shape = guide_legend(override.aes = list(size = 3)))

ggsave("./Figures/wealth_dep.png", p2, height = 5, width = 12)

