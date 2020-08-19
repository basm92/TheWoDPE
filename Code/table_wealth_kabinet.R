#Wealth_kabinet_table
library(tidyverse)
library(readxl)
library(xtable)

# Function for ministers
## Function is self-sufficient, imports everything
get_wealth_kabinet <- function(precision = 50) {
  #For Loop over different cabinets with:
  kabinetten <- read_csv("./Data/kabinetten.csv")
  ministers <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx", sheet = 2)%>%
    filter(grepl("^minister van", waarde) | grepl("voorzitter van de ministerraad", waarde)) %>%
    separate(col = datum,into = c("begin", "end"), sep = "/") %>%
    mutate(across(c(begin, end), lubridate::dmy))
  
  #make a list to store results
  results <- vector("list", length(kabinetten$namegovt))
  
  for(i in 1:length(kabinetten$namegovt)){
    
    begindate <- lubridate::dmy(kabinetten$arrival[i])
    enddate <- lubridate::dmy(kabinetten$resign[i])
    # Start with professions (sheet 2)
    #Filter on date
    #Extract the b1_nummers
    query <- ministers %>%
      filter(end >= begindate+precision, begin <= enddate-precision)
    #Filter wealth dataset on indexnummer %in% b1 nummer
    wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
      filter(Indexnummer %in% query$`b1-nummer`)
    
    query <- left_join(query, wealth, by = c("b1-nummer" = "Indexnummer"))
    
    results[[i]] <- query %>%
      mutate(govt = kabinetten$namegovt[i]) %>%
      distinct(`b1-nummer`, waarde, .keep_all = T)
    
    # Summarize and compute avg. and median wealth
  }
  
  #combine everything
  purrr::reduce(results, rbind) %>%
    mutate(govt = as.factor(govt)) %>%
    janitor::clean_names()
  
}

#Get the data
get_wealth_kabinet() -> wealthreg

#Preserve factor level
wealthreg$govt <- factor(wealthreg$govt, levels = unique(wealthreg$govt))

#add prime minister dummy
wealthreg <- wealthreg %>%
  group_by(govt) %>%
  mutate(premier = ifelse(grepl("voorzitter", waarde), 1, 0))

#Summarize and clean
wealthpm <- wealthreg %>%
  group_by(govt) %>%
  filter(premier == 1) %>%
  select(govt, w_deflated) %>%
  unique()

table <- wealthreg %>%
  group_by(govt) %>%
  distinct(b1_nummer, .keep_all = T) %>%
  summarize(Mean = mean(w_deflated, na.rm = T), 
            Median = median(w_deflated, na.rm = T),
            SD = sd(w_deflated, na.rm = T),
            N = sum(!is.na(w_deflated))) %>%
  left_join(wealthpm, by.x = govt, by.y = govt) %>%
  rename(Government = govt, WealthPM = w_deflated)

test <- xtable(table, 
               caption = "Average Wealth of Governments",
               digits = 0)

print(test, include.rownames = FALSE, file = "./Tables/table_wealth_kabinet.tex")