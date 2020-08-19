###Portfolio_composition_table
#in both panel 1, panel 2, according to political FUNCTION
#welath naar affil: eerste en tweede kamer
library(readxl)
library(tidyverse)
source("./Code/classify.R")

## Functions
get_shares <- function(df) {
  df %>%
    rowwise() %>%
    mutate(total = sum(across(re:misc), na.rm = T),
           re_share = re/total,
           shares_share = (fosh+dush)/total,
           bonds_share = (foprbo + duprbo + fogobo + dugobo)/total, 
           misc_share = (cash + misc)/total)
}

get_summary <- function(df) {
  
  if (!all(c("re_share", "shares_share") %in% names(df))) {
    stop("`df` must contain re_share and shares_share columns")
  }
  
  df %>%
    dplyr::summarize(mean_re = mean(re_share,na.rm = T),
                     mean_shares = mean(shares_share, na.rm = T),
                     mean_bonds = mean(bonds_share, na.rm =T),
                     mean_misc = mean(misc_share, na.rm = T),
                     n = sum(!is.na(re_share)))
}

append <- function(df, text) {
  if(!is.character(text)){
    stop("text must be text.")
  }

  if("class" %in% names(df)){
  df %>%
    mutate(House = text) %>%
    relocate(House, class, before_after)
  } else{
    df %>%
      mutate(House = text, class = "-") %>%
      relocate(House, class, before_after) %>%
      na.omit()
  }
  
}

append2 <- function(df, text) {
  
  if(!is.character(text)){
    stop("text must be text.")
  }
  
  df %>%
    mutate(House = text) %>%
    relocate(House, class)
  
}

      
## Data cleaning
wealth <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "Analysis") %>%
  clean_names()

wealth <- get_shares(wealth) %>%
  select(indexnummer, re_share, shares_share, bonds_share, misc_share)

#functie year
## lh
lh <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

lh <- lh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

lh <- lh %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Lower House")

##uh
uh <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

uh <- uh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

uh <- uh %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Upper House")

##min
min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names() %>%
  classify() %>%
  filter(class != "") %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

min <- merge(min, wealth, 
      by.x = "b1_nummer",
      by.y = "indexnummer") %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

min <- min %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Ministers")

##dep
dep <- read_xlsx("./Data/AnalysisFile.xlsx", sheet = "RegistrationFile") %>%
  clean_names() %>%
  filter(grepl("C|G", index_nummer)) %>%
  select(index_nummer, beginyr, endyr) %>%
  left_join(wealth, by = c("index_nummer" = "indexnummer")) %>%
  mutate(before_after = ifelse(beginyr > "1900-01-01", "After 1900", "Before 1900"))

dep <- dep %>%
  group_by(before_after) %>%
  get_summary() %>%
  append("Provincial Executives")

table <- bind_rows(lh, uh, min, dep) 

maybetable <- table %>%
  select(-class, -n) %>% #-n is necessary for this table, otherwise no pivot
  pivot_longer(mean_re:mean_misc) %>%
  pivot_wider(names_from = c(before_after, name), values_from = value) %>%
  relocate(House, contains("Before"), contains("After"))


#functie party
lh <- read_csv("./Data/lh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

lh <- lh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

lh <- lh %>%
  group_by(class) %>%
  get_summary() %>%
  append2("Lower House")

uh <- read_csv("./Data/uh_parliaments.csv") %>%
  clean_names() %>%
  classify() %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

uh <- uh %>%
  left_join(wealth, 
            by = c("b1_nummer"="indexnummer")) %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

uh <- uh %>%
  group_by(class) %>%
  get_summary() %>%
  append2("Upper House")

min <- read_xlsx("./Data/bewindslieden_1815tot1950_uu.xlsx") %>%
  clean_names() %>%
  classify() %>%
  filter(class != "") %>%
  distinct(b1_nummer, .keep_all = TRUE) %>%
  select(b1_nummer, begin_periode, einde_periode, class)

min <- merge(min, wealth, 
             by.x = "b1_nummer",
             by.y = "indexnummer") %>%
  mutate(before_after = ifelse(begin_periode > "1900-01-01", "After 1900", "Before 1900"))

min <- min %>%
  group_by(class) %>%
  get_summary() %>%
  append2("Ministers")


#Now make the tables
table1_1 <- table %>%
  select(-class) %>%
  filter(before_after == "Before 1900") %>%
  select(-before_after) %>%
  xtable(caption = "Before 1900", label = "fig:portcomp1_1")

table1_2 <- table %>%
  select(-class) %>%
  filter(before_after == "After 1900") %>%
  select(-before_after) %>%
  xtable(caption = "After 1900", label = "fig:portcomp1_2")

table2 <- bind_rows(lh, uh, min)

table2 <- xtable(table2, 
                 caption = "Portfolio Composition according to Political Color",
                 label = "fig:portcomp2") 

print.xtable(table1_1, file = "./Tables/portcomp1_1.tex", 
             include.rownames = FALSE)
print.xtable(table1_2, file = "./Tables/portcomp1_2.tex", 
             include.rownames = FALSE)
print.xtable(table2, file = "./Tables/portcomp_2.tex", 
             size = "small",
             include.rownames = FALSE)

#kable(table2, format = "latex", booktabs = T) %>%
#  kable_styling() %>%
#  add_footnote("Footnote 1")

#library(kableExtra)
