library(tidyverse)
library(here)
library(kableExtra)

load(here("data","qualtrics_data"))

# **Table 1.** Descriptive statistics

summary <- qualtrics_data %>%
  select(14, 33, 52, 71,
         15, 34, 53, 72, 
         16, 35, 54, 73, 
         80, 81)

summ <- summary %>%
  summarize_all(mean) 
sumsd <- summary %>%
  summarize_all(sd) 
summin <- summary %>%
  summarize_all(min) 
summax <- summary %>%
  summarize_all(max) 


fullname<- c("Knowledge_Sport shoes", 
             "Knowledge_Mobile phones", 
             "Knowledge_Ball-point pens", 
             "Knowledge_Hard disks", 
             "Importance_Sport shoes", 
             "Importance_Mobile phones", 
             "Importance_Ball-point pens", 
             "Importance_Hard disks", 
             "Interest_Sport Shoes", 
             "Interest_Mobile phones", 
             "Interest_Ball-point pens", 
             "Interest_Hard disks", 
             "Ease with online information seeking", 
             "Ease with online shopping")

summ_stat <- tibble(Variable=fullname, "Mean"= t(round(summ, digits = 3)), 
                    "SD"=t(round(sumsd, digits = 3)),"Min."= t(summin), "Max"= t(summax))

rm(summ,sumsd,summin,summax)

#### Table 1

knitr::kable(summ_stat, row.names = FALSE) %>% 
  group_rows("Knowledge", 1, 4) %>%
  group_rows("Importance", 5, 8) %>%
  group_rows("Interest", 9, 12)


