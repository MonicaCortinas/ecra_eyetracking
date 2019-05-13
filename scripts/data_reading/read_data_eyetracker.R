library(tidyverse)
library(here)
library(readxl)
library(purrr)

## Eye tracker 

### We have the data in Excel with order of appearance and time. 
### We have so many archives as individual x number of task
### We create a whole data frame


# List file names
file.list <- list.files(here("data/dataeyetracker"),pattern="*.xls*", full.names = TRUE)
file.list <- setNames(file.list, file.list) # only needed when you need an id-column with the file-names

# read excel on the whole list and put it in one dataframe
df <- purrr::map_df(file.list, read_excel, .id = "id", col_names =FALSE)

# New names:
names(df)<- c("id", "order", "zone", "fixations")

# Extract subject ID

# We need to remove these patterns from the ID Column

pattern <- "D:/Dropbox/2_ Investigacion/2019_ECRA/secondrevision/data/dataeyetracker/"
pattern2 <-"_tarea\\d\\.xlsx?$"
newdf <- df %>%
  mutate(subject = str_remove(id, pattern)) %>%
  mutate(subject = str_remove(subject, pattern2)) 



# Extract STORE information

pattern3 <-"_tarea\\d"
pattern4 <-"_"

newdf<- mutate(newdf,
                store=str_extract(id, pattern3))%>%
        mutate(store=str_remove(store, pattern4))%>%
        mutate(store=str_replace(store, "tarea", "store"))

## Summary: fixations by subject

by_subject <- newdf %>%
  group_by(subject) %>%
  summarize(count=n(), total_fixations_subject=sum(fixations))
## Resumen por tienda

## By store

by_store <- newdf %>%
  group_by(store) %>%
  summarize(count=n(), total_fixations_store=sum(fixations))

# By zone

by_zone <- newdf %>%
  group_by(zone) %>%
  summarize(count=n(), total_fixations_zone=sum(fixations))

eye_tracker_data <- newdf  %>%
  select(id, subject, store, order, zone, fixations)

rm(list=setdiff(ls(), "eye_tracker_data"))

save(eye_tracker_data,file=here("data", "eye_tracker_data"))

