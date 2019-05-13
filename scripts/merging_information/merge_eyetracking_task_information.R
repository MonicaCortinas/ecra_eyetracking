

### Cargar data frames ############

load(here("data","qualtrics_data"))
load(here("data","eye_tracker_data"))


# [9] "Response ID" en tidyqualtrics_data
# [2] "subject" en eye_tracker_data

### Fusión dataframes

tidy_df <- merge(x=eye_tracker_data, y= qualtrics_data, by.x = "subject", by.y = "Response ID", 
             all = FALSE, all.x = TRUE, all.y = FALSE,
             sort = FALSE) 

tidy_df<- tidy_df %>%
  mutate(task= case_when(store=="store1" ~ TareaZapatillas,
                          store=="store2" ~ TareaMovil,
                          store=="store3" ~ TareaBolis,
                          store=="store4" ~ TareaHDS))
tidy_df<- tidy_df %>%
  mutate(tot_seconds_task= case_when(store=="store1" ~ secZapa,
                             store=="store2" ~ secTele,
                             store=="store3" ~ secBoli,
                             store=="store4" ~ secHd))
tidy_df<- tidy_df %>%
  mutate(task=recode(task, "a"="task1_exp",
                "b"="task2_search",
                "c"="task3_purch",
                "d"="task4_post"))

eye_tracker_seconds_task_df<- tidy_df %>%
  select(subject, store, task, tot_seconds_task, order, zone, fixations)


rm(ls=eye_tracker_data,qualtrics_data, tidy_df)

save(eye_tracker_seconds_task_df, file=here("data","eye_tracker_seconds_task_df"))