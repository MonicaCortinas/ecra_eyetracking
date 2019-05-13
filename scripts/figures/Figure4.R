library(tidyverse)
library(here)
library(kableExtra)
library(cowplot)

load(here("data","qualtrics_data"))

# **Figure 1.** Seconds. BoxPlot

df_figure1<- qualtrics_data %>%
  rename(id="Response ID") %>%
  dplyr::select(id, TareaZapatillas, TareaMovil, TareaBolis,TareaHDS, 
                secZapa, secTele,secBoli, secHd)


task_long <- df_figure1 %>%
  gather(store, task ,TareaZapatillas:TareaHDS) %>%
  mutate(task=recode(task, "a"="task1_exp",
                      "b"="task2_search",
                      "c"="task3_purch",
                      "d"="task4_post")) %>%
  mutate(store=recode(store, "TareaZapatillas"="sport_shoes_store",
                      "TareaMovil"="mobile_store",
                      "TareaBolis"="pen_store",
                      "TareaHDS"="hd_store")) %>%
  dplyr::select(id, store, task)

seconds_long <- df_figure1 %>%
  gather(store, seconds ,secZapa:secHd) %>%
  mutate(store=recode(store, "secZapa"="sport_shoes_store",
                      "secTele"="mobile_store",
                      "secBoli"="pen_store",
                      "secHd"="hd_store")) %>%
  dplyr::select(id, store, seconds)

df_long_figure1 <- left_join(task_long, seconds_long)

rm(df_figure1,task_long, seconds_long)


df_long_figure1$task<- factor(df_long_figure1$task, levels=c("task1_exp", 
                                                     "task2_search",
                                                     "task3_purch",
                                                     "task4_post"))

p1 <- ggplot(df_long_figure1, aes(x=task, y=seconds)) + 
  geom_boxplot() + labs(title="Time spent on each task",x="Task", y = "Seconds")+
  theme_light(base_size = 8)+
  theme(plot.title = element_text(hjust = 0.5))


ggsave(here("figures", "Figure4.pdf"), plot = plot_grid(p1), 
       scale = 1, width = 6, height = 4, 
       dpi = 600)

ggsave(here("figures", "Figure4.png"), plot = plot_grid(p1), 
       scale = 1, width = 6, height = 4, 
       dpi = 600)
