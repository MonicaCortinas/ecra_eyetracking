library(tidyverse)
library(here)
library(kableExtra)
library(cowplot)

load(here("data","eye_tracker_seconds_task_df"))
load(here("data","surface_by_zones"))
load(here("data","surface_by_AOI"))


# Merging information about the area

df_figure6 <- left_join(eye_tracker_seconds_task_df, surface_by_zones, by="zone") %>%
  select(subject, store, order, task,tot_seconds_task, zone, fixations, AOI) %>%
  filter(AOI != "other") ## Filtramos otras areas


# Gross Fixations: total fixations by AOI and task

fixations_by_subject_AOI_task <- df_figure6 %>%
  group_by(subject,task,store, AOI) %>%
  summarize(count=n(), total_fixations_subject_AOI_task=sum(fixations))

# **Figure 6.** BoxPlot Fixations

p1 <- fixations_by_subject_AOI_task %>%
  filter(AOI=="A") %>%
  ggplot(., aes(x=task, y=total_fixations_subject_AOI_task))  +
  geom_boxplot() + 
  scale_y_log10(limits = c(1, 1200))+
  coord_fixed(ratio = 1)+
  labs(title="Zone A (Header)",x="Task", y = "Fixations")+
  theme_light(base_size = 8)+
  theme(plot.title = element_text(hjust = 0.5))


p2 <- fixations_by_subject_AOI_task %>%
  filter(AOI=="B") %>%
  ggplot(., aes(x=task, y=total_fixations_subject_AOI_task))  +
  geom_boxplot() + 
  scale_y_log10(limits = c(1, 1200))+
  coord_fixed(ratio = 1)+
  labs(title="Zone B (Menu)",x="Task", y = "Fixations")+
  theme_light(base_size = 8)+
  theme(plot.title = element_text(hjust = 0.5))

p3 <- fixations_by_subject_AOI_task %>%
  filter(AOI=="C") %>%
  ggplot(., aes(x=task, y=total_fixations_subject_AOI_task))+
  coord_fixed(ratio = 1)+
  geom_boxplot() + 
  scale_y_log10(limits = c(1, 1200))+
  labs(title="Zone C (Products)",x="Task", y = "Fixations")+
  theme_light(base_size = 8)+
  theme(plot.title = element_text(hjust = 0.5))


ggsave(here("figures", "Figure6.pdf"), plot = plot_grid(p1, p2,p3, ncol=3), 
       scale = 1, width = 10, height = 2.5, 
       dpi = 600)

ggsave(here("figures", "Figure6.png"), plot = plot_grid(p1, p2,p3, ncol=3), 
       scale = 1, width = 10, height = 2.5, 
       dpi = 600)



