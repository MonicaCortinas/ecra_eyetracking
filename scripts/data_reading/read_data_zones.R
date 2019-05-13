library(tidyverse)
library(here)

# Zones

zones<-read_excel(here("data/zonas","areaszonas.xls"), sheet =1, col_names =FALSE)

names(zones)<- c("zone", "zondesc", "areastore1", "areastore2", "areastore3", "areastore4")


# Creating variables for the identification of areas:

zones <- zones %>%
  mutate(AOI= case_when(
    (zone == 1 | zone ==2)  ~ "A",
    (zone >= 3 & zone <=6)  ~ "B",
    (zone >= 10 & zone <=90)  ~ "C",
    (zone == 99 | zone == -99 | zone == 0) ~ "other" ))

# Agregation of areas for calculating total surface

by_AOI <- zones %>%
  group_by(AOI) %>%
  summarize(count=n(), area1=sum(areastore1), area2=sum(areastore2),
            area3=sum(areastore3), area4=sum(areastore4))

# Total aggregation by store for calcultating total store surface

total_store <- by_AOI %>%
  summarize(count=n(), areaT1=sum(area1), areaT2=sum(area2),
            areaT3=sum(area3), areaT4=sum(area4))

# Including data abbout total area in order to calculate the perceptages

surface_by_AOI <- by_AOI %>%
  mutate(total_surface_store1 = total_store$areaT1[1]) %>%
  mutate(total_surface_store2 = total_store$areaT2[1]) %>%
  mutate(total_surface_store3 = total_store$areaT3[1]) %>%
  mutate(total_surface_store4 = total_store$areaT4[1]) 
  

surface_by_zones <- zones %>%
  mutate(total_surface_store1 = total_store$areaT1[1]) %>%
  mutate(total_surface_store2 = total_store$areaT2[1]) %>%
  mutate(total_surface_store3 = total_store$areaT3[1]) %>%
  mutate(total_surface_store4 = total_store$areaT4[1])  

surface_by_AOI <- surface_by_AOI %>%
  mutate(prop_AOI_Store1 = 100*area1/total_surface_store1) %>%
  mutate(prop_AOI_Store2 = 100*area2/total_surface_store2) %>%
  mutate(prop_AOI_Store3 = 100*area3/total_surface_store3) %>%
  mutate(prop_AOI_Store4 = 100*area4/total_surface_store4)

rm(total_store, by_AOI, zones) ## we don't neeed it any more

save(surface_by_zones,file=here("data", "surface_by_zones")) #save total zones data frame
save(surface_by_AOI,file=here("data", "surface_by_AOI")) # save dataframe AOI