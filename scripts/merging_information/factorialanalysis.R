library(tidyverse)
library(here)
library(kableExtra)
library(lme4)
library(nFactors)

load(here("data", "qualtrics_data"))



# Factoriales

impli_zapa <- qualtrics_data %>% dplyr::select(14:16)
d<-factanal(impli_zapa, factors=1)
a1<-psych::alpha(impli_zapa)

qualtrics_data <- qualtrics_data %>%
  mutate(impli_zapa = d$loadings[1,]*Q9ConoZapa+ d$loadings[2,]*Q10ImpZapa+d$loadings[3,]*Q11IntZapa)

impli_mobile <- qualtrics_data %>%
  dplyr::select(33:35)
d<-factanal(impli_mobile, factors=1)
a1<-psych::alpha(impli_mobile)

qualtrics_data <- qualtrics_data %>%
  mutate(impli_mobile = d$loadings[1,]*Q25ConoMovil+ d$loadings[2,]*Q26ImpMovil+d$loadings[3,]*Q27IntMovil)

impli_pens <- qualtrics_data %>%
  dplyr::select(52:54)
d<-factanal(impli_pens, factors=1)
a1<-psych::alpha(impli_pens)

qualtrics_data <- qualtrics_data %>%
  mutate(impli_pens = d$loadings[1,]*Q40ConoBolis+ d$loadings[2,]*Q41ImpBolis+d$loadings[3,]*Q42IntBolis) 

impli_hds <- qualtrics_data %>%
  dplyr::select(71:73)
d<-factanal(impli_hds, factors=1)
a1<-psych::alpha(impli_hds)

qualtrics_data <- qualtrics_data %>%
  mutate(impli_hds = d$loadings[1,]*Q56ConoHDS+ d$loadings[2,]*Q57ImpHDS+d$loadings[3,]*Q58IntHDS) 

rm(a1, d, impli_hds, impli_mobile, impli_pens, impli_zapa)


#### Selección de variables

df_qualtrics_impli<- qualtrics_data %>%
  rename(id="Response ID") %>%
  dplyr::select(id, TareaZapatillas, TareaMovil, TareaBolis,TareaHDS, 
                secZapa, secTele,secBoli, secHd,
                impli_zapa, impli_mobile, impli_pens, impli_hds, 
                Q65ComodobuscaInet, Q66ComodocompraInet)

save(df_qualtrics_impli,file=here("data", "df_qualtrics_impli"))
