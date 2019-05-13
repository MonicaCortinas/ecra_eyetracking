library(tidyverse)
library(here)

################################################################################

qualtrics<-read_csv(here("data/qualtrics","qualtrics.csv"), skip = 1)
################################################################################

###########################################################################
###################### Codificar las Tareas###############################
#######################Guardar Los TIEMPOS ##############################
#########################################################################

qualtrics2<- qualtrics %>%
  mutate(TareaZapatillas = case_when(Tarea1==1 ~ "a",
                                     Tarea1==2 ~ "b",
                                     Tarea1==3 ~ "c",
                                     Tarea1==4 ~ "d"))%>%
  mutate(TareaMovil = case_when((Tarea1==1)&(Tarea2==1) ~ "b",
                                (Tarea1==1)&(Tarea2==2) ~ "c",
                                (Tarea1==1)&(Tarea2==3) ~ "d",
                                (Tarea1==2)&(Tarea2==1) ~ "a",
                                (Tarea1==2)&(Tarea2==2) ~ "c",
                                (Tarea1==2)&(Tarea2==3) ~ "d",
                                (Tarea1==3)&(Tarea2==1) ~ "a",
                                (Tarea1==3)&(Tarea2==2) ~ "b",
                                (Tarea1==3)&(Tarea2==3) ~ "d",
                                (Tarea1==4)&(Tarea2==1) ~ "a",
                                (Tarea1==4)&(Tarea2==2) ~ "b",
                                (Tarea1==4)&(Tarea2==3) ~ "c") )%>%
  mutate(TareaBolis = case_when((Tarea1==1)&(Tarea2==1)&(Tarea3==1) ~ "c",
                                (Tarea1==1)&(Tarea2==1)&(Tarea3==2) ~ "d",
                                (Tarea1==1)&(Tarea2==2)&(Tarea3==1) ~ "b",
                                (Tarea1==1)&(Tarea2==2)&(Tarea3==2) ~ "d",
                                (Tarea1==1)&(Tarea2==3)&(Tarea3==1) ~ "b",
                                (Tarea1==1)&(Tarea2==3)&(Tarea3==2) ~ "c",
                                (Tarea1==2)&(Tarea2==1)&(Tarea3==1) ~ "c",
                                (Tarea1==2)&(Tarea2==1)&(Tarea3==2) ~ "d",
                                (Tarea1==2)&(Tarea2==2)&(Tarea3==1) ~ "a",
                                (Tarea1==2)&(Tarea2==2)&(Tarea3==2) ~ "d",
                                (Tarea1==2)&(Tarea2==3)&(Tarea3==1) ~ "a",
                                (Tarea1==2)&(Tarea2==3)&(Tarea3==2) ~ "c",
                                (Tarea1==3)&(Tarea2==1)&(Tarea3==1) ~ "b",
                                (Tarea1==3)&(Tarea2==1)&(Tarea3==2) ~ "d",
                                (Tarea1==3)&(Tarea2==2)&(Tarea3==1) ~ "a",
                                (Tarea1==3)&(Tarea2==2)&(Tarea3==2) ~ "d",
                                (Tarea1==3)&(Tarea2==3)&(Tarea3==1) ~ "a",
                                (Tarea1==3)&(Tarea2==3)&(Tarea3==2) ~ "b",
                                (Tarea1==4)&(Tarea2==1)&(Tarea3==1) ~ "b",
                                (Tarea1==4)&(Tarea2==1)&(Tarea3==2) ~ "c",
                                (Tarea1==4)&(Tarea2==2)&(Tarea3==1) ~ "a",
                                (Tarea1==4)&(Tarea2==2)&(Tarea3==2) ~ "c",
                                (Tarea1==4)&(Tarea2==3)&(Tarea3==1) ~ "a",
                                (Tarea1==4)&(Tarea2==3)&(Tarea3==2) ~ "b") )%>%
  mutate(TareaHDS = case_when((Tarea1==1)&(Tarea2==1)&(Tarea3==1) ~ "d",
                              (Tarea1==1)&(Tarea2==1)&(Tarea3==2) ~ "c",
                              (Tarea1==1)&(Tarea2==2)&(Tarea3==1) ~ "d",
                              (Tarea1==1)&(Tarea2==2)&(Tarea3==2) ~ "b",
                              (Tarea1==1)&(Tarea2==3)&(Tarea3==1) ~ "c",
                              (Tarea1==1)&(Tarea2==3)&(Tarea3==2) ~ "b",
                              (Tarea1==2)&(Tarea2==1)&(Tarea3==1) ~ "d",
                              (Tarea1==2)&(Tarea2==1)&(Tarea3==2) ~ "c",
                              (Tarea1==2)&(Tarea2==2)&(Tarea3==1) ~ "d",
                              (Tarea1==2)&(Tarea2==2)&(Tarea3==2) ~ "a",
                              (Tarea1==2)&(Tarea2==3)&(Tarea3==1) ~ "c",
                              (Tarea1==2)&(Tarea2==3)&(Tarea3==2) ~ "a",
                              (Tarea1==3)&(Tarea2==1)&(Tarea3==1) ~ "d",
                              (Tarea1==3)&(Tarea2==1)&(Tarea3==2) ~ "b",
                              (Tarea1==3)&(Tarea2==2)&(Tarea3==1) ~ "d",
                              (Tarea1==3)&(Tarea2==2)&(Tarea3==2) ~ "a",
                              (Tarea1==3)&(Tarea2==3)&(Tarea3==1) ~ "b",
                              (Tarea1==3)&(Tarea2==3)&(Tarea3==2) ~ "a",
                              (Tarea1==4)&(Tarea2==1)&(Tarea3==1) ~ "c",
                              (Tarea1==4)&(Tarea2==1)&(Tarea3==2) ~ "b",
                              (Tarea1==4)&(Tarea2==2)&(Tarea3==1) ~ "c",
                              (Tarea1==4)&(Tarea2==2)&(Tarea3==2) ~ "a",
                              (Tarea1==4)&(Tarea2==3)&(Tarea3==1) ~ "b",
                              (Tarea1==4)&(Tarea2==3)&(Tarea3==2) ~ "a"))%>%
  mutate(secZapa = as.numeric(zapa_shop_end)/1000-as.numeric(zapa_shop_start)/1000,
         secTele = as.numeric(tele_shop_end)/1000-as.numeric(tele_shop_start)/1000,
         secBoli = as.numeric(boli_shop_end)/1000-as.numeric(boli_shop_start)/1000, 
         secHd   = as.numeric(hd_shop_end)/1000-as.numeric(hd_shop_start)/1000)



###################################################################################
#################################  CLEANING AND RECODING DATA #######################
###################################################################################


#####################################################################################
################### Tiendas. Zapatillas #############################################
#####################################################################################

  
qualtrics2<- qualtrics2 %>%  
  rename(Q1_CompraZapatillas="Q30 - ¿Has comprado alguna vez unas zapatillas deportivas?" )%>%  
  rename(Q2_ZapatillasFrecuencia="Q90 - ¿Con qué frecuencia compras zapatillas deportivas?")%>%
  mutate(Q2_ZapatillasFrecuencia = factor(Q2_ZapatillasFrecuencia, levels = c("Cada 6 meses", "Entre 6 meses y 1 año", "Entre 1 y 2 años", "Entre 2 y 3 años", "Con menor frecuencia")))%>%
  rename(Q3_ZapatillasOnline="Q7 - ¿Has comprado alguna vez unas zapatillas online?")%>%
  rename(Q4_ZapOnlineFREQ="Q72 - ¿Con qué frecuencia acudes a Internet para comprar zapatillas deportivas?")%>%
  rename(Q5_AtractZapatillas="Q80 - ¿En qué medida te parece atractiva esta página web?")


qualtrics2<- qualtrics2 %>%  
  rename("Q61_ZapaModel1"="Q49_1 - Por favor, selecciona el modelo elegido o los modelos elegidos - Adidas Gazelle",
         "Q61_ZapaModel2"="Q49_4 - Por favor, selecciona el modelo elegido o los modelos elegidos - Adidas Stan Smith Leather",
         "Q61_ZapaModel3"="Q49_5 - Por favor, selecciona el modelo elegido o los modelos elegidos - Asics Tiger Leather" ,
         "Q61_ZapaModel4"="Q49_6 - Por favor, selecciona el modelo elegido o los modelos elegidos - Converse All Star",
         "Q61_ZapaModel5"="Q49_7 - Por favor, selecciona el modelo elegido o los modelos elegidos - Le Coq Sportif Quartz",
         "Q61_ZapaModel6"="Q49_8 - Por favor, selecciona el modelo elegido o los modelos elegidos - New Balance 373" ,
         "Q61_ZapaModel7"="Q49_9 - Por favor, selecciona el modelo elegido o los modelos elegidos - Reebok Classic Leather"  ,
         "Q61_ZapaModel8"="Q49_10 - Por favor, selecciona el modelo elegido o los modelos elegidos - Nike Internationalist",
         "Q61_ZapaModel9"="Q49_11 - Por favor, selecciona el modelo elegido o los modelos elegidos - No lo recuerdo") %>%  
  mutate (Q6modelozapatilla = case_when(Q61_ZapaModel1== "Adidas Gazelle" ~ "Adidas Gazelle",
                                        Q61_ZapaModel2=="Adidas Stan Smith Leather" ~  "Adidas Stan Smith Leather",
                                        Q61_ZapaModel3=="Asics Tiger Leather" ~  "Asics Tiger Leather",
                                        Q61_ZapaModel4=="Converse All Star" ~  "Converse All Star",
                                        Q61_ZapaModel5=="Le Coq Sportif Quartz" ~  "Le Coq Sportif Quartz",
                                        Q61_ZapaModel6=="New Balance 373" ~  "New Balance 373",
                                        Q61_ZapaModel7=="Reebok Classic Leather" ~  "Reebok Classic Leather",
                                        Q61_ZapaModel8=="Nike Internationalist" ~  "Nike Internationalist",
                                        Q61_ZapaModel9=="No lo recuerdo" ~  "No lo recuerdo"))

qualtrics2<- qualtrics2 %>%  
  rename("Q7_DifCompraZapa"="Q54 - ¿En qué medida te ha resultado difícil completar la tarea?")%>%  
  rename("Q8_DifPostZapa"="Q63 - ¿En qué medida te ha resultado difícil completar la tarea?_1")


qualtrics2<- qualtrics2 %>%  
  rename(Q9ConoZapaT1="Q111_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre zapatillas deportivas es muy alto",
         Q9ConoZapaT2= "Q94_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre zapatillas deportivas es muy alto",
         Q9ConoZapaT3="Q98_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre zapatillas deportivas es muy alto",
         Q9ConoZapaT4="Q102_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre zapatillas deportivas es muy alto")%>% 
  mutate(Q9ConoZapa = ifelse (is.na(Q9ConoZapaT1)==FALSE, Q9ConoZapaT1, ifelse(is.na(Q9ConoZapaT2)==FALSE, Q9ConoZapaT2,ifelse(is.na(Q9ConoZapaT3)==FALSE, Q9ConoZapaT3, Q9ConoZapaT4))))%>% 
  mutate(Q9ConoZapa = factor(Q9ConoZapa, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))



qualtrics2<- qualtrics2 %>%  
  rename(Q10ImpZapaT1="Q111_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar unas zapatillas deportivas",
         Q10ImpZapaT2= "Q94_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar unas zapatillas deportivas",
         Q10ImpZapaT3="Q98_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar unas zapatillas deportivas",
         Q10ImpZapaT4="Q102_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar unas zapatillas deportivas") %>%  
  mutate(Q10ImpZapa = ifelse (is.na(Q10ImpZapaT1)==FALSE, Q10ImpZapaT1, ifelse(is.na(Q10ImpZapaT2)==FALSE, Q10ImpZapaT2,ifelse(is.na(Q10ImpZapaT3)==FALSE, Q10ImpZapaT3, Q10ImpZapaT4)))) %>% 
  mutate(Q10ImpZapa = factor(Q10ImpZapa, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))

qualtrics2<- qualtrics2 %>%  
  rename(Q11IntZapaT1="Q111_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar zapatillas deportivas",
         Q11IntZapaT2= "Q94_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar zapatillas deportivas",
         Q11IntZapaT3="Q98_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar zapatillas deportivas",
         Q11IntZapaT4="Q102_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar zapatillas deportivas") %>%  
  mutate( Q11IntZapa = ifelse (is.na(Q11IntZapaT1)==FALSE, Q11IntZapaT1, ifelse(is.na(Q11IntZapaT2)==FALSE, Q11IntZapaT2,ifelse(is.na(Q11IntZapaT3)==FALSE, Q11IntZapaT3, Q11IntZapaT4))))%>% 
  mutate(Q11IntZapa = factor(Q11IntZapa, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))


qualtrics2<- qualtrics2 %>%  
  rename(Q12PrecioZapaT1="Q81_1 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio",
         Q12PrecioZapaT2= "Q96_1 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio"  ,
         Q12PrecioZapaT3="Q100_1 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio" ,
         Q12PrecioZapaT4="Q104_1 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio") %>%
  mutate(Q12PrecioZapaT1 = ifelse (Q12PrecioZapaT1=="Muy Alto", "Muy alto", Q12PrecioZapaT1)) %>%  
  mutate( Q12PrecioZapa = ifelse (is.na(Q12PrecioZapaT1)==FALSE, Q12PrecioZapaT1, ifelse(is.na(Q12PrecioZapaT2)==FALSE, Q12PrecioZapaT2,ifelse(is.na(Q12PrecioZapaT3)==FALSE, Q12PrecioZapaT3, Q12PrecioZapaT4))))%>% 
  mutate(Q12PrecioZapa = factor(Q12PrecioZapa, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q13MarcaZapaT1"="Q81_2 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Marca"   ,
         "Q13MarcaZapaT2"= "Q96_2 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Marca"  ,
         "Q13MarcaZapaT3"="Q100_2 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Marca" ,
         "Q13MarcaZapaT4"="Q104_2 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Marca") %>%
  mutate(Q13MarcaZapaT1 = ifelse (Q13MarcaZapaT1=="Muy Alto", "Muy alto", Q13MarcaZapaT1))  %>%  
  mutate(Q13MarcaZapa = ifelse (is.na(Q13MarcaZapaT1)==FALSE, Q13MarcaZapaT1, ifelse(is.na(Q13MarcaZapaT2)==FALSE, Q13MarcaZapaT2,
                                                                                     ifelse(is.na(Q13MarcaZapaT3)==FALSE, Q13MarcaZapaT3, Q13MarcaZapaT4))))%>% 
  mutate(Q13MarcaZapa = factor(Q13MarcaZapa, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q14PagoZapaT1"="Q81_3 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Métodos de pago"   ,
         "Q14PagoZapaT2"= "Q96_3 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Métodos de pago"  ,
         "Q14PagoZapaT3"="Q100_3 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Métodos de pago" ,
         "Q14PagoZapaT4"="Q104_3 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Métodos de pago") %>%
  mutate(Q14PagoZapaT1 = ifelse (Q14PagoZapaT1=="Muy Alto", "Muy alto", Q14PagoZapaT1)) %>%  
  mutate(Q14PagoZapa = ifelse (is.na(Q14PagoZapaT1)==FALSE, Q14PagoZapaT1, ifelse(is.na(Q14PagoZapaT2)==FALSE, Q14PagoZapaT2,
                                                                                  ifelse(is.na(Q14PagoZapaT3)==FALSE, Q14PagoZapaT3, Q14PagoZapaT4))))%>% 
  mutate(Q14PagoZapa = factor(Q14PagoZapa, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q15DevolZapaT1"="Q81_4 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Devoluciones y Reembolsos"   ,
         "Q15DevolZapaT2"= "Q96_4 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Devoluciones y Reembolsos"  ,
         "Q15DevolZapaT3"="Q100_4 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Devoluciones y Reembolsos" ,
         "Q15DevolZapaT4"="Q104_4 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Devoluciones y Reembolsos") %>%
  mutate(Q15DevolZapaT1 = ifelse (Q15DevolZapaT1=="Muy Alto", "Muy alto", Q15DevolZapaT1))  %>%  
  mutate(Q15DevolZapa = ifelse (is.na(Q15DevolZapaT1)==FALSE, Q15DevolZapaT1, ifelse(is.na(Q15DevolZapaT2)==FALSE, Q15DevolZapaT2,
                                                                                     ifelse(is.na(Q15DevolZapaT3)==FALSE, Q15DevolZapaT3, Q15DevolZapaT4))))%>% 
  mutate(Q15DevolZapa = factor(Q15DevolZapa, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q16EnvioZapaT1"="Q81_5 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Formas de envío y plazos"    ,
         "Q16EnvioZapaT2"= "Q96_5 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Formas de envío y plazos"   ,
         "Q16EnvioZapaT3"="Q100_5 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Formas de envío y plazos"     ,
         "Q16EnvioZapaT4"="Q104_5 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Formas de envío y plazos"  ) %>%
  mutate(Q16EnvioZapaT1 = ifelse (Q16EnvioZapaT1=="Muy Alto", "Muy alto", Q16EnvioZapaT1))  %>%  
  mutate(Q16EnvioZapa = ifelse (is.na(Q16EnvioZapaT1)==FALSE, Q16EnvioZapaT1, ifelse(is.na(Q16EnvioZapaT2)==FALSE, Q16EnvioZapaT2,
                                                                                     ifelse(is.na(Q16EnvioZapaT3)==FALSE, Q16EnvioZapaT3, Q16EnvioZapaT4))))%>% 
  mutate(Q16EnvioZapa = factor(Q16EnvioZapa, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))

qualtrics2<- qualtrics2 %>%  
  rename("Q17SeguiZapaT1"="Q81_6 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Seguimiento de Pedidos"  ,
         "Q17SeguiZapaT2"= "Q96_6 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Seguimiento de Pedidos"   ,
         "Q17SeguiZapaT3"="Q100_6 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Seguimiento de Pedidos" ,
         "Q17SeguiZapaT4"="Q104_7 - Al adquirir unas zapatillas deportivas a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Seguimiento de Pedidos" ) %>%
  mutate(Q17SeguiZapaT1 = ifelse (Q17SeguiZapaT1=="Muy Alto", "Muy alto", Q17SeguiZapaT1))  %>%  
  mutate(Q17SeguiZapa = ifelse (is.na(Q17SeguiZapaT1)==FALSE, Q17SeguiZapaT1, ifelse(is.na(Q17SeguiZapaT2)==FALSE, Q17SeguiZapaT2,
                                                                                     ifelse(is.na(Q17SeguiZapaT2)==FALSE, Q17SeguiZapaT3, Q17SeguiZapaT4))))%>% 
  mutate(Q17SeguiZapa = factor(Q17SeguiZapa, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))



###################################################################################################
################################################################################################
############################# Telefonos Moviles ################################################
##############################################################################################

qualtrics2<- qualtrics2 %>%  
  rename("Q18_CompraMovil"= "Q5 - ¿Has comprado alguna vez un teléfono móvil?"  )

qualtrics2<- qualtrics2 %>%  
  rename(Q19_MovilFrecuencia="Q90 - ¿Con qué frecuencia compras un teléfono móvil?")


qualtrics2<- qualtrics2 %>%  
  rename(Q20_MovilOnline="Q31 - ¿Has comprado alguna vez un teléfono móvil online?")

qualtrics2<- qualtrics2 %>%  
  rename(Q21_MovilOnlineFREQ="Q69 - ¿Con qué frecuencia acudes a Internet para comprar un teléfono móvil?")

qualtrics2<- qualtrics2 %>%  
  rename(Q22_AtractMovil="Q59 - Vuelve a esta pantalla cuando hayas terminado.\n\n \n\n¿En qué medida te parece atrativo este establecimiento web?")

qualtrics2 <- qualtrics2 %>%
  rename (Q23_ModeloMovil1 = 36,
          Q23_ModeloMovil2 = 37,
          Q23_ModeloMovil3 = 38,
          Q23_ModeloMovil4 = 39,
          Q23_ModeloMovil5 = 40,
          Q23_ModeloMovil6 = 41,
          Q23_ModeloMovil7 = 42,
          Q23_ModeloMovil8 = 43,
          Q23_ModeloMovil9 = 44)


qualtrics2<- qualtrics2 %>%  
  mutate (Q23_ModeloMovil = case_when(is.na(Q23_ModeloMovil1)== FALSE ~ "Sony Xperia Z3",
                                      is.na(Q23_ModeloMovil2)== FALSE ~  "Samsung Galaxy J3",
                                      is.na(Q23_ModeloMovil3)== FALSE ~  "Moto G 5",
                                      is.na(Q23_ModeloMovil4)== FALSE ~  "LG K10",
                                      is.na(Q23_ModeloMovil5)== FALSE ~  "Huawei P9 Lite",
                                      is.na(Q23_ModeloMovil6)== FALSE ~  "Honor 6X",
                                      is.na(Q23_ModeloMovil7)== FALSE ~  "BQ Aquaris U2",
                                      is.na(Q23_ModeloMovil8)== FALSE ~  "ASUS ZenFone 2",
                                      is.na(Q23_ModeloMovil9)== FALSE ~  "No lo recuerdo"))

qualtrics2<- qualtrics2 %>%  
  rename(Q23_DifCompraMovil="Q53 - ¿En qué medida te ha resultado difícil completar la tarea?")

qualtrics2<- qualtrics2 %>%  
  rename("Q24_DifPostMovil"="Q74 - ¿En qué medida te ha resultado difícil completar la tarea?")



qualtrics2<- qualtrics2 %>%  
  rename("Q25ConoMovilT1"="Q102_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre teléfonos móviles es muy alto",
         "Q25ConoMovilT2"= "Q106_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre teléfonos móviles es muy alto",
         "Q25ConoMovilT3"="Q110_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre teléfonos móviles es muy alto",
         "Q25ConoMovilT4"="Q106_8 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Mi conocimiento sobre teléfonos móviles es muy alto_1")

qualtrics2<- qualtrics2 %>%  
  mutate(Q25ConoMovil = ifelse (is.na(Q25ConoMovilT1)==FALSE, Q25ConoMovilT1, ifelse(is.na(Q25ConoMovilT2)==FALSE, 
                                                                                     Q25ConoMovilT2,ifelse(is.na(Q25ConoMovilT3)==FALSE, Q25ConoMovilT3, Q25ConoMovilT4))))%>% 
  mutate(Q25ConoMovil = factor(Q25ConoMovil, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))



qualtrics2<- qualtrics2 %>%  
  rename("Q26ImpMovilT1"="Q102_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar un teléfono móvil",
         "Q26ImpMovilT2"= "Q106_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar un teléfono móvil",
         "Q26ImpMovilT3"="Q110_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar un teléfono móvil",
         "Q26ImpMovilT4"="Q106_9 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me importa mucho tomar buenas decisiones al comprar un teléfono móvil_1")

qualtrics2<- qualtrics2 %>%  
  mutate(Q26ImpMovil = ifelse (is.na(Q26ImpMovilT1)==FALSE, Q26ImpMovilT1, ifelse(is.na(Q26ImpMovilT2)==FALSE, Q26ImpMovilT2,ifelse(is.na(Q26ImpMovilT3)==FALSE, 
                                                                                                                                    Q26ImpMovilT3, Q26ImpMovilT4))))%>% 
  mutate(Q26ImpMovil = factor(Q26ImpMovil, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))


qualtrics2<- qualtrics2 %>%  
  rename("Q27IntMovilT1"="Q102_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar teléfonos móviles",
         "Q27IntMovilT2"= "Q106_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar teléfonos móviles",
         "Q27IntMovilT3"="Q110_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar teléfonos móviles",
         "Q27IntMovilT4"="Q106_10 - Indica tu nivel de acuerdo o desacuerdo con las siguientes cuestiones: - Me parece interesante comprar teléfonos móviles_1")

qualtrics2<- qualtrics2 %>%  
  mutate(Q27IntMovil = ifelse (is.na(Q27IntMovilT1)==FALSE, Q27IntMovilT1, ifelse(is.na(Q27IntMovilT2)==FALSE, 
                                                                                  Q27IntMovilT2,ifelse(is.na(Q27IntMovilT3)==FALSE, Q27IntMovilT3, Q27IntMovilT4))))%>% 
  mutate(Q27IntMovil = factor(Q27IntMovil, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))




qualtrics2<- qualtrics2 %>%  
  rename("Q28PrecioMovilT1"="Q104_1 - Al adquirir un teléfono móvil a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio"   ,
         "Q28PrecioMovilT2"= "Q108_1 - Al adquirir un teléfono móvil a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio" ,
         "Q28PrecioMovilT3"="Q112_1 - Al adquirir un teléfono móvil a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio",
         "Q28PrecioMovilT4"="Q108_1 - Al adquirir un teléfono móvil a través de Internet, ¿qué importancia darías a los siguientes aspectos? - Precio_1")  %>%  
  mutate(Q28PrecioMovil = ifelse (is.na(Q28PrecioMovilT1)==FALSE, Q28PrecioMovilT1, ifelse(is.na(Q28PrecioMovilT2)==FALSE, Q28PrecioMovilT2,
                                                                                           ifelse(is.na(Q28PrecioMovilT3)==FALSE, Q28PrecioMovilT3, Q28PrecioMovilT4))))%>% 
  mutate(Q28PrecioMovil = factor(Q28PrecioMovil, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q29MarcaMovilT1"=49,
         "Q29MarcaMovilT2"= 107,
         "Q29MarcaMovilT3"=153,
         "Q29MarcaMovilT4"=181)  %>%  
  mutate(Q29MarcaMovil = ifelse (is.na(Q29MarcaMovilT1)==FALSE, Q29MarcaMovilT1, ifelse(is.na(Q29MarcaMovilT2)==FALSE, Q29MarcaMovilT2,
                                                                                        ifelse(is.na(Q29MarcaMovilT3)==FALSE, Q29MarcaMovilT3, Q29MarcaMovilT4))))%>% 
  mutate(Q29MarcaMovil = factor(Q29MarcaMovil, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q30PagoMovilT1"=50,
         "Q30PagoMovilT2"= 108,
         "Q30PagoMovilT3"=154,
         "Q30PagoMovilT4"=182) %>%  
  mutate(Q30PagoMovil = ifelse (is.na(Q30PagoMovilT1)==FALSE, Q30PagoMovilT1, ifelse(is.na(Q30PagoMovilT2)==FALSE, Q30PagoMovilT2,
                                                                                     ifelse(is.na(Q30PagoMovilT3)==FALSE, Q30PagoMovilT3, Q30PagoMovilT4))))%>% 
  mutate(Q30PagoMovil = factor(Q30PagoMovil, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q31DevolucionesMovilT1"=51,
         "Q31DevolucionesMovilT2"= 109,
         "Q31DevolucionesMovilT3"=155,
         "Q31DevolucionesMovilT4"=183)  %>%  
  mutate(Q31DevolucionesMovil = ifelse (is.na(Q31DevolucionesMovilT1)==FALSE, Q31DevolucionesMovilT1, ifelse(is.na(Q31DevolucionesMovilT2)==FALSE, Q31DevolucionesMovilT2,
                                                                                                             ifelse(is.na(Q31DevolucionesMovilT3)==FALSE, Q31DevolucionesMovilT3, Q31DevolucionesMovilT4))))%>% 
  mutate(Q31DevolucionesMovil = factor(Q31DevolucionesMovil, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q32EnvioMovilT1"=52,
         "Q32EnvioMovilT2"= 110,
         "Q32EnvioMovilT3"=156,
         "Q32EnvioMovilT4"=184) %>%  
  mutate(Q32EnvioMovil = ifelse (is.na(Q32EnvioMovilT1)==FALSE, Q32EnvioMovilT1, ifelse(is.na(Q32EnvioMovilT2)==FALSE, Q32EnvioMovilT2,
                                                                                        ifelse(is.na(Q32EnvioMovilT3)==FALSE, Q32EnvioMovilT3, Q32EnvioMovilT4))))%>% 
  mutate(Q32EnvioMovil = factor(Q32EnvioMovil, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q33SeguimientoMovilT1"=51,
         "Q33SeguimientoMovilT2"= 109,
         "Q33SeguimientoMovilT3"=155,
         "Q33SeguimientoMovilT4"=183)  %>%  
  mutate(Q33SeguimientoMovil = ifelse (is.na(Q33SeguimientoMovilT1)==FALSE, Q33SeguimientoMovilT1, ifelse(is.na(Q33SeguimientoMovilT2)==FALSE, Q33SeguimientoMovilT2,
                                                                                                          ifelse(is.na(Q33SeguimientoMovilT3)==FALSE, Q33SeguimientoMovilT3, Q33SeguimientoMovilT4))))%>% 
  mutate(Q33SeguimientoMovil = factor(Q33SeguimientoMovil, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


###################################################################################################
################################################################################################
############################# Bolígrafos ################################################
##############################################################################################

# Tiendas. Bolígrafos

## Bolígrafos Hábitos


qualtrics2<- qualtrics2 %>%  
  rename("Q34_bolis"= 54)


qualtrics2<- qualtrics2 %>%  
  rename(Q35_bolisFrecuencia=55)
                                                                                                              

qualtrics2<- qualtrics2 %>%  
  rename(Q36_bolisOnline=56)



qualtrics2<- qualtrics2 %>%  
  rename(Q37_bolisOnlineFREQ=57)


qualtrics2<- qualtrics2 %>%  
  rename(Q38_AtractBolis=186)


qualtrics2 <- qualtrics2 %>%
  rename (Q38_ModeloBoli1 = 112,
          Q38_ModeloBoli2 = 113,
          Q38_ModeloBoli3 = 114,
          Q38_ModeloBoli4 = 115,
          Q38_ModeloBoli5 = 116,
          Q38_ModeloBoli6 = 117,
          Q38_ModeloBoli7 = 118,
          Q38_ModeloBoli8 = 119,
          Q38_ModeloBoli9 = 120)

qualtrics2<- qualtrics2 %>%  
  mutate (Q38_ModeloBoli = case_when(is.na(Q38_ModeloBoli1)== FALSE ~ "Uniball",
                                     is.na(Q38_ModeloBoli2)== FALSE ~ "Staedler",
                                     is.na(Q38_ModeloBoli3)== FALSE ~ "Stabilo",
                                     is.na(Q38_ModeloBoli4)== FALSE ~ "Pilot",
                                      is.na(Q38_ModeloBoli5)== FALSE ~ "MILAN",
                                      is.na(Q38_ModeloBoli6)== FALSE ~"Edding",
                                      is.na(Q38_ModeloBoli7)== FALSE ~"Bic Naranja",
                                      is.na(Q38_ModeloBoli8)== FALSE ~"Bic Cristal",
                                      is.na(Q38_ModeloBoli9)== FALSE ~  "No lo recuerdo"))

qualtrics2<- qualtrics2 %>%  
  rename(Q39_DifCompraBoli=58)


qualtrics2<- qualtrics2 %>%  
  rename("Q40_DifPostBoli"=82)



qualtrics2<- qualtrics2 %>%  
  rename("Q40ConoBolisT1"=59,
         "Q40ConoBolisT2"= 83,
         "Q40ConoBolisT3"=121,
         "Q40ConoBolisT4"=187)

qualtrics2<- qualtrics2 %>%  
  mutate( Q40ConoBolis = ifelse (is.na(Q40ConoBolisT1)==FALSE, Q40ConoBolisT1, ifelse(is.na(Q40ConoBolisT2)==FALSE, Q40ConoBolisT2,ifelse(is.na(Q40ConoBolisT3)==FALSE,
                                                                                                                                          Q40ConoBolisT3, Q40ConoBolisT4))))%>% 
 mutate(Q40ConoBolis = factor(Q40ConoBolis, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))


qualtrics2<- qualtrics2 %>%  
  rename("Q41ImpBolisT1"=60,
         "Q41ImpBolisT2"= 84,
         "Q41ImpBolisT3"=122,
         "Q41ImpBolisT4"=188)

qualtrics2<- qualtrics2 %>%  
  mutate( Q41ImpBolis = ifelse (is.na(Q41ImpBolisT1)==FALSE, Q41ImpBolisT1, ifelse(is.na(Q41ImpBolisT2)==FALSE, Q41ImpBolisT2,ifelse(is.na(Q41ImpBolisT3)==FALSE, 
                                                                                                                                     Q41ImpBolisT3, Q41ImpBolisT4))))%>% 
 mutate(Q41ImpBolis = factor(Q41ImpBolis, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))



qualtrics2<- qualtrics2 %>%  
  rename("Q42IntBolisT1"=61,
         "Q42IntBolisT2"= 85,
         "Q42IntBolisT3"=123,
         "Q42IntBolisT4"=189)

qualtrics2<- qualtrics2 %>%  
  mutate( Q42IntBolis = ifelse (is.na(Q42IntBolisT1)==FALSE, Q42IntBolisT1, ifelse(is.na(Q42IntBolisT2)==FALSE, Q42IntBolisT2,ifelse(is.na(Q42IntBolisT3)==FALSE, 
                                                                                                                                     Q42IntBolisT3, Q42IntBolisT4))))%>% 
 mutate(Q42IntBolis = factor(Q42IntBolis, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))



qualtrics2<- qualtrics2 %>%  
  rename("Q43PrecioBolisT1"=62,
         "Q43PrecioBolisT2"= 86,
         "Q43PrecioBolisT3"=124,
         "Q43PrecioBolisT4"=190) 


qualtrics2<- qualtrics2 %>%  
  mutate(Q43PrecioBolis = ifelse (is.na(Q43PrecioBolisT1)==FALSE, Q43PrecioBolisT1, ifelse(is.na(Q43PrecioBolisT2)==FALSE, Q43PrecioBolisT2,
                                            ifelse(is.na(Q43PrecioBolisT3)==FALSE, Q43PrecioBolisT3, Q43PrecioBolisT4))))%>% 
 mutate(Q43PrecioBolis = factor(Q43PrecioBolis, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q44MarcaBolisT1"=63,
         "Q44MarcaBolisT2"= 87,
         "Q44MarcaBolisT3"=125,
         "Q44MarcaBolisT4"=191) 


qualtrics2<- qualtrics2 %>%  
  mutate(Q44MarcaBolis = ifelse (is.na(Q44MarcaBolisT1)==FALSE, Q44MarcaBolisT1, ifelse(is.na(Q44MarcaBolisT2)==FALSE, Q44MarcaBolisT2,
                                            ifelse(is.na(Q44MarcaBolisT3)==FALSE, Q44MarcaBolisT3, Q44MarcaBolisT4))))%>% 
 mutate(Q44MarcaBolis = factor(Q44MarcaBolis, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q45PagoBolisT1"=64,
         "Q45PagoBolisT2"= 88,
         "Q45PagoBolisT3"=126,
         "Q45PagoBolisT4"=192) 


qualtrics2<- qualtrics2 %>%  
  mutate(Q45PagoBolis = ifelse (is.na(Q45PagoBolisT1)==FALSE, Q45PagoBolisT1, ifelse(is.na(Q45PagoBolisT2)==FALSE, Q45PagoBolisT2,
                                            ifelse(is.na(Q45PagoBolisT3)==FALSE, Q45PagoBolisT3, Q45PagoBolisT4))))%>% 
 mutate(Q45PagoBolis = factor(Q45PagoBolis, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q46DevoluBolisT1"=65,
         "Q46DevoluBolisT2"= 89,
         "Q46DevoluBolisT3"=127,
         "Q46DevoluBolisT4"=193) 


qualtrics2<- qualtrics2 %>%  
  mutate(Q46DevoluBolis = ifelse (is.na(Q46DevoluBolisT1)==FALSE, Q46DevoluBolisT1, ifelse(is.na(Q46DevoluBolisT2)==FALSE, Q46DevoluBolisT2,
                                            ifelse(is.na(Q46DevoluBolisT3)==FALSE, Q46DevoluBolisT3, Q46DevoluBolisT4))))%>% 
 mutate(Q46DevoluBolis = factor(Q46DevoluBolis, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q47EnvioBolisT1"=66,
         "Q47EnvioBolisT2"= 90,
         "Q47EnvioBolisT3"=128,
         "Q47EnvioBolisT4"=194) 


qualtrics2<- qualtrics2 %>%  
  mutate(Q47EnvioBolis = ifelse (is.na(Q47EnvioBolisT1)==FALSE, Q47EnvioBolisT1, ifelse(is.na(Q47EnvioBolisT2)==FALSE, Q47EnvioBolisT2,
                                            ifelse(is.na(Q47EnvioBolisT3)==FALSE, Q47EnvioBolisT3, Q47EnvioBolisT4))))%>% 
 mutate(Q47EnvioBolis = factor(Q47EnvioBolis, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))



qualtrics2<- qualtrics2 %>%  
  rename("Q48SeguimientoBolisT1"=67,
         "Q48SeguimientoBolisT2"= 91,
         "Q48SeguimientoBolisT3"=129,
         "Q48SeguimientoBolisT4"=195) 


qualtrics2<- qualtrics2 %>%  
  mutate(Q48SeguimientoBolis = ifelse (is.na(Q48SeguimientoBolisT1)==FALSE, Q48SeguimientoBolisT1, ifelse(is.na(Q48SeguimientoBolisT2)==FALSE, Q48SeguimientoBolisT2,
                                            ifelse(is.na(Q48SeguimientoBolisT3)==FALSE, Q48SeguimientoBolisT3, Q48SeguimientoBolisT4))))%>% 
 mutate(Q48SeguimientoBolis = factor(Q48SeguimientoBolis, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))



###################################################################################################
########################################Discos Duros ################################################
##############################################################################################



# Tiendas. Discos Duros

## Tiendas. Hábitos


qualtrics2<- qualtrics2 %>%  
rename("Q49_CompraHDS"= 68)

qualtrics2<- qualtrics2 %>%  
rename(Q50_HDSFrecuencia=69)


qualtrics2<- qualtrics2 %>%  
rename(Q51_HDSOnline=70)


qualtrics2<- qualtrics2 %>%  
rename(Q52_HDSFREQ=71)


qualtrics2<- qualtrics2 %>%  
rename(Q53_AtractHDS=196)

qualtrics2 <- qualtrics2 %>%
  rename (Q53_ModeloHDS1 = 130,
          Q53_ModeloHDS2 = 131,
          Q53_ModeloHDS3 = 132,
          Q53_ModeloHDS4 = 133,
          Q53_ModeloHDS5 = 134,
          Q53_ModeloHDS6 = 135,
          Q53_ModeloHDS7 = 136,
          Q53_ModeloHDS8 = 137,
          Q53_ModeloHDS9 = 138) %>%  
  mutate (Q53_ModeloHDS = case_when(is.na(Q53_ModeloHDS1)== FALSE ~ "Toshiba Canvio",
                                    is.na(Q53_ModeloHDS2)== FALSE ~ "WD Elements 1T",
                                    is.na(Q53_ModeloHDS3)== FALSE ~ "WD Elements 2T",
                                    is.na(Q53_ModeloHDS4)== FALSE ~ "Toshiba Externo",
                                    is.na(Q53_ModeloHDS5)== FALSE ~ " Maxtor",
                                    is.na(Q53_ModeloHDS6)== FALSE ~ "Samsung SSD",
                                    is.na(Q53_ModeloHDS7)== FALSE ~ "Lacie Porsche",
                                    is.na(Q53_ModeloHDS8)== FALSE ~"Intenso",
                                    is.na(Q53_ModeloHDS9)== FALSE ~  "No lo recuerdo"))

qualtrics2<- qualtrics2 %>%  
  rename(Q54_DifCompraHDS=92)

qualtrics2<- qualtrics2 %>%  
  rename("Q55_DifPostHDS"=72)

qualtrics2<- qualtrics2 %>%  
  rename("Q56ConoHDST1"=73,
         "Q56ConoHDST2"= 93,
         "Q56ConoHDST3"=139,
         "Q56ConoHDST4"=197) %>%  
  mutate( Q56ConoHDS = ifelse (is.na(Q56ConoHDST1)==FALSE, Q56ConoHDST1, ifelse(is.na(Q56ConoHDST2)==FALSE, Q56ConoHDST2,ifelse(is.na(Q56ConoHDST3)==FALSE, Q56ConoHDST3, Q56ConoHDST4))))%>% 
  mutate(Q56ConoHDS = factor(Q56ConoHDS, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))


qualtrics2<- qualtrics2 %>%  
  rename("Q57ImpHDST1"=74,
         "Q57ImpHDST2"= 94,
         "Q57ImpHDST3"=140,
         "Q57ImpHDST4"=198) %>%  
  mutate( Q57ImpHDS = ifelse (is.na(Q57ImpHDST1)==FALSE, Q57ImpHDST1, ifelse(is.na(Q57ImpHDST2)==FALSE, Q57ImpHDST2,ifelse(is.na(Q57ImpHDST3)==FALSE, Q57ImpHDST3, Q57ImpHDST4))))%>% 
  mutate(Q57ImpHDS = factor(Q57ImpHDS, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))

qualtrics2<- qualtrics2 %>%  
  rename("Q58IntHDST1"=75,
         "Q58IntHDST2"= 95,
         "Q58IntHDST3"=141,
         "Q58IntHDST4"=199) %>%  
  mutate( Q58IntHDS = ifelse (is.na(Q58IntHDST1)==FALSE, Q58IntHDST1, ifelse(is.na(Q58IntHDST2)==FALSE, Q58IntHDST2,ifelse(is.na(Q58IntHDST3)==FALSE, Q58IntHDST3, Q58IntHDST4))))%>% 
  mutate(Q58IntHDS = factor(Q58IntHDS, levels = c("Completamente en desacuerdo", "2", "3", "Ni de acuerdo ni en desacuerdo", "5", "6", "Completamente de acuerdo")))


qualtrics2<- qualtrics2 %>%  
  rename("Q59PrecioHDST1"=76,
         "Q59PrecioHDST2"= 96,
         "Q59PrecioHDST3"=142,
         "Q59PrecioHDST4"=200)  %>%  
  mutate(Q59PrecioHDS = ifelse (is.na(Q59PrecioHDST1)==FALSE, Q59PrecioHDST1, ifelse(is.na(Q59PrecioHDST2)==FALSE, Q59PrecioHDST2,
                                                                                     ifelse(is.na(Q59PrecioHDST3)==FALSE, Q59PrecioHDST3, Q59PrecioHDST4))))%>% 
  mutate(Q59PrecioHDS = factor(Q59PrecioHDS, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q60MarcaHDST1"=77,
         "Q60MarcaHDST2"= 97,
         "Q60MarcaHDST3"=143,
         "Q60MarcaHDST4"=201) %>%  
  mutate(Q60MarcaHDS = ifelse (is.na(Q60MarcaHDST1)==FALSE, Q60MarcaHDST1, ifelse(is.na(Q60MarcaHDST2)==FALSE, Q60MarcaHDST2,
                                                                                  ifelse(is.na(Q60MarcaHDST3)==FALSE, Q60MarcaHDST3, Q60MarcaHDST4))))%>% 
  mutate(Q60MarcaHDS = factor(Q60MarcaHDS, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q61PagoHDST1"=78,
         "Q61PagoHDST2"= 98,
         "Q61PagoHDST3"=144,
         "Q61PagoHDST4"=202) %>%  
  mutate(Q61PagoHDS = ifelse (is.na(Q61PagoHDST1)==FALSE, Q61PagoHDST1, ifelse(is.na(Q61PagoHDST2)==FALSE, Q61PagoHDST2,
                                                                               ifelse(is.na(Q61PagoHDST3)==FALSE, Q61PagoHDST3, Q61PagoHDST4))))%>% 
  mutate(Q61PagoHDS = factor(Q61PagoHDS, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q62DevolucionesHDST1"=79,
         "Q62DevolucionesHDST2"= 99,
         "Q62DevolucionesHDST3"=145,
         "Q62DevolucionesHDST4"=203)  %>%  
  mutate(Q62DevolucionesHDS = ifelse (is.na(Q62DevolucionesHDST1)==FALSE, Q62DevolucionesHDST1, ifelse(is.na(Q62DevolucionesHDST2)==FALSE, Q62DevolucionesHDST2,
                                                                                                       ifelse(is.na(Q62DevolucionesHDST3)==FALSE, Q62DevolucionesHDST3, Q62DevolucionesHDST4))))%>% 
  mutate(Q62DevolucionesHDS = factor(Q62DevolucionesHDS, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q63EnvioHDST1"=80,
         "Q63EnvioHDST2"= 100,
         "Q63EnvioHDST3"=146,
         "Q63EnvioHDST4"=204)  %>%  
  mutate(Q63EnvioHDS = ifelse (is.na(Q63EnvioHDST1)==FALSE, Q63EnvioHDST1, ifelse(is.na(Q63EnvioHDST2)==FALSE, Q63EnvioHDST2,
                                                                                  ifelse(is.na(Q63EnvioHDST3)==FALSE, Q63EnvioHDST3, Q63EnvioHDST4))))%>% 
  mutate(Q63EnvioHDS = factor(Q63EnvioHDS, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))


qualtrics2<- qualtrics2 %>%  
  rename("Q64SeguimientoHDST1"=81,
         "Q64SeguimientoHDST2"= 101,
         "Q64SeguimientoHDST3"=147,
         "Q64SeguimientoHDST4"=205)  %>%  
  mutate(Q64SeguimientoHDS = ifelse (is.na(Q64SeguimientoHDST1)==FALSE, Q64SeguimientoHDST1, ifelse(is.na(Q64SeguimientoHDST2)==FALSE, Q64SeguimientoHDST2,
                                                                                                    ifelse(is.na(Q64SeguimientoHDST3)==FALSE, Q64SeguimientoHDST3, Q64SeguimientoHDST4))))%>% 
  mutate(Q64SeguimientoHDS = factor(Q64SeguimientoHDS, levels = c("Muy bajo", "2", "3", "Medio", "5", "6", "Muy alto")))

  
  ###################################################################################################
  ######################################## Demográficos ################################################
  ##############################################################################################
  

  qualtrics2<- qualtrics2 %>%  
    rename("Q65ComodobuscaInet"=226)  %>%  
    rename("Q66ComodocompraInet"=227)  %>%  
    rename("Sexo"=228) %>%  
    rename(Nacimiento=229) %>% 
    mutate (edad= 2018-Nacimiento) %>%  
    rename("Estudios"=230)  %>%  
    rename("Ocupacion"=231) 

qualtrics2 <- qualtrics2 %>%
  mutate_at(c("Q4_ZapOnlineFREQ","Q21_MovilOnlineFREQ", "Q52_HDSFREQ"),
            funs(recode(., "Nunca"=0, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, "Siempre"=10, .default = NaN))) %>%
  mutate_at(c("Q9ConoZapa","Q10ImpZapa", "Q11IntZapa", 
              "Q25ConoMovil", "Q26ImpMovil", "Q27IntMovil",
              "Q40ConoBolis", "Q41ImpBolis", "Q42IntBolis",
              "Q56ConoHDS", "Q57ImpHDS", "Q58IntHDS"),
            funs(recode(., "Completamente en desacuerdo"=1, "2"=2, "3"=3, "Ni de acuerdo ni en desacuerdo"=4,"5"=5, "6"=6, "Completamente de acuerdo"=7, .default = NaN))) %>%
  mutate_at(c("Q12PrecioZapa","Q13MarcaZapa", "Q14PagoZapa","Q15DevolZapa", "Q16EnvioZapa", "Q17SeguiZapa",
              "Q28PrecioMovil", "Q29MarcaMovil", "Q30PagoMovil", "Q31DevolucionesMovil", "Q32EnvioMovil", "Q33SeguimientoMovil",
              "Q43PrecioBolis", "Q44MarcaBolis", "Q45PagoBolis", "Q46DevoluBolis", "Q47EnvioBolis", "Q48SeguimientoBolis",
              "Q59PrecioHDS","Q60MarcaHDS", "Q61PagoHDS", "Q62DevolucionesHDS", "Q63EnvioHDS", "Q64SeguimientoHDS" ),
            funs(recode(., "Muy bajo"=1,  "2"=2, "3"=3, "Medio"=4,"5"=5, "6"=6, "Muy alto"=7, .default = NaN)))%>%
  mutate_at(c("Q65ComodobuscaInet","Q66ComodocompraInet"),
            funs(recode(.,"1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8,"9"=9, "Extremadamente cómodo"=10, .default = NaN)))



qualtrics_data <- qualtrics2 %>%
  select("Response ID", "Start Date", "Duration (in seconds)", 
         TareaZapatillas, secZapa, Q1_CompraZapatillas, Q2_ZapatillasFrecuencia,Q3_ZapatillasOnline, Q4_ZapOnlineFREQ,
         Q5_AtractZapatillas,Q6modelozapatilla,Q7_DifCompraZapa, Q8_DifPostZapa,
         Q9ConoZapa,Q10ImpZapa, Q11IntZapa,
         Q12PrecioZapa,Q13MarcaZapa, Q14PagoZapa,Q15DevolZapa, Q16EnvioZapa, Q17SeguiZapa,
         TareaMovil, secTele, Q18_CompraMovil, Q19_MovilFrecuencia, Q20_MovilOnline, Q21_MovilOnlineFREQ, 
         Q22_AtractMovil, Q23_ModeloMovil, Q23_DifCompraMovil, Q23_DifCompraMovil, Q24_DifPostMovil,
         Q25ConoMovil, Q26ImpMovil, Q27IntMovil,
         Q28PrecioMovil, Q29MarcaMovil, Q30PagoMovil, Q31DevolucionesMovil, Q32EnvioMovil, Q33SeguimientoMovil,
         TareaBolis, secBoli, Q34_bolis, Q35_bolisFrecuencia, Q36_bolisOnline,Q37_bolisOnlineFREQ,
         Q38_AtractBolis,Q38_ModeloBoli, Q39_DifCompraBoli, Q40_DifPostBoli,
         Q40ConoBolis, Q41ImpBolis, Q42IntBolis,
         Q43PrecioBolis, Q44MarcaBolis, Q45PagoBolis, Q46DevoluBolis, Q47EnvioBolis,Q48SeguimientoBolis,
         TareaHDS, secHd, Q49_CompraHDS, Q50_HDSFrecuencia, Q51_HDSOnline, Q52_HDSFREQ,
         Q53_AtractHDS, Q53_ModeloHDS, Q54_DifCompraHDS, Q55_DifPostHDS, 
         Q56ConoHDS, Q57ImpHDS, Q58IntHDS,
         Q59PrecioHDS, Q60MarcaHDS, Q61PagoHDS, Q62DevolucionesHDS, Q63EnvioHDS, Q64SeguimientoHDS,
         Q65ComodobuscaInet, Q66ComodocompraInet, Sexo, edad, Estudios, Ocupacion)

rm(ls=qualtrics,qualtrics2)

save(qualtrics_data, file=here("data","qualtrics_data"))
  