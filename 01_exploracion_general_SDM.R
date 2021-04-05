
library(rtweet)
library(httpuv)
library(tidyverse)
library(lubridate)
library(knitr)

SD <- search_tweets("#SindicatoDelMiedo", n = 18000) #extraigo los datos con el paquete rtweet

write_rds(SD, paste0(Sys.Date(),"_SD2.rds")) #guardo la base de datos con su fecha

#-----------------Explorando los datos-------------------

names(SD) #nombres de las variables

head(SD$created_at)
tail(SD$created_at)
head(SD$screen_name)
tail(SD$screen_name)

usuarios_unicos <- SD %>%
  pull(screen_name)%>% #pull retorna una columna como vector a diferencia de select que devuelve un dataframe
  unique()%>%
  length()

#Los datos se extrajeron el viernes 2 Abril de 2021 a las 12:30 hs
#Hasta ese momento había 5408 tuits que utilizaban el hashtag #SindicatoDelMiedo incluyendo retuits
#El primer tuit con el hashtag se realizó el lunes 29 a las 12:47 hs por el usuario @bruno_telestca_ 
#El último tuit del hashtag de esta base lo realizó @FranciscoPacoR6 a las 12:27 del viernes 2 de abril
# 2641 usuarios diferentes han utilizado el hashtag

#--------------------Tuits por día---------------------------------

diasTuit <- table(wday(SD$created_at, label = TRUE, abbr = FALSE))
tablaTPD <- kable(sort(diasTuit, decreasing = TRUE),#kable pertenece a la librería Knitr
                  col.names= c("Días", "Tweets"))
tablaTPDF <- data.frame(diasTuit)
tablaTPDF <- tablaTPDF %>%
  select(Días=Var1, Tuits=Freq) %>%  #cambio el nombre de las columnas
  filter(Tuits  > 0 )

ggplot(tablaTPDF, aes(x=Días, y= Tuits))+
  geom_bar(stat = "identity",  
           color = "#3b6968", fill = "#3b6968")+
  ylab("Nº de Tuits")+ 
  xlab(" ")+
  ggtitle("Distribución de Tuits del hashtag #SindicatoDelMiedo por día")+
  theme_minimal()

tablaTPDF <- kable(tablaTPDF)

#Tuits por día
#  |Días      | Tuits|
#  |:---------|-----:|
#  |lunes     |   543|
#  |martes    |  4178|
#  |miércoles |   606|
#  |jueves    |    60|
#  |viernes   |    21|
  

#-----------------Tuits originales del hashtag-------------------------
  
tuits_originales <- SD %>%
  filter(!is_retweet) %>% #saco los que son retuits
  pull(status_id) %>%
  unique() %>%
  length()

retuiteados <- nrow(SD) - tuits_originales
porcentaje_retuiteados <- retuiteados*100/nrow(SD)
porcentaje_retuiteados <- round(porcentaje_retuiteados, digits = 0)

# 1002 tuits originales
# 4406 retuiteados
# 81% de los tuits fueron retuiteados  
