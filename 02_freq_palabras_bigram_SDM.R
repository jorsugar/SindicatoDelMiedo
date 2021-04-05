
#-------------------------Palabras más usadas----------------------

library(tidyverse)
library(tidytext)
library(viridisLite)
library(viridis)

SD <- read_rds("2021-04-02_SD2.rds") #importo la base de datos

terminos <- SD %>%
  filter(!is_retweet) %>% 
  select(text) %>%
  unnest_tokens(palabras, text) %>%
  count(palabras,sort=TRUE) 

articulos <- c("el", "la", "lo", "los", "las", "un", "uno", "una", "unos", "unas")
demostrativosY <- c("este", "éste", "esta", "ésta", "está", "estos", "éstos", "estas", "éstas", "estás", "esto", "ese", "esa", 
                    "esos", "esas", "eso", "aquel", "aquella", "aquellos", "aquellas", "aquello")
posesivos <- c("mi", "mis", "mío", "míos", "mía", "mías", "nuestro", "nuestros", "nuestra", "nuestras", "tu", "tus", "tuyo", 
               "tuyos", "tuya", "tuyas", "sus", "sus", "suyo", "suyos", "suya", "suyas")
personales <- c("yo", "tú", "él", "vos", "nosotros", "ustedes", "ellos", "ellas")
adTiempo <- c("ahora", "antes", "después", "luego", "tarde", "ayer", "temprano", "ya", "todavía", "aún", "pronto", "hoy")
adLugar <- c("aquí", "ahí", "allí", "allá", "cerca", "lejos", "fuera", "alrededor", "aparte", "encima", "debajo", "delante", "detrás")
adCantidad <- c("mucho", "poco", "casi", "muy", "todo", "nada", "algo", "medio", "demasiado", "bastante", "más", "menos", "además", 
                "incluso", "también")
adModo <- c("así", "bien", "mal", "despacio", "deprisa", "como")
adAfirmacion <- c("sí", "también", "asimismo")
adNegacion <- c("no", "tampoco", "jamás", "nunca")
adDuda <- c("acaso", "quizá", "quizás", "tal vez", "a lo mejor")
preguntaY <- c("qué", "cuándo", "cómo", "dónde", "cuál", "por qué", "que", "cuando", "como", "donde", "cual", "porque")
vocales <- c("a", "e", "i", "o", "u")
otras <- c("sindicatodelmiedo" ,"de", "la", "https", "t.co", "y", "en", "del", "es", "se", "con", "para", 
           "te", "les", "por", "al", "su", "si", "hay", "pero", "q", "zarteta", "nos", "le", "toda", "hashtag",
           "me", "danidlsa", "otro")

mi_stopwords <- c(articulos, demostrativosY, posesivos, personales, adTiempo, adLugar, adCantidad, adModo, adAfirmacion, 
                  adNegacion, adDuda, preguntaY, vocales, otras)


terminos <- filter(terminos,!palabras %in% mi_stopwords)

terminos %>%
  top_n(50) %>%
  mutate (palabras = reorder(palabras, n))%>%
  ggplot(aes(n, palabras, fill = palabras))+
  geom_col(show.legend = FALSE)+
  theme_minimal()+
  labs(x = NULL , y = NULL, title = "Top 50 palabras más usadas en el hashtag #SindicatoDelMiedo")


#---------------------Bigram--------------------------------

library(igraph)
library(ggraph)

bigram <- SD %>% 
  select(text) %>%
  unnest_tokens(palabras,text, token = "ngrams", n = 2) %>%
  separate(palabras, c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% mi_stopwords,
         !palabra2 %in% mi_stopwords) %>%
  count(palabra1, palabra2, sort=TRUE) 


grafico_bigram <- bigram %>%
  filter(n > 20) %>%
  graph_from_data_frame()

grafico_bigram
V(grafico_bigram)
E(grafico_bigram)


a <- grid::arrow(type = "closed", length = unit(.1, "inches"))

ggraph(grafico_bigram, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "#3b6968", size = 4) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle("Red de relaciones de dos palabras en tuits del hashtag #SindicatoDelMiedo (frecuencia > 20)")+
  theme_void()
