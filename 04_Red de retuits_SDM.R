
#-------------------------RED de retuits-------------------------------------

library(tidyverse)
library(igraph)
library(networkD3)


# 1-Creo la red en igraph

red <- SD%>%
  filter(is_retweet == TRUE)%>%
  select(screen_name,retweet_screen_name) #columnas que integrarán la red

red <- graph_from_data_frame(red) #Creo el el objeto igraph para darle formato red 
class(red)
V(red)
E(red)


# 2-ploteo con NetworkD3


red3D <- igraph_to_networkD3(red)

vinculos <- red3D[["links"]]
vertices <- red3D[["nodes"]]

forceNetwork(Links = vinculos, Nodes = vertices, 
             Source = 'source', Target = "target", 
             NodeID = 'name', Group = 'name', opacity = 1.8, 
             fontSize = 16, linkDistance = 100, zoom = TRUE)


