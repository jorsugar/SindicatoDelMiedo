library(tidyverse)

SD <- read_rds("2021-04-02_SD2.rds")


#---------------------------Top 15 tuiteadores----------------------  

top_tuiteadores <- SD %>%
  filter(!is_retweet)%>%
  count(screen_name)%>%
  arrange(desc(n))%>%
  top_n(15) %>%   
  rename(Usuario = "screen_name", Tuits ="n")

top_tuiteadores <- kable(top_tuiteadores)

#Graficando tuiteadores

top_tuiteadores %>%
  filter(Tuits > 5) %>%
  mutate (Usuario = reorder(Usuario, Tuits))%>% 
  ggplot(aes(Tuits, Usuario, fill = Usuario))+
  geom_col(show.legend = FALSE)+
  theme_minimal()+
  labs(x = NULL , y = NULL, title = "Top tuiteadores (n tuits > 9 )")


#----------------------------Top 15, retuiteadores--------------------

top_retuiteadores <- SD %>%
  filter(is_retweet)%>%
  count(screen_name)%>%
  arrange(desc(n))%>%
  top_n(15)%>%   
  rename(Usuario = "screen_name", Tuits ="n")

top_retuiteadores <- kable(top_retuiteadores)


#Graficando retuiteadores

top_retuiteadores %>%
  filter(Tuits > 21) %>%
  mutate (Usuario = reorder(Usuario, Tuits))%>% 
  ggplot(aes(Tuits, Usuario, fill = Usuario))+
  geom_col(show.legend = FALSE)+
  theme_minimal()+
  labs(x = NULL , y = NULL, title = "Top  15 retuiteadores (n retuits > 22 )")


#----------------------------Tuits populares-----------------------------

library(gt)

mas_retuiteado <- SD %>%
  filter(!is_retweet)%>%
  arrange(desc(retweet_count))
mas_retuiteado %>%
  select(text, screen_name, retweet_count, favorite_count)%>%
  rename(Tuit = "text", Usuario = "screen_name", Retuits = "retweet_count",
         MeGusta = "favorite_count")%>%
  head(5)%>%
  gt()

más_faveado <- SD %>% 
  arrange(desc(favorite_count)) %>% 
  select(text, screen_name, favorite_count, retweet_count)%>%
  
  más_faveado %>%  rename(Tuit = "text", Usuario = "screen_name", MeGusta = "favorite_count",
                          Retuits = "retweet_count")%>%
  head(5)%>%
  gt()

