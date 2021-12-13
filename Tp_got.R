library(readr)
library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
appearances = read_csv('data/appearances.csv')
characters = read_csv('data/characters.csv')
scenes = read_csv('data/scenes.csv')
episodes = read_csv('data/episodes.csv')
populations = read_csv('data/populations.csv')
locations=st_read("./data/GoTRelease/Locations.shp",crs=4326)

#Analyse globale de la série

#La mort par sex

#Créer une dataframe des morts par sex
deaths_by_sex = characters %>% 
  select(sex, killedBy) %>% 
  group_by(sex)%>% 
  summarize(nbd_by_sex=n())%>%
  na.omit()
#Construire le piechart montrant le nombre de morts par sex
ggplot(deaths_by_sex, aes(x="", y=nbd_by_sex, fill=sex)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)


#La mort par house

#Créer une dataframe des morts par house

char_house = characters %>% 
  select(house, killedBy) %>% 
  na.omit() %>% group_by(house) %>% 
  summarise(nbd_by_house=n())%>%
  arrange(desc(nbd_by_house)) %>% 
  mutate(house=factor(house,levels=house)) #ordonner le dataframe par nombre de morts cummulé par house 
#Construire le barplot mobtrant la mort par house
ggplot(data = char_house)+ 
  geom_bar(aes(x=house, y=nbd_by_house), stat='identity')+ 
  scale_x_discrete("house")+ 
  scale_y_continuous("deaths")+ coord_flip()


#La mort par saison 

#Créer un dataframe de nombre de mort par episode dans un saison
death_by_episode = episodes %>% 
  left_join(scenes) %>% 
  group_by(seasonNum, episodeNum) %>% 
  summarize (nbd_by_episode=sum(nbdeath))
#Créer un dataframe de nombre de mort par par saison
death_by_season = death_by_episode %>% 
  group_by(seasonNum) %>% 
  summarize (nbd_by_season=sum(nbd_by_episode))%>% 
  arrange(nbd_by_season) %>% 
  mutate(nbs=factor(seasonNum,levels = seasonNum))
#Créer un dataframe de nombre de mort par par saison et par episode
death_by_epi_seas= death_by_episode %>% left_join(death_by_season) %>% filter(!is.na(nbs))
ggplot(data = death_by_epi_seas)+ 
  geom_bar(aes(y=nbs,x=nbd_by_episode,fill=factor(episodeNum,level=10:1)), stat="identity")+ 
  scale_fill_brewer("episode",palette = "RdYlGn")+theme_bw()+ 
  geom_text(data=death_by_season,aes(y=nbs,x=nbd_by_season, label=paste(round(nbd_by_season))),hjust = "left")+ 
  scale_x_continuous("deaths", breaks = seq(0,100,by=25),limits = c(0,100),expand = c(0,1))+ 
  scale_y_discrete("season")


# Evolution de la mort durant la série

#Créer un dataframe des morts dans la série par saison
deaths_in_serie = scenes %>% 
  left_join(episodes) %>% 
  group_by(seasonNum) %>% 
  summarize(nbd_in_serie=sum(nbdeath))
#Construire le lineplot montrant l'évolution de la mort dans la série
ggplot(data=deaths_in_serie)+
  geom_line(aes(x=seasonNum, y=nbd_in_serie), stat='identity')+
  scale_x_continuous("Season", breaks =seq(1,8, by=1), limits =c(1,8), expand=c(0,1))+
  scale_y_continuous("Deaths")


#Analuse par saison

#La mort par episode dans chaque saison

#Créer un dataframe de morts par saison et leurs durée d'apparition par episode
death_by_episode = episodes %>% 
  left_join(scenes) %>% 
  group_by(seasonNum, episodeNum) %>% 
  summarize (nbd_by_episode=sum(nbdeath))
#Créer un dataframe de nombre de mort par par saison
death_by_season = death_by_episode %>% 
  group_by(seasonNum) %>% 
  summarize (nbd_by_season=sum(nbd_by_episode))%>% 
  arrange(nbd_by_season) %>% 
  mutate(nbs=factor(seasonNum,levels = seasonNum))
#Créer un dataframe de nombre de mort par par saison et par episode
death_by_epi_seas= death_by_episode %>% left_join(death_by_season) %>% filter(!is.na(nbs))
#Construire le scatterplot montrant le nombre des morts par épisode dans chaque saison
ggplot(data = death_by_epi_seas)+ 
  geom_bar(aes(y=nbs,x=nbd_by_episode,fill=factor(episodeNum,level=10:1)), stat="identity")+ 
  scale_fill_brewer("episode",palette = "RdYlGn")+theme_bw()+ 
  geom_text(data=death_by_season,aes(y=nbs,x=nbd_by_season, label=paste(round(nbd_by_season))),hjust = "left")+ 
  scale_x_continuous("deaths", breaks = seq(0,100,by=25),limits = c(0,100),expand = c(0,1))+ 
  scale_y_discrete("season")


#La mort par apparence

#Créer un dataframe de nombre de mort par episode et leurs durées d'apparition
death_epi_by_app =  scenes%>% 
  left_join(appearances) %>%
  left_join(characters) %>%filter(!is.na(killedBy)) %>% 
  left_join(episodes) %>%filter(seasonNum=="1") %>% 
  group_by(seasonNum, episodeNum, name)%>% 
  summarize(dur_epi=sum(duration)) %>% 
  arrange(desc(dur_epi))
#Créer un dataframe de nombre de mort par saison et leurs durées d'apparition
death_seas_by_app = death_epi_by_app %>% group_by(seasonNum, name)%>%
  summarize(dur_seas=sum(dur_epi))%>%
  filter(dur_seas>60*45) %>% 
  arrange(dur_seas)%>%
  mutate(nameC=factor(name,levels = name))
#Créer un dataframe de nombre de mort par par saison et par episode et leur durée d'apparition
db = death_epi_by_app %>% left_join(death_seas_by_app) %>%filter(!is.na(nameC))
#Construire le boxplot montrant la durée d'apparition des personnages mortes dans chaque saison
ggplot(data=db)+
  geom_boxplot(aes(x=nameC,y=dur_epi, fill=factor(seasonNum)))+
  scale_x_discrete("Characters Mortes")+
  scale_y_continuous("durée d'apparition")


#Les killers et les familles qu'il a tuée le plus 

#Créer un dataframe des plus grands meurterieux de la série
top_killers = characters %>% 
  group_by(killedBy) %>% filter(!is.na(killedBy)) %>% 
  summarise(nb =n()) %>% 
  arrange(desc(nb)) %>% 
  head(5)
#Créer un dataframe des houses le plus touchés par la mort
top_houses_killed=characters %>% filter(!is.na(killedBy)) %>%filter(!is.na(house))%>%
  group_by(house) %>% 
  summarize(nbk=n()) %>% arrange(desc(nbk)) %>% head(5)
#Créer un dataframe des meurterieux et des houses le plus touchés par la mort
killers=top_killers %>% 
  left_join(characters) %>% filter(!is.na(killedBy)) %>% filter(!is.na(house))%>%
  left_join(top_houses_killed) %>% 
  group_by (killedBy, house)  %>%
  summarize(nbk=n())
killers=pivot_wider(killers,names_from=house,values_from=nbk)
killers[is.na(killers)]=0
killers=killers%>% select(Stark,Lannister,Dothraki,`Gard Royal`,'Night Gard')
killers=killers %>% filter(killedBy=='Jon Snow')
dt=subset(killers, select = -c(killedBy))
colnames(dt) <- c('Stark','Lannister','Dothraki','`Gard Royal`','Night Gard')
rownames(dt) <- c('Jon Snow')
dt = rbind(rep(7,5) , rep(0,5) , dt)
colors_border=c( rgb(0.2,0.5,0.5,0.5), rgb(0.8,0.2,0.5,0.5) , rgb(0.7,0.5,0.1,0.5), rgb(1,0.7,0.8,0.5), rgb(0.5,0,0.5,0.5) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.5) , rgb(0.7,0.5,0.1,0.5), rgb(1,0.7,0.8,0.5), rgb(0.5,0,0.5,0.5) )
radarchart( dt  , axistype=1 , 
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            cglcol="blue", cglty=1, axislabcol="blue", caxislabels=seq(0,8,2), cglwd=0.8,
            vlcex=0.8)
legend(x=1.6, y=1, legend = 'Jon Snow', bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=5)