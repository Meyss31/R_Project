library(readr)
appearances = read_csv('data/appearances.csv')
characters = read_csv('data/characters.csv')
scenes = read_csv('data/scenes.csv')
episodes = read_csv('data/episodes.csv')
populations = read_csv('data/populations.csv')


#nbdeath par sex
library(dplyr)
library(ggplot)
char_sex = characters %>% 
  select(sex, killedBy) %>% 
  na.omit()
ggplot(data = char_sex)+ 
  geom_bar(aes(x=sex))+ 
  scale_x_discrete("Sex")


#nbdeath par house

char_house = characters %>% 
  select(house, killedBy) %>% 
  na.omit() %>% group_by(house) %>% 
  summarise(nb=n()) 
char_house_ordered = char_house %>% 
  arrange(desc(nb)) %>% 
  mutate(house=factor(house,levels=house))
ggplot(data = char_house_ordered)+ 
  geom_bar(aes(x=house, y=nb), stat='identity')+ 
  scale_x_discrete("house")+ 
  scale_y_continuous("deaths")+ coord_flip()


#nbdeath par saison 

death_episode = episodes %>% 
  left_join(scenes) %>% 
  group_by(seasonNum, episodeNum) %>% 
  summarize (nbe=sum(nbdeath))
death_saison = death_episode %>% 
  group_by(seasonNum) %>% 
  summarize (nbs=sum(nbe)) 
seasons = death_saison %>% 
  arrange(nbs) %>% 
  mutate(snb=factor(seasonNum,levels = seasonNum))
df = death_episode %>% left_join(seasons) %>% filter(!is.na(snb))
ggplot(data = df)+ 
  geom_bar(aes(y=snb,x=nbe,fill=factor(episodeNum,level=10:1)), stat="identity")+ 
  scale_fill_brewer("episode",palette = "Spectral")+theme_bw()+ 
  geom_text(data=seasons,aes(y=snb,x=nbs, label=paste(round(nbs),'deaths')),hjust = "left")+ 
  scale_x_continuous("deaths", breaks = seq(0,100,by=25),limits = c(0,100),expand = c(0,1))+ 
  scale_y_discrete("season")


# Death evolution per season
deaths = scenes %>% 
  left_join(episodes) %>% 
  group_by(seasonNum) %>% 
  summarize(nbd=sum(nbdeath))
ggplot(data=deaths)+
  geom_line(aes(x=seasonNum, y=nbd), stat='identity')+
  scale_x_continuous("season", breaks =seq(1,8, by=1), limits =c(1,8), expand=c(0,1))+
  scale_y_continuous("deaths")


#Death par episode
death_episode=scenes%>%
  left_join(episodes) %>%filter(seasonNum==1)%>% 
  group_by(episodeTitle, episodeNum) %>% 
  summarize(nbe=sum(nbdeath), duration_epi=sum(duration))
ggplot(death_episode,aes(x=duration_epi/60,y=nbe,col=factor(episodeNum)))+
  geom_point(aes(size=nbe))+
  geom_text(data=death_episode,aes(label=episodeTitle),vjust=-0.6)+
  scale_x_continuous("Durée de l'épisode",seq (50, 65, by=3), limits = c(50,65))+
  scale_y_continuous("Nombre des morts",limits = c(0,8))+
  scale_color_brewer("Episode",palette ="Spectral")+
  guides(colour = "legend", size = "legend")+
  theme_bw()
