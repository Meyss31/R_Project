library(readr)
appearances = read_csv('data/appearances.csv')
characters = read_csv('data/characters.csv')
scenes = read_csv('data/scenes.csv')
episodes = read_csv('data/episodes.csv')
populations = read_csv('data/populations.csv')

#voir les infos de la data frame appearances 
dim(appearances)
summary(appearances)
str(appearances)
names(appearances)

#voir les infos de la data frame scenes 
dim(scenes)
summary(scenes)
str(scenes)
names(scenes)

#voir les intersections des data frames appearances et scenes
names(appearances) %in% names(scenes)
intersect(names(appearances), names(scenes))

#Le nombre de personnages mortes dans toute la série 
sum(scenes['nbdeath'])

#Le nombre de personnages mortes dans la première saison 
sum(scenes$nbdeath[scenes$episodeId<=10])

#Le plus grand meurtier de la serie
summary(characters)
sort(table(characters$killedBy), decreasing = TRUE)[1:5]

#la durée de la scene la plus long et l'Id de l'épisode correspondante

print(paste0("l'épisode correspondante est :", scenes$episodeId[which.max(scenes$duration)]))
print(paste0("la durér de la scene la plus longue est :", scenes$duration[which.max(scenes$duration)]))

#La même question en utilisant dplyr
library(dplyr)
scenes %>% arrange(desc(duration)) %>% head(1)[scenes$episodeId]
print(paste0("la durér de la scene la plus longue est :", scenes$duration[which.max(scenes$duration)]))


#nbdeath par sex
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

#Death per appearance
ggplot(appearances %>% left_join(scenes)) %>% 
  left_join(episodes) %>% group_by()
  +geom_boxplot(aes(x=factor(episodeId),y=duration))
