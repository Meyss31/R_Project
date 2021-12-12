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
