library(reader)
library(dplyr)

appearances = read.csv("data/appearances.csv")
characters = read.csv("data/characters.csv")
episodes = read.csv("data/episodes.csv")
scenes = read.csv("data/scenes.csv")

seine_maritime = read.csv("data/data_seine_maritime.csv")


killers_data = characters %>% select(name, killedBy) %>% na.omit()
