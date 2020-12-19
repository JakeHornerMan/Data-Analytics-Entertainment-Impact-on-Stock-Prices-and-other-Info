library(tidyverse)
#install.packages("rvest")
library(rvest)
library(readr)
library(ggplot2)
library(dplyr) 

#extract top films
content <- read_html("https://en.wikipedia.org/wiki/List_of_highest-grossing_films")
tables <- content %>% html_table(fill = TRUE)
topMoviesWiki <- tables[[1]]
#topMoviesWiki<-topMoviesWiki %>% filter(topMoviesWiki$Year > 2005)
#topMoviesWiki<-topMoviesWiki %>% filter(topMoviesWiki$Year < 2016)
#after extracting i relized this isnt useful without production companys in the table
#Sorting my IMDB table by top revenue gave the same results but this dataset was useful to compare


#extract top games
content <- read_html("https://en.wikipedia.org/wiki/List_of_best-selling_video_games")
tables <- content %>% html_table(fill = TRUE)
topGamesWiki <- tables[[2]]
colnames(topGamesWiki) <- c("Rank", "Title", "Sales", "Platform", "release_date", "Studio", "Publisher", "ref")
topGamesWiki <- filter(topGamesWiki, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015', release_date))

ActivisionTG <-topGamesWiki %>%
  select(Rank,Title,Sales,Platform,Publisher,Studio,release_date)%>%
  filter(Publisher == "Activision")

SonyTG <-topGamesWiki %>%
  select(Rank,Title,Sales,Platform,Publisher,Studio,release_date)%>%
  filter(Publisher == "Sony Computer Entertainment")

