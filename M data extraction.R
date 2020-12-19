#install.packages("tidyverse")
require(tidyverse)
library(readr)
library(dplyr)
#revenue is full million i want to reduce to a mor manageable rounded number
x<-c(1000000)

#extracting imdb data and cleaning
movies_metadata <- read_csv("movies_metadata.csv")
movies_data <- filter(movies_metadata, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015', release_date))
movies_data <- movies_data %>%
  filter(status == "Released") %>%
  select(title,release_date,production_companies,revenue,popularity)
movies_data$release_date = substr(movies_data$release_date,1,nchar(movies_data$release_date)-1)
movies_data$revenue <- as.double(movies_data$revenue)
movies_data$revenue <- movies_data$revenue/x
#View(movies_data)
#top 50 highest grossing movies
Top50Movies<-movies_metadata[with(movies_metadata, order(-revenue)), ] %>% select (title,production_companies,revenue,release_date)
Top50Movies<-head(Top50Movies,50)

#creating datasets containing All Disney products
Disney_movies <- filter(movies_data, grepl('Disney|Pixar|Marvel',production_companies))
Disney_mTR <-Disney_movies %>%
  group_by(release_date)%>%
  summarise(Total_Sales = sum(revenue))
Disney_mTP <-Disney_movies %>%
  group_by(release_date) %>%
  summarise(Count = n())

Disney_moviesTM<-filter(Top50Movies, grepl('Disney|Pixar|Marvel|Lucasfilm',production_companies))

#creating dataset containing All Sony products
Sony_movies <- filter(movies_data, grepl('Sony|Columbia|Screen Gems|Imageworks|3000|Tristar',production_companies))
Sony_mTR <-Sony_movies %>%
  group_by(release_date)%>%
  summarise(Total_Sales = sum(revenue))
Sony_mTP <-Sony_movies %>%
  group_by(release_date) %>%
  summarise(Count = n())

Sony_moviesTM<-filter(Top50Movies, grepl('Sony|Columbia|Screen Gems|Imageworks|3000|Tristar',production_companies))

#creating dataset containing All Fox products
Fox_movies <- filter(movies_data, grepl('Fox',production_companies))
Fox_mTR <-Fox_movies %>%
  group_by(release_date)%>%
  summarise(Total_Sales = sum(revenue))
Fox_mTP <-Fox_movies %>%
  group_by(release_date) %>%
  summarise(Count = n())

#creating dataset containing All Netflix products
Netflix_movies <- filter(movies_data, grepl('NetFlix',production_companies))
Netflix_mTP <-Netflix_movies %>%
  group_by(release_date) %>%
  summarise(Count = n())
