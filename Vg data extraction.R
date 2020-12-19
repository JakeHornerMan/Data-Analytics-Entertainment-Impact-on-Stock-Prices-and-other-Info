#install.packages("tidyverse")
require(tidyverse)
library(readr)
library(ggplot2)
library(dplyr) 
#extrating all vg published games
vgsales <- read_csv("vgsales.csv")
vgsales<-vgsales %>% filter(vgsales$Year > 2005)
vgsales<-vgsales %>% filter(vgsales$Year < 2016)
#View(vgsales)

#creating vaiable for approximate revenue
#dataset used is total sales per million
#avg game price is 50$
x<-c(50)

#Activison
#creating dataset containing All Activision products
Activision_vg <- vgsales %>%
  select(Name,Year,Platform,Publisher,Global_Sales)%>%
  filter(Publisher == "Activision")

Activision_vgTR <-Activision_vg %>%
  group_by(Year)%>%
  summarise(Total_Sales = (sum(Global_Sales)*x))

Activision_vgTP <-Activision_vg %>% group_by(Year) %>%
  summarise(Count = n())

ActivisonSameGame<-Activision_vg%>%
  group_by(Name)%>%
  summarise(Total_unitSales = (sum(Global_Sales)*x))
ActivisonSameGame$TotalRevenue <- Activision_vg%>%
  group_by(Name)%>%
  summarise(Total_unitSales = (sum(Global_Sales)))

#Sony
Sony_vg<-filter(vgsales, grepl('Sony',Publisher))

Sony_vgTR <-Sony_vg %>%
  group_by(Year)%>%
  summarise(Total_Sales = (sum(Global_Sales)*x))

Sony_vgTP<-Sony_vg %>% group_by(Year) %>%
  summarise(Count = n())

SonySameGame<-Sony_vg%>%
  group_by(Name)%>%
  summarise(Total_unitSales = (sum(Global_Sales)))

#creating dataset containing All Take-Two Interactive products
TT_vg <-vgsales %>%
  select(Name,Year,Platform,Publisher,Global_Sales)%>%
  filter(Publisher == "Take-Two Interactive")

TT_vgTR <-TT_vg %>%
  group_by(Year)%>%
  summarise(Total_Sales = (sum(Global_Sales)*x))

TT_vgTP <-TT_vg %>% group_by(Year) %>%
  summarise(Count = n())
