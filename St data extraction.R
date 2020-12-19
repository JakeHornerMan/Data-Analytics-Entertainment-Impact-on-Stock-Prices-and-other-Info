#please import dis.us Environment>Import Dataset/dis.us(in project folder)
#please import sne.us Environment>Import Dataset/sne.us(in project folder)
#please import atvi.us Environment>Import Dataset/atvi.us(in project folder)
#these next 3 were not used in the report
#please import fox.us Environment>Import Dataset/fox.us(in project folder)
#please import ttwo.us Environment>Import Dataset/ttwo.us(in project folder)
#please import nflx.us Environment>Import Dataset/ttwo.us(in project folder)
require(tidyverse)
library(readr)
library(dplyr)
#check avg number of rows per year = 251
i <-nrow(filter(atvi.us, grepl('2015', Date)))
q <-c(251/4)

#Fucnctions
#quaterly open price change function
QDiff<- function(dataframe,startDate,endDate){
  comparleadx<-grep(startDate, dataframe$Date)
  comparleady<-grep(endDate,dataframe$Date)
  diff <- dataframe$Close[comparleady]-dataframe$Open[comparleadx]
  diff
}
startOpen<- function(dataframe,startDate){
  comparleadx<-grep(startDate, dataframe$Date)
  dataframe$Open[comparleadx]
}
endClose<- function(dataframe,endDate){
  comparleady<-grep(endDate,dataframe$Date)
  dataframe$Close[comparleady]
}
percentageDiff <-function(x,y){
  pd<-(-(x-y)/((x+y)/2))*100
  pd
}

#DISNEY
#dataset for 2006-2015 stocks market disney and prep for time series
disney_stock <- filter(dis.us, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015',Date))
disney_timeseries <-ts(disney_stock$Open,start=c(2006, 01, 01),end=c(2016, 01, 01), frequency =251)
plot(disney_timeseries, col=rgb(0.2,0.1,0.5,0.9), xlab="Year" , ylab="Open daily", ylim=c(0,150))
disney_stock$diffNext <- lead(disney_stock$Open,1) - disney_stock$Open
disney_stock$diffPrev <- disney_stock$Open - lag(disney_stock$Open,1)
#Q4 2012 for the Avengers
open<-startOpen(disney_stock,'2012-10-01')
open
close<-endClose(disney_stock,'2012-12-31')
close
QDiff(disney_stock,'2012-10-01','2012-12-31')
percentageDiff(open,close)
#2010-06-16  to Q3 2012 for Toy Story 3
open<-startOpen(disney_stock,'2010-09-16')
open
close<-endClose(disney_stock,'2010-12-30')
close
QDiff(disney_stock,'2010-09-16','2010-12-30')
percentageDiff(open,close)
#Q4 2013 Frozen
open<-startOpen(disney_stock,'2013-10-01')
open
close<-endClose(disney_stock,'2013-12-31')
close
QDiff(disney_stock,'2013-10-01','2013-12-31')
percentageDiff(open,close)
#Q4 2013 Star Wars Force Awakens
open<-startOpen(disney_stock,'2015-10-01')
open
close<-endClose(disney_stock,'2015-12-31')
close
QDiff(disney_stock,'2015-10-01','2015-12-31')
percentageDiff(open,close)

#ACTIVISION
#dataset for 2006-2015 stocks market Activision and prep for time series
activision_stock <- filter(atvi.us, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015',Date))
activision_timeseries <-ts(activision_stock$Open,start=c(2006),end=c(2016),frequency =251)
plot(activision_timeseries, col=rgb(0.2,0.1,0.5,0.9), xlab="Year" , ylab="Open daily", ylim=c(0,150))
activision_stock$diffNext <- lead(activision_stock$Open,1) - activision_stock$Open
activision_stock$diffPrev <- activision_stock$Open - lag(activision_stock$Open,1)
#Q4 2009 for Call of Duty: Modern Warfare 2
open<-startOpen(activision_stock,'2009-10-01')
open
close<-endClose(activision_stock,'2009-12-31')
close
QDiff(activision_stock,'2009-10-01','2009-12-31')
percentageDiff(open,close)
#Q4 2010 for Call of Duty: Black Ops
open<-startOpen(activision_stock,'2010-10-01')
open
close<-endClose(activision_stock,'2010-12-31')
close
QDiff(activision_stock,'2010-10-01','2010-12-31')
percentageDiff(open,close)
#Q4 2010 for Call of Duty: Modern Warfare 3
open<-startOpen(activision_stock,'2011-10-03')
open
close<-endClose(activision_stock,'2011-12-30')
close
QDiff(activision_stock,'2011-10-03','2011-12-30')
percentageDiff(open,close)
#Q4 2010 for Call of Duty: Black Ops 2
open<-startOpen(activision_stock,'2012-10-01')
open
close<-endClose(activision_stock,'2012-12-31')
close
QDiff(activision_stock,'2012-10-01','2012-12-31')
percentageDiff(open,close)

#SONY 
sony_stock <- filter(sne.us, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015',Date))
sony_timeseries <-ts(sony_stock$Open,start=c(2006),end=c(2016),frequency =251)
plot(sony_timeseries, col=rgb(0.2,0.1,0.5,0.9), xlab="Year" , ylab="Open daily", ylim=c(0,150))
sony_stock$diffNext <- lead(sony_stock$Open,1) - sony_stock$Open
sony_stock$diffPrev <- sony_stock$Open - lag(sony_stock$Open,1)
#PS3
open<-startOpen(sony_stock,'2006-11-13')
open
close<-endClose(sony_stock,'2007-12-31')
close
QDiff(sony_stock,'2006-11-13','2007-12-31')
percentageDiff(open,close)
#PS4
open<-startOpen(sony_stock,'2013-11-15')
open
close<-endClose(sony_stock,'2014-12-31')
close
QDiff(sony_stock,'2013-11-15','2014-12-31')
percentageDiff(open,close)
#LAST OF US ps3
open<-startOpen(sony_stock,'2013-06-14')
open
close<-endClose(sony_stock,'2013-09-30')
close
QDiff(sony_stock,'2013-06-14','2013-09-30')
percentageDiff(open,close)
#LAST OF US ps4
open<-startOpen(sony_stock,'2014-07-01')
open
close<-endClose(sony_stock,'2014-09-30')
close
QDiff(sony_stock,'2013-06-14','2013-09-30')
percentageDiff(open,close)
#Spider-man 3
open<-startOpen(sony_stock,'2007-04-02')
open
close<-endClose(sony_stock,'2007-06-29')
close
QDiff(sony_stock,'2013-06-14','2013-09-30')
percentageDiff(open,close)
#the Smurfs
open<-startOpen(sony_stock,'2011-07-01')
open
close<-endClose(sony_stock,'2011-09-30')
close
QDiff(sony_stock,'2011-07-01','2011-09-30')
percentageDiff(open,close)
#Skyfall
open<-startOpen(sony_stock,'2012-10-01')
open
close<-endClose(sony_stock,'2012-12-31')
close
QDiff(sony_stock,'2012-10-01','2012-12-31')
percentageDiff(open,close)

#dataset for 2006-2015 stocks market Take Two Interactive and prep for time series
taketwo_stock <- filter(ttwo.us, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015',Date))
taketwo_timeseries <-ts(taketwo_stock$Open,start=c(2006),end=c(2016),frequency =251)
plot(taketwo_timeseries, col=rgb(0.2,0.1,0.5,0.9), xlab="Year" , ylab="Open daily", ylim=c(0,150))

#dataset for 2006-2015 stocks market Netflix and prep for time series
Netflix_stock <- filter(nflx.us, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015',Date))
Netflix_timeseries <-ts(Netflix_stock$Close,start=c(2006),end=c(2016),frequency =251)
plot(Netflix_timeseries, col=rgb(0.2,0.1,0.5,0.9), xlab="Year" , ylab="Open daily",ylim=c(0,150))

#dataset for 2006-2015 stocks market FOX and prep for time series
fox_stock <- filter(fox.us, grepl('2006|2007|2008|2009|2010|2011|2012|2013|2014|2015',Date))
fox_timeseries <-ts(fox_stock$Open,start=c(2006),end=c(2016),frequency =251)
plot(fox_timeseries, col=rgb(0.2,0.1,0.5,0.9),, xlab="Year" , ylab="Open daily", ylim=c(0,150))


