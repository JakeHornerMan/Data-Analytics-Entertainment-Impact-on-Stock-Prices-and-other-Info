#FUNCTIONS
#barplot function vg
barplotProductsvg <- function(tp){
  barplot(tp$Count, xlab = "Year", ylab="Count",ylim=c(0,120))
}

#barplot function m
barplotProductsm <- function(tp){
  barplot(tp$Count, xlab = "Year", ylab="Count",ylim=c(0,60))
}

#Create AREA CHART func Year:TotalSales (vg)
dataTRvg <- function(ts){
  data <- data.frame(
    x=ts$Year, 
    y=ts$Total_Sales
  )
  TRareachart(data)
}
#Create AREA CHART func numOfProducts:TotalSales (vg)
dataTPvg <- function(tp){
  data <- data.frame(
    x=tp$Year, 
    y=tp$Count
  )
  TPareachart(data)
}

#Create AREA CHART func Year:TotalSales (m)
dataTRm <- function(ts){
  data <- data.frame(
    x=ts$release_date, 
    y=ts$Total_Sales
  )
  TRareachart(data)
}
#Create AREA CHART func numOfProducts:TotalSales (m)
dataTPm <- function(tp){
  data <- data.frame(
    x=tp$release_date, 
    y=tp$Count
  )
  TPareachart(data)
}

#Total_Revenue :Year
TRareachart<- function(data){
  # Add line on top
  plot( data$x , data$y , col=rgb(0.2,0.1,0.5,0.9) , type="o" , lwd=3 , xlab="Year" , ylab="Revenue (million)" , pch=20 ,ylim=c(0,6000),xlim=c(2006,2015))
  
  # Fill the area
  polygon( 
    c(min(data$x), data$x , max(data$x)) , 
    c( 0 , data$y ,0) , 
    col=rgb(0.2,0.1,0.5,0.2) , border=F
  )
}
#numOfProducts:Year 
TPareachart<- function(data){
  # Add line on top
  plot( data$x , data$y , col=rgb(0.2,0.1,0.5,0.9) , type="o" , lwd=3 , xlab="Year" , ylab="Count" , pch=20, ylim=c(0,150),xlim=c(2006,2015))
  
  # Fill the area
  polygon( 
    c(min(data$x), data$x , max(data$x)) , 
    c( 0 , data$y , 0) , 
    col=rgb(0.2,0.1,0.5,0.2) , border=F
  )
}

dataTRvg(Sony_vgTR)
#dataTPvg(Sony_vgTP)
barplotProductsvg(Sony_vgTP)

dataTRvg(Activision_vgTR)
#dataTPvg(Activision_vgTP)
barplotProductsvg(Activision_vgTP)

dataTRm(Disney_mTR)
#dataTPm(Disney_mTP)
barplotProductsm(Disney_mTP)

dataTRvg(TT_vgTR)
#dataTPvg(TT_vgTP)
barplotProductsvg(TT_vgTP)

dataTRm(Sony_mTR)
#dataTPm(Sony_mTP)
barplotProductsm(Sony_mTP)

dataTRm(Fox_mTR)
#dataTPm(Fox_mTP)
barplotProductsm(Fox_mTP)

