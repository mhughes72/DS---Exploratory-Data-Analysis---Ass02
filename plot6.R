plot6 <- function() {
  
  library("dplyr")
  library("plyr")
  library ("ggplot2")
  
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  merged <- merge(NEI, SCC, by="SCC")
  
  y<-filter(merged, grepl('[Vv]ehicles', merged$Short.Name)|
              grepl('[Vv]ehicles', merged$EI.Sector)|
              grepl('[Vv]ehicles', merged$SCC.Level.One)|
              grepl('[Vv]ehicles', merged$SCC.Level.Two)|
              grepl('[Vv]ehicles', merged$SCC.Level.Three)|
              grepl('[Vv]ehicles', merged$SCC.Level.Four)
  )
  
  y<-transform(y,fips=factor(fips))
  y = subset(y, fips=="24510" | fips=="06037", select=c(Emissions, year, fips)) 
  
  rename(y, c("fips"="City"))
  plotdata<-ddply(y, c("year", "fips"),summarize,sum=sum(Emissions))
  

  
#   png("plot6.png")
#   p<-qplot(year, sum, data=plotdata, geom="point", color=fips, size=I(5), alpha=I(.5), 
#            main="Motor Vehicle Related Sources", xlab="Year", ylab="Emissions")
#   print(p)
#   dev.off()
  
  
  return(plotdata)
  
}

