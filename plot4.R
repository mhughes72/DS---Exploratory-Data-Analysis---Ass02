plot4 <- function() {
  
  library("dplyr")
  library("plyr")
  library ("ggplot2")
  
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  merged <- merge(NEI, SCC, by="SCC")
  
  y<-filter(merged, grepl('[Cc]oal', merged$Short.Name)|
              grepl('[Cc]oal', merged$EI.Sector)|
            grepl('[Cc]oal', merged$SCC.Level.One)|
              grepl('[Cc]oal', merged$SCC.Level.Two)|
              grepl('[Cc]oal', merged$SCC.Level.Three)|
              grepl('[Cc]oal', merged$SCC.Level.Four)
              )
  
 # y = subset(y, fips=="24510", select=c(Emissions, year)) 
  plotdata<-ddply(y, c("year"),summarize,sum=sum(Emissions))
  
  png("plot4.png")
  p<-qplot(year, sum, data=plotdata, geom="point", size=I(5), alpha=I(.5), 
           main="Coal-Combustion Related Sources", xlab="Year", ylab="Emissions")
  print(p)
  dev.off()
  

return(y)
  
}




  
  