plot3 <- function() {
  
  library("plyr")
  library ("ggplot2")
  
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  #SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
 
  NEI<-transform(NEI,type=factor(type),year=factor(year))
  
  NEI = subset(NEI, fips=="24510", select=c(Emissions, year, type)) 
  plotdata<-ddply(NEI, c("year", "type"),summarize,sum=sum(Emissions))
  #NEIsub<-head(NEI, n=1000000)
png("plot3.png")
p<-qplot(year, sum, data=plotdata, geom="point", color=type, size=I(5), alpha=I(.5), 
       main="Baltimore City, Maryland", xlab="Type of Source", ylab="Emissions")
print(p)
 dev.off()
 
  #return(p)
  
}