plot2 <- function() {
  
  library("plyr")
  
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  #SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  myFips <- "24510"
 # merged <- merge(NEI,SCC,by="SCC")

  #Easily tranform ONE column into a factor:
  data<-transform(NEI,year=factor(year))
  
  plotdata1 = subset(data, fips=="24510", select=c(Emissions, year))
  
  #This is a terrific function to add up column numbers:
  plotdata2<-ddply(plotdata1,.(year),summarize,sum=sum(Emissions))
  #Takes care of scientific notation
  options(scipen=5)
  png("plot2.png")
  plot(plotdata2$year,plotdata2$sum,type="h", main="Baltimore City, Maryland", xlab="Year",ylab="Emission Totals", col="Red")
  dev.off()
  
  return(plotdata2)
  
  
}