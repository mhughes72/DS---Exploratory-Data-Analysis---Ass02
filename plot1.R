plot1 <- function() {
  nrows=2000
  library("plyr")
  
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  #SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

  #Easily tranform ONE column into a factor:
  data<-transform(NEI,year=factor(year))
  
  #This is a terrific function to add up column numbers:
  plotdata<-ddply(data,.(year),summarize,sum=sum(Emissions))
  #Takes care of scientific notation
  options(scipen=5)
  png("plot1.png")
  plot(plotdata$year,plotdata$sum,type="h",xlab="Year",ylab="Emission Totals", col="Red")
  dev.off()
  
  return(plotdata)
  
}