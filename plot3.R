plot3 <- function() {
  
  library ("ggplot2")
  
  #Read data
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  #SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  #Get subset of data then aggragate data and sum emissions.
  NEI = subset(NEI, fips=="24510", select=c(Emissions, year, type)) 
  plotdata <- aggregate(Emissions ~ year + type, data=NEI, FUN=sum)
  
  #Build the graph  
  png("plot3.png")
  p<-ggplot(plotdata, aes(x=factor(year), y=Emissions, fill=type)) +
    geom_bar(stat="identity") +
    facet_grid(. ~ type) +
    xlab("Year") +
    ylab(expression("PM"[2.5]*" Emission")) +
    ggtitle(expression("PM"[2.5]*paste(" Emissions in Baltimore ",
                                       "City", sep="")))
  print(p)
  dev.off()
  
  #return(plotdata)
  
}