plot5 <- function() {
  
  library ("dplyr")
  library ("ggplot2")
  
  #Read data
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  #Merge 2 datasets
  merged <- merge(NEI, SCC, by="SCC")
  
  #Get all data related to Vehicle emissions
  filtered<-filter(merged, grepl('[Vv]ehicles', merged$Short.Name)|
              grepl('[Vv]ehicles', merged$EI.Sector)|
              grepl('[Vv]ehicles', merged$SCC.Level.One)|
              grepl('[Vv]ehicles', merged$SCC.Level.Two)|
              grepl('[Vv]ehicles', merged$SCC.Level.Three)|
              grepl('[Vv]ehicles', merged$SCC.Level.Four)
  )
  
  #Get subset with only Baltimore emmisions
  subset = subset(filtered, fips=="24510", select=c(Emissions, year)) 
  
  #Create data to plot graph
  plotdata <- aggregate(Emissions ~ year, data=subset, FUN=sum)
  
  #Plot graph
  png("plot5.png")
  p<-ggplot(plotdata, aes(x=factor(year), y=Emissions)) +
    geom_bar(stat="identity") +
    xlab("Year") +
    ylab(expression("PM"[2.5]*" Emissions")) +
    ggtitle("Vehicle Emissions in Baltimore City")
  print(p)
  dev.off()
  
}

