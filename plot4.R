plot4 <- function() {
  
  library ("ggplot2")
  
  #Read data
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  #Merge the 2 dataframes
  merged <- merge(NEI, SCC, by="SCC")
  
  #Filter out data based on use of Coal
  merged<-filter(merged, grepl('[Cc]oal', merged$Short.Name)|
              grepl('[Cc]oal', merged$EI.Sector)|
              grepl('[Cc]oal', merged$SCC.Level.One)|
              grepl('[Cc]oal', merged$SCC.Level.Two)|
              grepl('[Cc]oal', merged$SCC.Level.Three)|
              grepl('[Cc]oal', merged$SCC.Level.Four)
  )
  
  #Create aggregate of data and sum up the emissions.
  plotdata <- aggregate(Emissions ~ year, data=merged, FUN=sum)
  
  #Build graph.
  #png("plot4.png")
  p<-ggplot(plotdata, aes(x=factor(year), y=Emissions)) +
    geom_bar(stat="identity") +
    xlab("year") +
    ylab(expression("PM"[2.5]*" Emissions")) +
    ggtitle("Emissions from Coal Related Sources")
  print(p)
  #dev.off()
  
}





