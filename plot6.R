plot6 <- function() {
  
  library("plyr")
  library("dplyr")
  library ("ggplot2")
  
  #Get data
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  #Merge the 2 data sets
  merged <- merge(NEI, SCC, by="SCC")
  
  #Filter new dataset based on the term 'Vehicles'
  y<-filter(merged, grepl('[Vv]ehicles', merged$Short.Name)|
              grepl('[Vv]ehicles', merged$EI.Sector)|
              grepl('[Vv]ehicles', merged$SCC.Level.One)|
              grepl('[Vv]ehicles', merged$SCC.Level.Two)|
              grepl('[Vv]ehicles', merged$SCC.Level.Three)|
              grepl('[Vv]ehicles', merged$SCC.Level.Four)
  )
  
  #Filter for the appropriate County (fips)
  la = subset(y, fips=="24510", select=c(Emissions, year, fips)) 
  baltimore = subset(y, fips=="06037", select=c(Emissions, year, fips)) 
  
  #Aggragate 2 data sets
  la <- aggregate(Emissions ~ year, data=la, FUN=sum)
  baltimore <- aggregate(Emissions ~ year, data=baltimore, FUN=sum)
  
  #Give the fips appropriate County names
  la$County <- "Los Angeles County, CA"
  baltimore$County <- "Baltimore City, MD"
  
  #Merge 2 Countyy datasets
  merged <- rbind(la, baltimore)
  
  #Draw Graph
 png("plot6.png")
  p<-ggplot(merged, aes(x=factor(year), y=Emissions, fill=County)) +
    geom_bar(stat="identity") + 
    facet_grid(County  ~ ., scales="free") +
    ylab("Emissions") + 
    xlab("Year") +
    ggtitle(expression("Motor vehicle Emissions in Baltimore and Los Angeles"))
  print(p)
  dev.off()
  
  #return(y)
  
}

