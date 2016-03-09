plot2 <- function() {

  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  #SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

  #Subset the dataset with only Balitmore entries and Sum the PM data.
  plotdata = subset(NEI, fips=="24510", select=c(Emissions, year))
  plotdata <- aggregate(Emissions ~ year, plotdata, sum)
  
  #Takes care of scientific notation
  options(scipen=5)
 
  #Plot the graph and export to png file.
  png('plot2.png')
  barplot(height=plotdata$Emissions, names.arg=plotdata$year, col="Red",
          xlab="Year", ylab=expression('PM'[2]*' Emission'),
          main=expression('PM'[2]*' Emissions in Balitmore City, Maryland'))
  dev.off()
  
 # return(plotdata)

}