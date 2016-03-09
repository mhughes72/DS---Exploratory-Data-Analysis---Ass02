plot1 <- function() {
  # Read data files
  NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
  #SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
  
  #Sum the Emissions data and compress data.
  plotdata <- aggregate(Emissions ~ year, NEI, sum)
  
  #Takes care of scientific notation
  options(scipen=5)
  
  #Plot the graph and export to png file.
  png('plot1.png')
  barplot(height=plotdata$Emissions, names.arg=plotdata$year, col="Red",
          xlab="Year", ylab=expression('PM'[2]*' Emission'),
          main=expression('PM'[2]*' Emissions Per Year'))
  dev.off()
  
  #return(plotdata)
}