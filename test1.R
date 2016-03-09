test1 <- function() {
# Read data files
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Have total emissions from PM2.5 decreased in the United States from 1999 to
# 2008?
total.emissions <- aggregate(Emissions ~ year, NEI, sum)

#png('plot1.png')
barplot(height=total.emissions$Emissions, names.arg=total.emissions$year,
        xlab="years", ylab=expression('total PM'[2]*' emission'),
        main=expression('Total PM'[2]*' emissions at various years'))
#dev.off()

return(total.emissions)

}