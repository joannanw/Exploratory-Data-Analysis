## Exploratory Data Analysis
## Course Project 2 Plot 5
## By Joanna Widjaja, 11 Feb 2015

library(dplyr)
library(tidyr)
library(ggplot2)

# Load raw data
sccRaw <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
pm25Raw <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Get sum of pm25 by year, Baltimore/LA and on-road
pm25SumMV <- as.tbl(pm25Raw) %>% 
  filter((fips == "24510" | fips == "06037") & type == "ON-ROAD") %>%
  gather(variable, value, -fips, -SCC, -Pollutant, -type, -year) %>% 
  group_by(fips, year, type, variable) %>% 
  summarise(sum.value = sum(value, na.rm = TRUE))
pm25SumMV$fips <- c(rep("Los Angeles County", 4),rep("Baltimore City", 4))

# Open graphic device and plot graph
png("plot6.png",width = 480, height = 480)
graph <- (ggplot(pm25SumMV, aes(year, sum.value, fips)) 
          + geom_point()
          + geom_line() 
          + facet_grid(. ~ fips)
          + labs(title = "Total PM25 Emissions by Motor Vehicles") 
          + labs(x = "Year") + labs(y = "Sum of PM25 in Tons") 
          + scale_x_continuous(breaks=pm25SumMV$year) 
          + geom_text(aes(label = round(sum.value,0)), size = 3, hjust = 0.5, vjust = -0.5))
print(graph)
# Close graphic device
dev.off()