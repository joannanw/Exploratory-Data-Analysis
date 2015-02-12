## Exploratory Data Analysis
## Course Project 2 Plot 5
## By Joanna Widjaja, 11 Feb 2015

library(dplyr)
library(tidyr)
library(ggplot2)

# Load raw data
sccRaw <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
pm25Raw <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Get sum of pm25 by year, Baltimore and on-road
pm25SumMV <- as.tbl(pm25Raw) %>% 
  filter(fips == "24510" & type == "ON-ROAD") %>%
  gather(variable, value, -fips, -SCC, -Pollutant, -type, -year) %>% 
  group_by(year, fips, type, variable) %>% 
  summarise(sum.value = sum(value, na.rm = TRUE))

# Open graphic device and plot graph
png("plot5.png",width = 480, height = 480)
graph <- (ggplot(pm25SumMV, aes(year, sum.value)) 
+ geom_point() 
+ geom_line() 
+ labs(title = "Total PM25 Emissions by Motor Vehicles in Baltimore") 
+ labs(x = "Year") + labs(y = "Sum of PM25 in Tons") 
+ scale_x_continuous(breaks=pm25SumMV$year) 
+ scale_y_continuous(breaks=round(pm25SumMV$sum.value, digits = 2)))
print(graph)
# Close graphic device
dev.off()

