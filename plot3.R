## Exploratory Data Analysis
## Course Project 2 Plot 3
## By Joanna Widjaja, 10 Feb 2015

library(dplyr)
library(tidyr)
library(ggplot2)

# Load raw data
sccRaw <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
pm25Raw <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Get sum of pm25 by year, fips = 24510, and type
pm25SumBaltType <- as.tbl(pm25Raw) %>% 
              filter(pm25Raw$fips == "24510") %>%
              gather(variable, value, -fips, -SCC, -Pollutant, -type, -year) %>% 
              group_by(year, fips, type, variable) %>% 
              summarise(sum.value = sum(value, na.rm = TRUE))

# Open graphic device and plot graph
png("plot3.png",width = 480, height = 480)
graph <- ggplot(pm25SumBaltType, aes(year, sum.value, type)) + 
  geom_point(aes(color = type)) + 
  geom_line(aes(color = type)) + 
  labs(title = "Total Emissions in Baltimore by Type") + 
  labs(x = "Year") + labs(y = "Sum of PM25")
print(graph)

# Close graphic device
dev.off()
