## Exploratory Data Analysis
## Course Project 2 Plot 2
## By Joanna Widjaja, 10 Feb 2015

library(dplyr)
library(tidyr)

# Load raw data
sccRaw <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
pm25Raw <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Get sum of pm25 by year and fips = 24510
pm25SumBalt <- as.tbl(pm25Raw) %>% 
          filter(pm25Raw$fips == "24510") %>%
          gather(variable, value, -fips, -SCC, -Pollutant, -type, -year) %>% 
          group_by(year, fips, variable) %>% 
          summarise(sum.value = sum(value, na.rm = TRUE))

# Open graphic device and plot graph
png("plot2.png",width = 480, height = 480)
plot(pm25SumBalt$year, pm25SumBalt$sum.value, xaxt = 'n', xlab = "Year", 
     ylab = "Sum of PM25 in Tons", main = "Total Emissions of PM25 in Baltimore",
     pch = 19)
lines(pm25SumBalt$year, pm25SumBalt$sum.value)
axis(1,xaxp = c(min(pm25SumBalt$year), max(pm25SumBalt$year),length(pm25SumBalt$year)-1))

# Close graphic device
dev.off()