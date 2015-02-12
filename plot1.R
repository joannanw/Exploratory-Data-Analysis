## Exploratory Data Analysis
## Course Project 2 Plot 1
## By Joanna Widjaja, 10 Feb 2015

library(dplyr)
library(tidyr)

# Load raw data
sccRaw <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
pm25Raw <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
  
# Get sum of pm25 by year
pm25Sum <- as.tbl(pm25Raw) %>% 
          gather(variable, value, -fips, -SCC, -Pollutant, -type, -year) %>% 
          group_by(year, variable) %>% 
          summarise(sum.value = sum(value, na.rm = TRUE))

# Open graphic device and plot graph
png("plot1.png",width = 480, height = 480)

plot(pm25Sum$year, pm25Sum$sum.value/1000, xaxt = 'n', xlab = "Year", 
     ylab = "Sum of PM25 in Kilotons", main = "Total Emissions of PM25",
     pch = 19)
lines(pm25Sum$year, pm25Sum$sum.value/1000)
axis(1,xaxp = c(min(pm25Sum$year), max(pm25Sum$year),length(pm25Sum$year)-1))

# Alternatively plot a bar plot
# barplot(names.arg = pm25Sum$year, pm25Sum$sum.value/1000, xlab = "Year", 
#         ylab = expression(paste('PM', ''[2.5], ' in Kilotons')), main = "Total Emissions of PM"[2.5])

# Close graphic device
dev.off()