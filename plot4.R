## Exploratory Data Analysis
## Course Project 2 Plot 5
## By Joanna Widjaja, 11 Feb 2015

library(dplyr)
library(tidyr)
library(ggplot2)

# Load raw data
sccRaw <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")
pm25Raw <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")

# Get list of SCC with "Comb" and "Coal"
sccCoal <- sccRaw[grepl("Coal", sccRaw$Short.Name) & grepl("Comb", sccRaw$SCC.Level.One),1]

# Get sum of pm25 by year and sccCoal
pm25SumCoal <- as.tbl(pm25Raw) %>% 
  filter(SCC %in% sccCoal) %>%
  gather(variable, value, -fips, -SCC, -Pollutant, -type, -year) %>% 
  group_by(year, variable) %>% 
  summarise(sum.value = sum(value, na.rm = TRUE))

# Open graphic device and plot graph
png("plot4.png",width = 480, height = 480)
graph <- (ggplot(pm25SumCoal, aes(year, sum.value/1000)) 
+ geom_point() 
+ geom_line() 
+ labs(title = "Total PM25 Emissions Caused by Coal") 
+ labs(x = "Year") 
+ labs(y = "Sum of PM25 in Kilotons") 
+ scale_x_continuous(breaks=pm25SumCoal$year))
print(graph)

# Close graphic device
dev.off()

