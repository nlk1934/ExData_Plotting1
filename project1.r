rm(list=ls())
library(tidyr)
suppressMessages(library(dplyr))
library(ggplot2)
## Ut's true the demo graphics were created by R base plot
## However, ggplot2 will be used in this homework for coninstensity

# reading the file
# it may not be efficent to load the whole data all in once
# however, a 50M file is not so big for a morden computer
con <- file("household_power_consumption.txt","rt")
classes <- c("character","character","character","character","character","character","character","character","character")
tblall <- read.table(con, header = TRUE, stringsAsFactor=FALSE,sep=";", colClasses = classes)
close(con)
tbl <- tbl_df(
        tbl <- tblall %>% 
        filter(Date=="1/2/2007"|Date=="2/2/2007")
        )
rm(tblall)

##cleaning data
# change the varibles to proper mode
timeSeries = as.data.frame( strptime(paste(tbl$Date, tbl$Time, sep = ":"), format="%d/%m/%Y:%H:%M:%S"))
colnames(timeSeries)="timeSeries"
tbl <- cbind(tbl, timeSeries)
tbl <- tbl_df(select(tbl, -Date, -Time))
#sorry, I can't finish the above steps with the elegent dplyr pipe command
        
#turn data into number except for the last column "timeSeries"
#remove NA values if there are any
tbl[-8] <- lapply(tbl[-8], as.numeric)
tbl <- na.omit(tbl)
# the data is ready to plot!

png("plot1.png", width=480, height=480)
binsize <- diff(range(tbl$Global_active_power))/15
p <- ggplot(data=tbl, aes(x=Global_active_power))
p <- p + geom_histogram(fill="brown1", color="grey", binwidth=binsize)
p <- p + xlab("Global Active Power (kilowatts)") + ylab("Frequency")
p
dev.off()