library(tidyr)
suppressMessages(library(dplyr))
library(ggplot2)
library(scales)
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
# cleaning data
# change the varibles to proper mode
tbl <- tbl %>% 
        mutate(timeSeries = paste(Date, Time)) %>% 
        mutate(timeSeries = as.POSIXct(timeSeries, format="%d/%m/%Y %H:%M:%S")) %>%
        select(-Date, -Time)
tbl[-8] <- lapply(tbl[-8], as.numeric)
tbl <- na.omit(tbl)
# the data is ready to plot!


png("plot2.png", width=480, height=480)
xbreaks=unique(format(tbl$timeSeries,"%Y%m%d"))
xlabs=unique(format(tbl$timeSeries,"%a"))
p2 <- ggplot(data=tbl, aes(x=timeSeries, y=Global_active_power))
p2 <- p2 + geom_line()
p2 <- p2 + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a"))
p2 <- p2 + xlab("") + ylab("Global Active Power (kilowatts)")
p2
dev.off()
