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
# tidying the data
tbl <- tbl %>% 
        mutate(timeSeries = paste(Date, Time)) %>% 
        mutate(timeSeries = as.POSIXct(timeSeries, format="%d/%m/%Y %H:%M:%S")) %>%
        select(-Date, -Time)
tbl[-8] <- lapply(tbl[-8], as.numeric)
tbl <- na.omit(tbl)
tbl <- tbl %>% gather(subMeter, metering, Sub_metering_1:Sub_metering_3)
# the data is ready to plot!


png("plot3.png", width=480, height=480)
p3 <- ggplot(data=tbl, aes(x=timeSeries, y=metering, color=subMeter))
p3 <- p3 + geom_line()
p3 <- p3 + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a"))
p3 <- p3 + xlab("") + ylab("Energy sub metering")
p3 <- p3 + theme(legend.position=c(1,1), legend.justification=c(1,1)) 
p3 <- p3 + guides(fill=guide_legend(title=NULL))
p3 <- p3 + scale_colour_manual(values=c("black", "red", "blue"))
p3
dev.off()
