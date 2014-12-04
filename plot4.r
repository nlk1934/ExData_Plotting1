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


png("plot4.png", width=480, height=480)

p2 <- ggplot(data=tbl, aes(x=timeSeries, y=Global_active_power))
p2 <- p2 + geom_line()
p2 <- p2 + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a"))
p2 <- p2 + xlab("") + ylab("Global Active Power (kilowatts)")
p2


p3 <- ggplot(data=tbl, aes(x=timeSeries, y=metering, color=subMeter))
p3 <- p3 + geom_line()
p3 <- p3 + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a"))
p3 <- p3 + xlab("") + ylab("Energy sub metering")
p3 <- p3 + theme(legend.position=c(1,1), legend.justification=c(1,1)) 
p3 <- p3 + guides(fill=guide_legend(title=NULL))
p3 <- p3 + scale_colour_manual(values=c("black", "red", "blue"))
p3

p4 <- ggplot(data=tbl, aes(x=timeSeries, y=Voltage ))
p4 <- p4 + geom_line()
p4 <- p4 + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a"))
p4 <- p4 + xlab("dateline") + ylab("Voltage")
p4

p5 <- ggplot(data=tbl, aes(x=timeSeries, y=Global_reactive_power ))
p5 <- p5 + geom_line()
p5 <- p5 + scale_x_datetime(breaks = date_breaks("1 day"), labels = date_format("%a"))
p5 <- p5 + xlab("dateline") + ylab("Global_reactive_power")
p5
multiplot(p2,p3,p4,p5,cols=2)

dev.off()


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
