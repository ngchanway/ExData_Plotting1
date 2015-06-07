## The overall goal is simply to examine how household energy usage varies over
## a 2-day period in February, 2007.

## This function will be using the "Individual household electric power
## consumption Data Set" from the UC Irvine Machine Learning Repository.

## This function construct 4 plots using the base plotting system.

plot4 <- function() {
        fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        zipfile <- "exdata-data-household_power_consumption.zip"
        filename <- "household_power_consumption.txt"
        
        ## Check the existence of the dataset
        ## If the dataset is not exist but is downloaded in a zipfile,
        ## unzip the dataset
        ## If the dataset is not exist and the zipfile is not downloaded,
        ## download the dataset and unzip the dataset from the zipfile
        if(!file.exists(filename)) {
                if(!file.exists(zipfile)) {
                        download.file(fileurl, zipfile, method = "curl")                        
                }
                unzip(zipfile)
                unlink(zipfile)
        }
        
        ## Load data from the dates 2007-02-01 and 2007-02-02
        ## and construct a list of the class "POSIXlt"
        start <- grep("1/2/2007", readLines(filename))[1]
        end <- grep("3/2/2007", readLines(filename))[1]
        data <- read.table(filename, sep = ";", na.strings = "?",
                           skip = start - 1, nrows = end - start)
        date_time <- paste(data[, 1], data[, 2])
        completeTime <- strptime(date_time, format = "%d/%m/%Y %H:%M:%S")
        
        ## Construct the plot of Energy sub metering over time
        ## and save it to a PNG file
        ## with a width of 480 pixels and a height of 480 pixels (default)
        png("plot4.png")
        par(mfcol = c(2, 2))
        
        ## Construct the plot of Global Active Power over time
        plot(completeTime, as.numeric(data[, 3]), type = "l",
             ylab = "Global Active Power (kilowatts)", xlab = "")
        
        ## Construct the plot of Energy sub metering over time
        plot(completeTime, as.numeric(data[, 7]), type = "l",
             ylab = "Energy sub metering", xlab = "")
        lines(completeTime, as.numeric(data[, 8]), col = "red")
        lines(completeTime, as.numeric(data[, 9]), col = "blue")
        legend("topright", lwd = 1, col = c("black", "red", "blue"),
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
               bty = "n")
        
        ## Construct the plot of minute-averaged voltage (in volt) over time
        plot(completeTime, as.numeric(data[, 5]), type = "l",
             ylab = "Voltage", xlab = "datetime")
        
        ## Construct the plot of household global minute-averaged reactive power
        plot(completeTime, as.numeric(data[, 4]), type = "l",
             ylab = "Global_reactive_power", xlab = "datetime")
        
        dev.off()
}