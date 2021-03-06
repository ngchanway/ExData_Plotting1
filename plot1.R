## The overall goal is simply to examine how household energy usage varies over
## a 2-day period in February, 2007.

## This function will be using the "Individual household electric power
## consumption Data Set" from the UC Irvine Machine Learning Repository.

## This function plots the histogram of household global minute-averaged active
## power using the base plotting system.

plot1 <- function() {
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
        start <- grep("1/2/2007", readLines(filename))[1]
        end <- grep("3/2/2007", readLines(filename))[1]
        data <- read.table(filename, sep = ";", na.strings = "?",
                           skip = start - 1, nrows = end - start)
        data <- data[complete.cases(data), ]
        
        ## Construct the histogram of Global Active Power
        ## and save it to a PNG file
        ## with a width of 480 pixels and a height of 480 pixels (default)
        png("plot1.png", bg = "transparent")
        hist(data[, 3], col = "red", main = "Global Active Power",
             xlab = "Global Active Power (kilowatts)")
        dev.off()
}