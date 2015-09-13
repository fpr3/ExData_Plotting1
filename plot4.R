# Exploratory Data Analysis
# Course Project 1 (9/13/2015)
#
# Part 4 of 4
#######################################################################################
# 
# Full project description available at: 
#  https://class.coursera.org/exdata-032/human_grading/view/courses/975127/assessments/3/submissions
#
# This project replicates several graphics based on Electric Power Consumption data
# from the UC Irvine Machine Learning Repository.
# 
# The following code provides for downloading and extracting the raw data, loading the
# appropriate data for analysis into R and replicating Plot 4 of this project. The download
# and extract portion includes checks to prevent repeated handling of the large dataset in
# the case that is is already downloaded and/or extracted on the local system.
#
#######################################################################################


## FILE RETRIEVAL AND DATA EXTRACTION
zipfile <- "household_power_consumption.zip"
sourceurl <- paste0("https://d396qusza40orc.cloudfront.net/exdata/data/", zipfile)
datafile <- "household_power_consumption.txt"

# Setup data directory if it is not present
if(!file.exists("data")){dir.create( paste0(getwd(), "/data"))}
try(setwd("./data"), stop("Error: Unable to set working directory."))

# If the datafile is present don't repeat the download and extraction.
if(!file.exists(datafile)){
        # Don't repeat the download if the zip file is found.
        if(!file.exists(zipfile)){
                try(
                        download.file(sourceurl, destfile = paste0("./",zipfile), method = "curl"),
                        stop("Error Unable to download remote zipfile.")
                )
                dateDownloaded <- date()
        }
        # Extract the data from new or existing zipfile
        try(
                unzip(zipfile, files = datafile, list = FALSE, overwrite = TRUE,
                      junkpaths = FALSE, exdir = ".", unzip = "internal",
                      setTimes = FALSE),
                stop("Error: Unable to extract datafile from archive.")
        )
}

## LOAD DATA
electric <- read.table(datafile, colClasses = c(date="character",
                                              time="character", 
                                              global_active_power = "numeric",
                                              global_reactive_power = "numeric",
                                              voltage = "numeric",
                                              global_intensity = "numeric",
                                              sub_metering_1 = "numeric",
                                              sub_metering_2 = "numeric",
                                              sub_metering_3 = "numeric"
                                              ),
                       header=TRUE, sep=";", na.strings=c("?"), stringsAsFactors = FALSE
                     )
setwd("..") # done with data directory

## RETAIN DATA FOR ANALYSIS
selectedDates <- electric[ electric[1]=="1/2/2007" | electric[1]=="2/2/2007",]
rm(electric)

#GENERATE PLOT
# Plot 4 - Multi-plot Chart
# 4 figures arranged in 2 rows and 2 columns
png("plot4.png", width = 480, height = 480, units = "px")
par(mfrow=c(2,2))

# Global Active Power (Top Left)
x <- strptime(paste(selectedDates$Date, selectedDates$Time), "%d/%m/%Y %H:%M:%S")
y <- selectedDates$Global_active_power
xAxisLabel <- ""
yAxisLabel <- "Global Active Power"
plot(x, y, type = "n", xlab = xAxisLabel, ylab = yAxisLabel)
lines(x, y)

# Voltage vs datetime (Top Right)
y <- selectedDates$Voltage
yAxisLabel <- names(selectedDates["Voltage"])
xAxisLabel <- "datetime"
plot(x, y, type = "n", xlab = xAxisLabel, ylab = yAxisLabel)
lines(x, y)

# Energy sub metering w/o legend boxed in (Bottom Left)
y1 <- selectedDates$Sub_metering_1
y2 <- selectedDates$Sub_metering_2
y3 <- selectedDates$Sub_metering_3
xAxisLabel <- ""
yAxisLabel <- "Energy sub metering"
plot(x, y1, type = "n", xlab = xAxisLabel, ylab = yAxisLabel)
lines(x, y1)
lines(x, y2, col = "red")
lines(x, y3, col = "blue")
legend("topright",names(selectedDates[7:9]),lty=c(1,1,1), lwd=c(1,1,1), col=c("black","red","blue"), bty = "n")

# Global_reactive_power vs datetime (Bottom Right)
y <- selectedDates$Global_reactive_power
yAxisLabel <- names(selectedDates["Global_reactive_power"])
xAxisLabel <- "datetime"
plot(x, y, type = "n", xlab = xAxisLabel, ylab = yAxisLabel)
lines(x, y)

dev.off()