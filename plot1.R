require(lubridate)

# read the electricity data
readData <- function(infile="../household_power_consumption.txt",
                     minDate=ymd("2007-02-01"),
                    maxDate=ymd("2007-02-03")) {
  dft <- read.table(infile, sep=";", header=TRUE, 
             colClasses=c("character", "character", "numeric", 
                          "numeric", "numeric", "numeric", 
                          "numeric", "numeric", "numeric"),
             nrows=-1, na.strings="?")
  dft[,"Date"] <- dmy_hms(paste(dft[,"Date"], dft[,"Time"]))
  dft <- subset(dft[,-2], Date>=minDate & Date<maxDate)
  colnames(dft) <- gsub("_", " ", colnames(dft))
  dft
}

# render plot1
plot1 <- function(df) {
  # df = data frame, returned from readData()
  png(file="plot1.png")
  par(mar=c(5,5,2,1))
  hist(df[,"Global active power"],
       col=2, breaks=12,
       main="Global Active Power",
       xlab="Global Active Power (kilowatts)",
       xlim=c(0,6),
       ylim=c(0,1200), xaxt="n")
  axis(1,at=c(0,2,4,6))
  dev.off()
}

# Read data and render plot

dft <- readData()
plot1(dft)
