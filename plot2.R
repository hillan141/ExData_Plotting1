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
  dft <- subset(dft[,-2], Date>=minDate & Date<=maxDate)
  colnames(dft) <- gsub("_", " ", colnames(dft))
  dft
}

# render plot2
plot2 <- function(df) {
  # df = data frame, returned from readData()
  # Renders plot2
  par(mar=c(5,5,2,1))
  plot(df[,"Date"], df[,"Global active power"],
       type="l",
       xaxt="n",
       main="",
       xlab="",
       ylab="Global Active Power (kilowatts)")
  nr <- nrow(df)
  iax <- seq(1,nr,length.out=3)
  axis(1, at=df[iax,"Date"], 
       labels=wday(df[iax,"Date"], label=TRUE))
  dev.copy(png, file="plot2.png")      
  dev.off()
}

# Read data and render plot

dft <- readData()
plot2(dft)
