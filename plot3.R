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

# render plot3
plot3 <- function(df) {
  # df = data frame, returned from readData()
  # Renders plot3
  par(mar=c(5,5,2,1), cex=0.8)
  plot(df[,"Date"], df[,"Sub metering 1"],
       col="black",
       type="l",
       xaxt="n",
       main="",
       xlab="",
       ylab="Energy sub metering")
  lines(df[,"Date"], df[,"Sub metering 2"],
        col="red")
  lines(df[,"Date"], df[,"Sub metering 3"],
        col="blue")
  # Render legend
  ltext <- gsub(" ", "_", names(df)[6:8])
  legend(x="topright", legend=ltext, 
         col=c("black", "red", "blue"), lty=1)
  # Render X-axis
  nr <- nrow(df)
  iax <- seq(1,nr,length.out=3)
  axis(1, at=df[iax,"Date"], 
       labels=wday(df[iax,"Date"], label=TRUE))
  dev.copy(png, file="plot3.png")      
  dev.off()
}


# Read data and render plot

dft <- readData()
plot3(dft)
