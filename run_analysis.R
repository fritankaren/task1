dataexpan <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
dataexpan$Date <- as.Date(dataexpan$Date, "%d/%m/%Y")
dataexpan <- subset(dataexpan,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
dataexpan <- dataexpan[complete.cases(dataexpan),]
dateTime <- paste(dataexpan$Date, dataexpan$Time)
dateTime <- setNames(dateTime, "DateTime")
dataexpan <- dataexpan[ ,!(names(dataexpan) %in% c("Date","Time"))]
dataexpan <- cbind(dateTime, dataexpan)
dataexpan$dateTime <- as.POSIXct(dateTime)

#1
hist(dataexpan$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")

#2
plot(dataexpan$Global_active_power~dataexpan$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

#3
with(dataexpan, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#4
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(dataexpan, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

