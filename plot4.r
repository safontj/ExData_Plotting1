plot4 <- function(datafile = "hpc.txt"){
        
# LOAD DATA
        
# datafile input parameter is the original file with the data
        
# If a file for the required dates already exists with name hpc2.txt takes that file        
        if(file.exists("hpc2.txt")){
                hpc2 <- read.table("hpc2.txt", header = TRUE, sep=";", na.strings="?")
                #converts date and time to Date and POSIXct formats
                hpc2[,1] <- as.Date(hpc2[,1],"%Y-%m-%d")
                hpc2[,2] <- as.POSIXct(hpc2[,2], format="%Y-%m-%d %H:%M:%S")                
        }
        
# Converts formats for date and time and creates a file hpc2.txt with the observations for the specified dates        
        else{
                hpc <- read.table(datafile, header = TRUE, sep=";", na.strings="?")
                hpc[,2] <- paste(hpc[,1],hpc[,2])
                hpc[,1] <- as.Date(hpc[,1],"%d/%m/%Y")
                hpc[,2] <- as.POSIXct(hpc[,2], format="%d/%m/%Y %H:%M:%S")
                hpc2 <- subset(hpc,Date >= as.Date("2007-02-01") & Date < as.Date("2007-02-03"))
                write.table(hpc2,"hpc2.txt", sep=";", row.names= FALSE)                
                
        }
        
# PLOT CHART

        png(file = "plot4.png")

        par(mfrow = c(2,2))
        with (hpc2,{
                plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
         
                plot(Time, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
         
                plot(Time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
                points(Time,Sub_metering_2, type = "l", col = "red")
                points(Time,Sub_metering_3, type = "l", col = "blue")
                legend("topright", lty = c(1,1,1), col=c("black","red","blue"), bty="n", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
         
                plot(Time, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
                 
        })

        dev.off()
        
}

