plot1 <- function(datafile = "hpc.txt"){

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

        png(file = "plot1.png")

        hist(hpc2$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")

        dev.off()
        
}

