plot1 <-
function(){
        #####Get ready#####
        link_url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
        target_file <- 'household_power_consumption.txt'
        target_zip <-"exdata_data_household_power_consumption.zip"
        
        ##### Is there a target file?#####
        if(!file.exists(target_file)){
                #####Is there at leat zip file?#####
                if(!file.exists(target_zip)){
                        #####if not, download zip file#####
                        download.file(link_url,destfile = target_zip)
                        
                }
                #####unzip file#####
                unzip(target_zip)
        }
        #open target file
        df <- read.table(target_file,header=T,sep=";")
        #convert Date variable to Date class
        df$Date <- as.Date(df$Date,format = "%d/%m/%Y")
        #choose the date we are going to invasgate
        df <- subset(df,Date=="2007-02-01"|Date=="2007-02-02")
        #convert Time variable to Date class
        df$Time <- paste(df$Date,df$Time)
        df$Time <- as.character(df$Time)
        df$Time <- strptime(df$Time,format = "%Y-%m-%d %H:%M:%S")
        
        #convert other variables to numeric class
        var.numeric <-  colnames(df)[-c(1,2)]
        df[,var.numeric] <- lapply(df[,var.numeric],as.character)
        df[,var.numeric] <- lapply(df[,var.numeric],as.numeric)
        
        #plot & save
        hist(df$Global_active_power ,main="Global_active_power",
             xlab="Global_active_power(kilowatts)",
             col="Red",breaks = 12)
        dev.copy(png,file="plot1.png",width=480,height=480)
        dev.off()
}
