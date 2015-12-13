#### LOAD INTRADAY DATA ####

# Download the intraday file

date.update <-Sys.Date()
prn.loc <- "./data/Stooq/"
date.update.file <- paste0(prn.loc, date.update, "-dh5.prn")
update.url <- paste0("http://stooq.com/db/d/?d=",gsub("-","",date.update),"&t=dh5&u=14835821")

download.file(update.url, date.update.file)

update <- read.delim(date.update.file, header=FALSE, sep=',', stringsAsFactors = FALSE, colClasses=c(V4="character"))
colnames(update) <- c("Ticker","Period","Date1","Time","Open","High","Low","Close","Vol","OI")
update$Ticker <- gsub("^", "", update$Ticker, fixed = TRUE)

update.5m <- subset(update, update[, 2] == "5")
update.hourly <- subset(update, update[, 2] == "60")
update.daily <- subset(update, update[, 2] == "D")

data.FiveMins.update <- list()
data.Hourly.update <- list()
data.Daily.update <- list()

na.FiveMins <- logical()
na.Hourly <- logical()
na.Daily <- logical()

# Load intraday data in a list of data frames from update file

for (i in 1:nrow(data.loc)) {
        list.name <- data.loc$Ticker[i]
        
        data.FiveMins.update[[list.name]] <- subset(update.5m, update.5m[ ,1]==data.loc$Ticker[i])
        data.Hourly.update[[list.name]] <- subset(update.hourly, update.hourly[ ,1]==data.loc$Ticker[i])
        data.Daily.update[[list.name]] <- subset(update.daily, update.daily[ ,1]==data.loc$Ticker[i])
        
        na.FiveMins <- c(na.FiveMins,is.na(data.FiveMins.update[[i]][1,"Close"]))
        na.Hourly <- c(na.Hourly,is.na(data.Hourly.update[[i]][1,"Close"]))
        na.Daily <- c(na.Daily,is.na(data.Daily.update[[i]][1,"Close"]))
}

# Check for missing data

print("Check for missing data (M05 / H1 / D):")
print(data.loc$Ticker[na.FiveMins])
print(data.loc$Ticker[na.Hourly])
print(data.loc$Ticker[na.Daily])

# Modify the data frames from above list and create a list of xts

data.FiveMins.update.xts <- list()
data.Hourly.update.xts <- list()
data.Daily.update.xts <- list()

for (i in 1:nrow(data.loc)) {
        list.name <- data.loc$Ticker[i]
        
        data.FiveMins.update[[i]]$Date <- strptime(paste(data.FiveMins.update[[i]][,3], data.FiveMins.update[[i]][,4]), format='%Y%m%d %H%M%S', tz="CET")
        data.FiveMins.update[[i]]$Ticker <- NULL
        data.FiveMins.update[[i]]$Period <- NULL
        data.FiveMins.update[[i]]$Date1 <- NULL
        data.FiveMins.update[[i]]$Time <- NULL
        data.FiveMins.update[[i]]$Vol <- NULL
        data.FiveMins.update[[i]]$OI <- NULL
        data.FiveMins.update[[i]] <- data.FiveMins.update[[i]][c(5,1,2,3,4)]
        data.FiveMins.update.xts[[list.name]] <- xts(data.FiveMins.update[[i]][,-1], order.by = data.FiveMins.update[[i]][,1])
        indexTZ(data.FiveMins.update.xts[[list.name]]) <-"CET"
        
        data.Hourly.update[[i]]$Date <- strptime(paste(data.Hourly.update[[i]][,3], data.Hourly.update[[i]][,4]), format='%Y%m%d %H%M%S', tz="CET")
        data.Hourly.update[[i]]$Ticker <- NULL
        data.Hourly.update[[i]]$Period <- NULL
        data.Hourly.update[[i]]$Date1 <- NULL
        data.Hourly.update[[i]]$Time <- NULL
        data.Hourly.update[[i]]$Vol <- NULL
        data.Hourly.update[[i]]$OI <- NULL
        data.Hourly.update[[i]] <- data.Hourly.update[[i]][c(5,1,2,3,4)]
        data.Hourly.update.xts[[list.name]] <- xts(data.Hourly.update[[i]][,-1], order.by = data.Hourly.update[[i]][,1])
        indexTZ(data.Hourly.update.xts[[list.name]]) <-"CET"
        
        data.Daily.update[[i]]$Date <- strptime(paste(data.Daily.update[[i]][,3], data.Daily.update[[i]][,4]), format='%Y%m%d %H%M%S', tz="CET")
        data.Daily.update[[i]]$Ticker <- NULL
        data.Daily.update[[i]]$Period <- NULL
        data.Daily.update[[i]]$Date1 <- NULL
        data.Daily.update[[i]]$Time <- NULL
        data.Daily.update[[i]]$Vol <- NULL
        data.Daily.update[[i]]$OI <- NULL
        data.Daily.update[[i]] <- data.Daily.update[[i]][c(5,1,2,3,4)]
        data.Daily.update.xts[[list.name]] <- xts(data.Daily.update[[i]][,-1], order.by = data.Daily.update[[i]][,1])
        indexTZ(data.Daily.update.xts[[list.name]]) <-"CET"
}

#### UPDATE THE HISTORICAL DATA WITH INTRADAY DATA ####

data.M05.xts <- list()
data.M15.xts <- list()
data.H1.xts <- list()
data.H4.xts <- list()
data.D.xts <- list()
data.W.xts <- list()

suppressWarnings(

for (i in 1:nrow(data.loc)) {
        list.name <- data.loc$Ticker[i]
        data.M05.xts[[list.name]]<-rbind(data.FiveMins.xts[[list.name]],data.FiveMins.update.xts[[list.name]])
        
        data.M15.xts[[list.name]] <- to.minutes15(data.M05.xts[[i]])
        colnames(data.M15.xts[[list.name]]) <- colnames(data.M05.xts[[i]])
        
        data.H1.xts[[list.name]]<-rbind(data.Hourly.xts[[list.name]],data.Hourly.update.xts[[list.name]])
        
        data.H4.xts[[list.name]] <- to.period(data.H1.xts[[i]], period = "hours", k=4)
        colnames(data.H4.xts[[list.name]]) <- colnames(data.H1.xts[[i]])
        
        data.D.xts[[list.name]]<-rbind(data.Daily.xts[[list.name]], last(data.Daily.update.xts[[list.name]]))
        
        data.W.xts[[list.name]] <- to.weekly(data.D.xts[[i]])
        colnames(data.W.xts[[list.name]]) <- colnames(data.D.xts[[i]])
}

)

tf.lst<- list(listM05=data.M05.xts,listM15=data.M15.xts,listH1=data.H1.xts,
              listH4=data.H4.xts,listD=data.D.xts,listW=data.W.xts)

# Check if all time frames closes and time stamps are the same

for (i in 1:nrow(data.loc)) {
        if (all(as.numeric(lapply(seq_along(tf.lst),function(x) last(tf.lst[[x]][[i]]$Close)))==as.numeric(lapply(seq_along(tf.lst),function(x) last(tf.lst[[x]][[i]]$Close)))[1]) == FALSE) {
                print(paste0("Issues in time frame closes: ",data.loc$Ticker[i]))
                print(as.numeric(lapply(seq_along(tf.lst),function(x) last(tf.lst[[x]][[i]]$Close))))
        }
}

# for (i in 1:nrow(data.loc)) {
#         if (all.equal(last(data.M05.xts[[i]]$Close),last(data.M15.xts[[i]]$Close), last(data.H1.xts[[i]]$Close),
#                   last(data.H4.xts[[i]]$Close), last(data.D.xts[[i]]$Close), last(data.W.xts[[i]]$Close))==FALSE) {
#                         print(paste0("Issues in time frame closes:",data.loc$Ticker[i]))
#                 }
# }

for (i in 1:nrow(data.loc)) {
        if (all.equal(last(index(data.M05.xts[[i]]$Close)),last(index(data.M15.xts[[i]]$Close)), last(index(data.H1.xts[[i]]$Close)),
                last(index(data.H4.xts[[i]]$Close)), last(index(data.D.xts[[i]]$Close)), last(index(data.W.xts[[i]]$Close))) == FALSE) {
                        print(paste0("Issues in time frame time stamp:",data.loc$Ticker[i]))
                        
                }
}
