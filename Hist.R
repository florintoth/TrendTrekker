# DOWNLOAD HISTORICAL DATA

zip.loc <- "./data/Stooq/"

# Get database location in zip files ####

zip.loc.5m <- paste0(zip.loc,"5min_hist.zip")
zip.loc.hourly <- paste0(zip.loc,"60min_hist.zip")
zip.loc.daily <- paste0(zip.loc, "daily_hist.zip")

loc.5m <- "data/5 min/world/"
currency.major.loc.5m <- "currencies\\major/"
currency.other.loc.5m <- "currencies\\other/"
commodities.loc.5m <- "commodities/"
indices.loc.5m <- "indices/"

loc.hourly <- "data/hourly/world/"
currency.major.loc.hourly <- "currencies\\major/"
currency.other.loc.hourly <- "currencies\\other/"
commodities.loc.hourly <- "commodities/"
indices.loc.hourly <- "indices/"

loc.daily <- "data/daily/world/"
currency.major.loc.daily <- "currencies\\major/"
currency.other.loc.daily <- "currencies\\other/"
commodities.loc.daily <- "commodities/"
indices.loc.daily <- "indices/"

ticker.currency.major <- read.csv("./data/Stooq/TickerList/R_Stooq/R_currmajor.csv", header=F, stringsAsFactors = F)
ticker.currency.major <- as.vector(ticker.currency.major$V1)

ticker.currency.other <- read.csv("./data/Stooq/TickerList/R_Stooq/R_currother.csv", header=F, stringsAsFactors = F)
ticker.currency.other <- as.vector(ticker.currency.other$V1)

ticker.commodities <- read.csv("./data/Stooq/TickerList/R_Stooq/R_commodities.csv", header=F, stringsAsFactors = F)
ticker.commodities <- as.vector(ticker.commodities$V1)

ticker.indices <- read.csv("./data/Stooq/TickerList/R_Stooq/R_indices.csv", header=F, stringsAsFactors = F)
ticker.indices <- as.vector(ticker.indices$V1)

ticker.not.intraday <- c(ticker.currency.other,"RB.F")

# Create a data frame with ticker paths ####

data.loc <- data.frame(Ticker=character(),
                       FiveMins=character(),
                       Hourly=character(),
                       Daily=character(),
                       stringsAsFactors = FALSE)

pos <- nrow(data.loc)
for (i in 1:length(ticker.currency.major)) {
        data.loc [pos+i, 1] <- ticker.currency.major[i]
        data.loc [pos+i, 2] <- paste0(loc.5m, currency.major.loc.5m, ticker.currency.major[i],".txt")
        data.loc [pos+i, 3] <- paste0(loc.hourly, currency.major.loc.hourly, ticker.currency.major[i],".txt")
        data.loc [pos+i, 4] <- paste0(loc.daily, currency.major.loc.daily, ticker.currency.major[i],".txt")
}

pos <- nrow(data.loc)
for (i in 1:length(ticker.currency.other)) {
        data.loc [pos+i, 1] <- ticker.currency.other[i]
        data.loc [pos+i, 2] <- paste0(loc.5m, currency.other.loc.5m, ticker.currency.other[i],".txt")
        data.loc [pos+i, 3] <- paste0(loc.hourly, currency.other.loc.hourly, ticker.currency.other[i],".txt")
        data.loc [pos+i, 4] <- paste0(loc.daily, currency.other.loc.daily, ticker.currency.other[i],".txt")
}

pos <- nrow(data.loc)
for (i in 1:length(ticker.commodities)) {
        data.loc [pos+i, 1] <- ticker.commodities[i]
        data.loc [pos+i, 2] <- paste0(loc.5m, commodities.loc.5m, ticker.commodities[i],".txt")
        data.loc [pos+i, 3] <- paste0(loc.hourly, commodities.loc.hourly, ticker.commodities[i],".txt")
        data.loc [pos+i, 4] <- paste0(loc.daily, commodities.loc.daily, ticker.commodities[i],".txt")
}

pos <- nrow(data.loc)
for (i in 1:length(ticker.indices)) {
        data.loc [pos+i, 1] <- ticker.indices[i]
        data.loc [pos+i, 2] <- paste0(loc.5m, indices.loc.5m, "^", ticker.indices[i],".txt")
        data.loc [pos+i, 3] <- paste0(loc.hourly, indices.loc.hourly, "^", ticker.indices[i],".txt")
        data.loc [pos+i, 4] <- paste0(loc.daily, indices.loc.daily, "^", ticker.indices[i],".txt")
}

# LOAD HISTORICAL DATA

# Load data in a list of data frames from zip file ####

sig.hist <- list()

data.FiveMins <- list()
data.Hourly <- list()
data.Daily <- list()
for (i in 1:nrow(data.loc)) {
        list.name <- data.loc$Ticker[i]
        
        data.FiveMins[[list.name]] <- read.delim(unz(description=zip.loc.5m, filename = tolower(data.loc[data.loc$Ticker==list.name,"FiveMins"])), header=TRUE, sep=',', stringsAsFactors = FALSE)
        data.Hourly[[list.name]] <- read.delim(unz(description=zip.loc.hourly, filename = tolower(data.loc[data.loc$Ticker==list.name,"Hourly"])), header=TRUE, sep=',', stringsAsFactors = FALSE)
        data.Daily[[list.name]] <- read.delim(unz(description=zip.loc.daily, filename = tolower(data.loc[data.loc$Ticker==list.name,"Daily"])), header=TRUE, sep=',', stringsAsFactors = FALSE)
}
closeAllConnections()

# Modify the data frames from above list and create a list of xts ####

data.FiveMins.xts <- list()
data.Hourly.xts <- list()
data.Daily.xts <- list()

for (i in 1:nrow(data.loc)) {
        list.name <- data.loc$Ticker[i]
        
        data.FiveMins[[i]]$DateFull <- strptime(paste(data.FiveMins[[i]]$Date, data.FiveMins[[i]]$Time), format='%Y-%m-%d %H:%M:%S', tz="CET")
        data.FiveMins[[i]]$Date <- data.FiveMins[[i]]$DateFull
        data.FiveMins[[i]]$Time <- NULL
        data.FiveMins[[i]]$Volume <- NULL
        data.FiveMins[[i]]$OpenInt <- NULL
        data.FiveMins[[i]]$DateFull <- NULL
        data.FiveMins.xts[[list.name]] <- xts(data.FiveMins[[i]][,-1], order.by = data.FiveMins[[i]][,1])
        indexTZ(data.FiveMins.xts[[list.name]]) <-"CET"
        
        data.Hourly[[i]]$DateFull <- strptime(paste(data.Hourly[[i]]$Date, data.Hourly[[i]]$Time), format='%Y-%m-%d %H:%M:%S', tz="CET")
        data.Hourly[[i]]$Date <- data.Hourly[[i]]$DateFull
        data.Hourly[[i]]$Time <- NULL
        data.Hourly[[i]]$Volume <- NULL
        data.Hourly[[i]]$OpenInt <- NULL
        data.Hourly[[i]]$DateFull <- NULL
        data.Hourly.xts[[list.name]] <- xts(data.Hourly[[i]][,-1], order.by = data.Hourly[[i]][,1])
        indexTZ(data.Hourly.xts[[list.name]]) <-"CET"
        
        data.Daily[[i]]$DateFull <- strptime(paste(data.Daily[[i]]$Date, data.Daily[[i]]$Time), format='%Y%m%d', tz="CET")
        data.Daily[[i]]$Date <- data.Daily[[i]]$DateFull
        data.Hourly[[i]]$Time <- NULL
        data.Daily[[i]]$Volume <- NULL
        data.Daily[[i]]$OpenInt <- NULL
        data.Daily[[i]]$DateFull <- NULL
        data.Daily.xts[[list.name]] <- xts(data.Daily[[i]][,-1], order.by = data.Daily[[i]][,1])
        indexTZ(data.Daily.xts[[list.name]]) <-"CET"
}

#### DS Specs Import ####

Specs <- read.csv("./data/Stooq/TickerList/Specs.csv", header=T, stringsAsFactors = F)
SpecsMini <- read.csv("./data/Stooq/TickerList/SpecsMini.csv", header=T, stringsAsFactors = F)

MM <- function(cap=numeric(),instr=character(),lots=numeric(),TrP=numeric(),SLT=numeric(),risk=numeric(),ATRx=numeric(),SARTF=numeric(),SLATR=numeric()){
        
        if(TrP==0){
                if (lots>0 | SLATR>0) {
                        SLATRxM05 <- DS$Close[DS$Ticker==instr] - ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                        SLATRxM15 <- DS$Close[DS$Ticker==instr] - ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                        SLATRxH1 <- DS$Close[DS$Ticker==instr] - ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                        SLATRxH4 <- DS$Close[DS$Ticker==instr] - ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                        SLATRxD <- DS$Close[DS$Ticker==instr] - ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                        SLATRxW <- DS$Close[DS$Ticker==instr] - ATR.MM$W[ATR.MM$Ticker==instr]*ATRx
                } else if (lots<0 | SLATR<0) {
                        SLATRxM05 <- DS$Close[DS$Ticker==instr] + ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                        SLATRxM15 <- DS$Close[DS$Ticker==instr] + ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                        SLATRxH1 <- DS$Close[DS$Ticker==instr] + ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                        SLATRxH4 <- DS$Close[DS$Ticker==instr] + ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                        SLATRxD <- DS$Close[DS$Ticker==instr] + ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                        SLATRxW <- DS$Close[DS$Ticker==instr] + ATR.MM$W[ATR.MM$Ticker==instr]*ATRx   
                }
        } else if (lots>0) {
                SLATRxM05 <- TrP - ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                SLATRxM15 <- TrP - ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                SLATRxH1 <- TrP - ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                SLATRxH4 <- TrP - ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                SLATRxD <- TrP - ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                SLATRxW <- TrP - ATR.MM$W[ATR.MM$Ticker==instr]*ATRx
        } else if (lots<0){
                SLATRxM05 <- TrP + ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                SLATRxM15 <- TrP + ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                SLATRxH1 <- TrP + ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                SLATRxH4 <- TrP + ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                SLATRxD <- TrP + ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                SLATRxW <- TrP + ATR.MM$W[ATR.MM$Ticker==instr]*ATRx   
        }
        
        if (SLT==0){
                if (abs(SLATR)==1) {SLT <- SLATRxM05}
                else if (abs(SLATR)==2) {SLT <- SLATRxM15}
                else if (abs(SLATR)==3) {SLT <- SLATRxH1}
                else if (abs(SLATR)==4) {SLT <- SLATRxH4}
                else if (abs(SLATR)==5) {SLT <- SLATRxD}
                else if (abs(SLATR)==6) {SLT <- SLATRxW}
        }
        
        if (SLT!=0 & TrP!=0) {
                lot.2T <- round((risk / 100 * cap) / (abs(TrP - SLT) * DS$TV.RON[DS$Ticker==instr] * DS$Pips[DS$Ticker==instr]),2)
                if (SLT > TrP) {lot.2T <- -lot.2T}
        }
        else if (SLT!=0 & TrP==0) {lot.2T <- round((risk / 100 * cap) / (abs(DS$Close[DS$Ticker==instr] - SLT) * DS$TV.RON[DS$Ticker==instr] * DS$Pips[DS$Ticker==instr]),2)
        if (SLT > DS$Close[DS$Ticker==instr]) {lot.2T <- -lot.2T}
        }
        else {lot.2T <- 0}                
        if (lots==0 & SLT!=0) {lots <- round(lot.2T)} 
        else if (lots==0) {lots <- 1}
        if (lots==0) {stop("Lot is 0 !!! \n\n")}
        
        lot.NoL <- round(cap / DS$VAL.RON[DS$Ticker==instr],2)
        LEV <- abs(round(DS$VAL.RON[DS$Ticker==instr] * lots / cap,2))
        
        SL.pips <- - round(risk/100*cap/DS$TV.RON[DS$Ticker==instr],4)/lots
        if (lots>0) {
                ifelse(TrP==0, SL <- round(DS$Close[DS$Ticker==instr]-abs(SL.pips)/DS$Pips[DS$Ticker==instr],4),
                       SL <- round(TrP-abs(SL.pips)/DS$Pips[DS$Ticker==instr],4))
        }
        else if (lots<0) {
                ifelse(TrP==0, SL <- round(DS$Close[DS$Ticker==instr]+abs(SL.pips)/DS$Pips[DS$Ticker==instr],4),
                       SL <- round(TrP+abs(SL.pips)/DS$Pips[DS$Ticker==instr],4))
        }
        
        
        if (TrP!=0) {ifelse(SLT==0,SLT.pips<-0,SLT.pips <- round((SLT-TrP)*DS$Pips[DS$Ticker==instr],4))}
        else {ifelse(SLT==0,SLT.pips<-0,SLT.pips <- round((SLT-DS$Close[DS$Ticker==instr])*DS$Pips[DS$Ticker==instr],4))}
        
        risk.VAL <- round(risk/100*cap,2)
        
        if (TrP!=0) {ifelse(SLT==0,risk.TVAL<-0,risk.TVAL <- round(DS$TV.RON[DS$Ticker==instr] * (SLT-TrP) * DS$Pips[DS$Ticker==instr] * lots,2))}
        else {ifelse(SLT==0,risk.TVAL<-0,risk.TVAL <- round(DS$TV.RON[DS$Ticker==instr] * (SLT-DS$Close[DS$Ticker==instr]) * DS$Pips[DS$Ticker==instr] * lots,2))}
        
        if (TrP!=0) {ifelse(SLT==0,risk.TVAL1<-0,risk.TVAL1 <- round(DS$TV.RON[DS$Ticker==instr] * (SLT-TrP) * DS$Pips[DS$Ticker==instr] * lot.2T,2))}
        else {ifelse(SLT==0,risk.TVAL1<-0,risk.TVAL1 <- round(DS$TV.RON[DS$Ticker==instr] * (SLT-DS$Close[DS$Ticker==instr]) * DS$Pips[DS$Ticker==instr] * lot.2T,2))}

        if (lots>0) {
                M05 <- DS$Close[DS$Ticker==instr] - ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                M15 <- DS$Close[DS$Ticker==instr] - ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                H1 <- DS$Close[DS$Ticker==instr] - ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                H4 <- DS$Close[DS$Ticker==instr] - ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                D <- DS$Close[DS$Ticker==instr] - ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                W <- DS$Close[DS$Ticker==instr] - ATR.MM$W[ATR.MM$Ticker==instr]*ATRx
        } else if (lots<0) {
                M05 <- DS$Close[DS$Ticker==instr] + ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                M15 <- DS$Close[DS$Ticker==instr] + ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                H1 <- DS$Close[DS$Ticker==instr] + ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                H4 <- DS$Close[DS$Ticker==instr] + ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                D <- DS$Close[DS$Ticker==instr] + ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                W <- DS$Close[DS$Ticker==instr] + ATR.MM$W[ATR.MM$Ticker==instr]*ATRx   
        } else if (lots==0) {
                M05 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx),2)
                M15 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx),2)
                H1 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx),2)
                H4 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx),2)
                D <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$D[ATR.MM$Ticker==instr]*ATRx),2)
                W <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$W[ATR.MM$Ticker==instr]*ATRx),2)
        }
        
        if (abs(SLATR)==1) {ATR <- M05}
        else if (abs(SLATR)==2) {ATR <- M15}
        else if (abs(SLATR)==3) {ATR <- H1}
        else if (abs(SLATR)==4) {ATR <- H4}
        else if (abs(SLATR)==5) {ATR <- D}
        else if (abs(SLATR)==6) {ATR <- W}
        
        if(TrP==0){M2M <- 0}
        else if(lots>0) {
                M2M <- (DS$Close[DS$Ticker==instr]-TrP)*DS$Pips[DS$Ticker==instr]*DS$TV.RON[DS$Ticker==instr]*abs(lots)
        } else if(lots<0) {
                M2M <- (TrP-DS$Close[DS$Ticker==instr])*DS$Pips[DS$Ticker==instr]*DS$TV.RON[DS$Ticker==instr]*abs(lots)
        }
        df.res<- data.frame(Ticker=instr,
                            Lots=lots,
                            Close=DS$Close[DS$Ticker==instr],
                            Price.T=TrP,
                            PipSL.T=SLT.pips,
                            SL.T=round(SLT,4),
                            SAR=round(SAR.MM[ATR.MM$Ticker==instr,SARTF+1],4),
                            Risk.T=risk.TVAL,
                            Leverage=LEV,
                            M2M=M2M,
                            PipSL.Risk=SL.pips, 
                            SL.Risk=SL,
                            Lot.Risk=lot.2T,
                            VAL.Risk=risk.TVAL1,
                            Risk=risk.VAL,
                            Lot.NoL=lot.NoL,
                            Margin=round(DS$MG.RON[DS$Ticker==instr],2)*abs(lots),
                            ATRxM05=M05,ATRxM15=M15,ATRxH1=H1,ATRxH4=H4,ATRxD=D,ATRxW=W,
                            SL.ATRxM05=round(SLATRxM05,4),SL.ATRxM15=round(SLATRxM15,4),SL.ATRxH1=round(SLATRxH1,4),
                            SL.ATRxH4=round(SLATRxH4,4),SL.ATRxD=round(SLATRxD,4),SL.ATRxW=round(SLATRxW,4),
                            SARxM05=round(SAR.MM[SAR.MM$Ticker==instr,2],4),
                            SARxM15=round(SAR.MM[SAR.MM$Ticker==instr,3],4),
                            SARxH1=round(SAR.MM[SAR.MM$Ticker==instr,4],4),
                            SARxH4=round(SAR.MM[SAR.MM$Ticker==instr,5],4),
                            SARxD=round(SAR.MM[SAR.MM$Ticker==instr,6],4),
                            SARxW=round(SAR.MM[SAR.MM$Ticker==instr,7],4),
                            ATR=ATR)
        return(df.res)
}

# Create new timeframes ####

skip.update <- FALSE
skip.update <- (as.character(weekdays(Sys.Date())) == "Saturday") | (as.character(weekdays(Sys.Date())) == "Sunday")

if (skip.update == TRUE) {
        data.M05.xts <- list()
        data.M15.xts <- list()
        data.H1.xts <- list()
        data.H4.xts <- list()
        data.D.xts <- list()
        data.W.xts <- list()
        
        for (i in 1:nrow(data.loc)) {
                list.name <- data.loc$Ticker[i]
                data.M05.xts[[list.name]]<-data.FiveMins.xts[[list.name]]
                
                data.M15.xts[[list.name]] <- to.minutes15(data.M05.xts[[i]])
                colnames(data.M15.xts[[list.name]]) <- colnames(data.M05.xts[[i]])
                
                data.H1.xts[[list.name]]<-data.Hourly.xts[[list.name]]
                
                data.H4.xts[[list.name]] <- to.period(data.H1.xts[[i]], period = "hours", k=4)
                colnames(data.H4.xts[[list.name]]) <- colnames(data.H1.xts[[i]])
                
                data.D.xts[[list.name]]<-data.Daily.xts[[list.name]]
                
                data.W.xts[[list.name]] <- to.weekly(data.D.xts[[i]])
                colnames(data.W.xts[[list.name]]) <- colnames(data.D.xts[[i]])
        }
}
