#### Update ####

# SyT.start <- Sys.time()
 
# source("./R Trading/StooqH.R")
 
# source("./R Trading/Hist.R")

SyT.start <- Sys.time()

source("./R Trading/Update.R")
source("./R Trading/Ind.R")
source("./R Trading/SigHist.R")

SyT.end <- Sys.time()
SyT.end - SyT.start

FLT(invest = 2000,risk = 5,ATRx = 2,SLATR.ST = 3,SLATR.LT = 5,SARTF.ST = 3,SARTF.LT = 5)
source("./R Trading/Pos.R")

#### First Update ####

SyT.start <- Sys.time()

StooqH.R <- system.time(source("./R Trading/StooqH.R"))

Hist.R <- system.time(source("./R Trading/Hist.R"))

DBSqlite.R <- system.time(source("./R Trading/DBSqlite.R"))

if (!((as.character(weekdays(Sys.Date())) == "Saturday") | (as.character(weekdays(Sys.Date())) == "Sunday"))) {
        Update.R <- system.time(source("./R Trading/Update.R"))}
Ind.R <- system.time(source("./R Trading/Ind.R"))
SigHist.R <- system.time(source("./R Trading/SigHist.R"))

SyT.end <- Sys.time()
SyT.end - SyT.start

SysTime <- cbind(StooqH.R,Hist.R,DBSqlite.R,Update.R,Ind.R,SigHist.R)
SysTime <- as.data.frame(SysTime[1:3,])
SyT.file <- paste0("./data/Stooq/SysTime/SysTime_", gsub(":","-",Sys.time()),".csv")
write.csv(SysTime, SyT.file)

FLT(invest = 2000,risk = 5,ATRx = 2,SLATR.ST = 3,SLATR.LT = 5,SARTF.ST = 3,SARTF.LT = 5)
source("./R Trading/Pos.R")


#### Loop Update ####

S.Update <- function(x)
{
        p1 <- proc.time()
        
        source("./R Trading/Update.R")
        source("./R Trading/Ind.R")
        source("./R Trading/SigHist.R")
        
        Sys.sleep(x)
        proc.time() - p1 # The cpu usage should be negligible
}

for (i in 1:5){
        S.Update(60*30)
}

#### Filter Lists ####

invest <- 2000
risk <- 10
ATRx <- 3
SLATR.ST <- 3
SLATR.LT <- 5
SARTF.ST <- 3
SARTF.LT <- 3

FLT(invest = invest, risk = risk, ATRx =ATRx, SLATR.ST = SLATR.ST, SLATR.LT = SLATR.LT, SAR.ST = SAR.ST, SAR.LT = SAR.LT)
FLT(invest = 2000,risk = 5,ATRx = 2,SLATR.ST = 4,SLATR.LT = 5,SARTF.ST = 3,SARTF.LT = 5)

ST[order(ST$Sig.s,ST$Sig.l), ]
LT[order(LT$Sig.l,LT$Sig.s), ]

#### Charting ####

tail.M05<-200;tail.M15<-200;tail.H1<-200;tail.H4<-200;tail.D<-200;tail.W<-200
chart.ticker <- "EURAUD"

graphics.off()
windows()
par(mfrow=c(3,2))
chartSeries(tail(data.M05.xts[[chart.ticker]],tail.M05),layout = NULL, TA = "addEMA(100,col='dark blue');addEMA(50,col='red');addBBands()")
chartSeries(tail(data.M15.xts[[chart.ticker]],tail.M15),layout = NULL, TA = "addEMA(100,col='dark blue');addEMA(50,col='red');addBBands()")
chartSeries(tail(data.H1.xts[[chart.ticker]],tail.H1),layout = NULL, TA = "addEMA(100,col='dark blue');addEMA(50,col='red');addBBands()")
chartSeries(tail(data.H4.xts[[chart.ticker]],tail.H4),layout = NULL, TA = "addEMA(100,col='dark blue');addEMA(50,col='red');addBBands()")
chartSeries(tail(data.D.xts[[chart.ticker]],tail.D),layout = NULL, TA = "addEMA(100,col='dark blue');addEMA(50,col='red');addBBands()")
chartSeries(tail(data.W.xts[[chart.ticker]],tail.W),layout = NULL, TA = "addEMA(100,col='dark blue');addEMA(50,col='red');addBBands()")


par(mfrow=c(1,1))

#### Trades Check ####

invest <- 2000
instr <- DS$Ticker
# instr <- c("EURUSD","GBPUSD")
instr <- "USDCHF"
lots <- 4
TrP <- 0.96271
SLT <- 0
risk <- 5
ATRx <- 2
SLATR <- 4
SARTF <- 3

MMDF <- MM(cap = invest, instr = instr, lots = lots, TrP = TrP, SLT = SLT , risk = risk, ATRx = ATRx, SARTF = SARTF, SLATR = SLATR)
MMDF[,c(1:7,36,8:9,17)]

MMDF <- MM(cap = invest, instr = instr, lots = 0, TrP = 0, SLT = 0 , risk = risk, ATRx = ATRx, SARTF = SARTF, SLATR = SLATR)
write.csv(MMDF,"MM.csv")

MMDF <- MM(cap = invest, instr = DS$Ticker, lots = 0, TrP = 0, SLT = 0 , risk = risk, ATRx = ATRx, SARTF = SARTF, SLATR = SLATR)
write.csv(MMDF[MMDF$Lots!=0, ], "MM-Non0.csv")


#### Position Check ####

invest <- 2000
risk.tr <- 5
Pos <- data.frame()
Pos <- rbind(Pos
,MM(cap = invest, instr = "EURCAD", lots = 4, TrP = 1.42624, SLT = 1.4200, risk = risk.tr, 
    ATRx = 2, SARTF = 5, SLATR = 4)
,MM(cap = invest, instr = "EURAUD", lots = 3, TrP = 1.48948, SLT = 1.4940, risk = risk.tr, 
    ATRx = 2, SARTF = 5, SLATR = 4)
,MM(cap = invest, instr = "AUDJPY", lots = -8, TrP = 90.272, SLT = 90.85, risk = risk.tr, 
    ATRx = 2, SARTF = 5, SLATR = 4)
,MM(cap = invest, instr = "SPX", lots = -2, TrP = 2093.04, SLT = 2105.00, risk = risk.tr, 
    ATRx = 2, SARTF = 3, SLATR = 3)
,MM(cap = invest, instr = "DAXm", lots = -2, TrP = 1133.12, SLT = 1145.00, risk = risk.tr, 
    ATRx = 2, SARTF = 3, SLATR = 3)
)

Pos$PosRisk.TV <- sum(Pos$Risk.T)
Pos$PosRisk.T <- sum(Pos$Risk.T)/invest
Pos$PosRiskV <- sum(Pos$Risk)
Pos$PosRisk <- sum(Pos$Risk)/invest

Pos$PosLev <- sum(abs(Pos$Leverage))
Pos$PosM2M <- sum(Pos$M2M)
Pos$PosMG <- round(sum(DS$MG.RON[which(DS$Ticker %in% Pos$Ticker)]*abs(Pos$Lots))/invest,4)
Pos$PosVAL <- sum(DS$VAL.RON[which(DS$Ticker %in% Pos$Ticker)]*abs(Pos$Lots))
Pos$Time <- Sys.time()

# Lots & Signals Update
ATR.in <- which(matrix(Pos$ATR==Pos[,c("ATRxM05","ATRxM15","ATRxH1","ATRxH4","ATRxD","ATRxW")],nrow(Pos):6)==TRUE, arr.ind = TRUE)
ATR.in <- data.frame(ATR.in[order(ATR.in[,1]),])
ATRx.in <- ATR.MM[ATR.MM$Ticker %in% Pos$Ticker,c(1:7)]
SAR.in <- which(matrix(Pos$SAR==Pos[,c("SARxM05","SARxM15","SARxH1","SARxH4","SARxD","SARxW")],nrow(Pos):6)==TRUE, arr.ind = TRUE)
SAR.in <- data.frame(SAR.in[order(SAR.in[,1]),])
SIG.in <- SIG.ALL[SIG.ALL$Ticker %in% Pos$Ticker,]
SIG.in <- SIG.in[match(Pos$Ticker, SIG.in$Ticker),]

Lot.upd <- numeric()
for (i in 1:nrow(Pos)){
        Lot.tmp <-
                MM(cap = invest, instr = Pos$Ticker[i], lots = 0, TrP = 0, SLT = 0, risk = risk.tr, 
                   ATRx = abs(Pos$ATR[i]-Pos$Close[i])/ATRx.in[Pos$Ticker[i]==ATRx.in$Ticker,(ATR.in[i,2])+1],
                   SARTF = as.numeric(SAR.in[i,2]), SLATR = as.numeric(ATR.in[i,2]))$Lot.Risk * sign(Pos$Lots[i])
                
        Lot.upd <- c(Lot.upd,Lot.tmp)
}
Pos$LotUpd <- Lot.upd
Pos$Sig.l <- SIG.in$Sig.l
Pos$LT.D <- SIG.in$LT.D
Pos$Sig.s <- SIG.in$Sig.s
Pos$ST.H1 <- SIG.in$ST.H1
Pos$Sig.d <- SIG.in$Sig.d
Pos$Sig <- SIG.in$Sig

# Write csv
write.csv(Pos,"./data/Stooq/Results/POS.csv")
Pos.file <- paste0("./data/Stooq/History/POS_", gsub(":","-",Sys.time()),".csv")
write.csv(Pos,Pos.file)

if (!exists("PosHist")) {PosHist <- data.frame()}
PosHist <- rbind(PosHist,Pos)
PosHist.file <- paste0("./data/Stooq/History/POS_", Sys.Date(),".csv")
write.csv(PosHist,PosHist.file)

#### Correlations ####

cor.list <- data.loc$Ticker
cor.list.ST <- ST$Ticker
cor.list.LT <- LT$Ticker
# cor.list <- c("EURAUD","AUDJPY","EURCAD")
# cor.list <- unique(rbind(ST,LT)$Ticker)
cor.list <- c(as.character(Pos$Ticker[1:4]),"DAX")
cor.list <- as.character(Pos$Ticker)

cor.df <- data.frame()
min.df <- min(as.data.frame(lapply(data.H1.xts[cor.list],nrow)))
for (i in 1:length(cor.list)){
        st <- (nrow(data.H1.xts[[cor.list[i]]])-min.df)
        for (j in 1:min(as.data.frame(lapply(data.H1.xts[cor.list],nrow)))){
                cor.df[j, cor.list[i]] <- as.numeric(data.H1.xts[[cor.list[i]]]$Close[j+st])
        }
}
cor.df.ST <- cor.df

cor.df <- data.frame()
min.df <- min(as.data.frame(lapply(data.D.xts[cor.list],nrow)))
for (i in 1:length(cor.list)){
        st <- (nrow(data.D.xts[[cor.list[i]]])-min.df)
        for (j in 1:min(as.data.frame(lapply(data.D.xts[cor.list],nrow)))){
                cor.df[j, cor.list[i]] <- as.numeric(data.D.xts[[cor.list[i]]]$Close[j+st])
        }
}
cor.df.LT <- cor.df

cor.wind.ST <- min(1e10, nrow(cor.df.ST))
out.ST <- rollapplyr(cor.df.ST, cor.wind.ST, function(x) c(cor(x)), by.column = FALSE)
L.ST <- lapply(1:nrow(out.ST), function(i) matrix(out.ST[i, ], ncol(cor.df.ST)))
for (i in 1:length(L.ST)){
        colnames(L.ST[[i]]) <- cor.list
        rownames(L.ST[[i]]) <- cor.list
}

cor.wind.LT <- min(5e10, nrow(cor.df.LT))
out.LT <- rollapplyr(cor.df.LT, cor.wind.LT, function(x) c(cor(x)), by.column = FALSE)
L.LT <- lapply(1:nrow(out.LT), function(i) matrix(out.LT[i, ], ncol(cor.df.LT)))
for (i in 1:length(L.LT)){
        colnames(L.LT[[i]]) <- cor.list
        rownames(L.LT[[i]]) <- cor.list
}

corel.ST <- L.ST[[length(L.ST)]]
mean.cor.ST <- mean(abs(corel.ST[lower.tri(corel.ST)]))
corel.LT <- L.LT[[length(L.LT)]]
mean_cor.LT <- mean(abs(corel.LT[lower.tri(corel.LT)]))

write.csv(corel.ST,"./data/Stooq/Results/corelST.csv")
write.csv(corel.LT,"./data/Stooq/Results/corelLT.csv")

# Chart Correlations
cor.plot1 <- "DAX"
cor.plot2 <- "SPX"
cor.plot.ST <- data.frame(Date=tail(index(data.H1.xts[[cor.plot1]]),length(L.ST)),
                       Correlation=sapply(1:length(L.ST), function(i) L.ST[[i]][cor.plot1,cor.plot2]))
cor.plot.LT <- data.frame(Date=tail(index(data.D.xts[[cor.plot1]]),length(L.LT)),
                       Correlation=sapply(1:length(L.LT), function(i) L.LT[[i]][cor.plot1,cor.plot2]))

cor.plot <- cor.plot.ST # change between ST & LT

g <- ggplot(cor.plot, aes(Date,Correlation))
# g <- g + geom_point(col="red", size=2)
g <- g + geom_line(col="blue", size=1)
g <- g + geom_hline(y=0, col="red")
g <- g + ggtitle(paste("Correlation of",cor.plot1,"with",cor.plot2))
g

# Check Correlations
cor.ticker <- "SPX"
cor.max <- 0.05

corel.ST[cor.ticker,abs(corel.ST[cor.ticker, ])<cor.max]
corel.LT[cor.ticker,abs(corel.LT[cor.ticker, ])<cor.max]

min.cor.ST <- data.frame(Ticker1 = rownames(corel.ST[as.numeric(which(abs(corel.ST)<cor.max,arr.ind = T)[,1]),as.numeric(which(abs(corel.ST)<cor.max,arr.ind = T)[,2])]),
                         Ticker2 = colnames(corel.ST[as.numeric(which(abs(corel.ST)<cor.max,arr.ind = T)[,1]),as.numeric(which(abs(corel.ST)<cor.max,arr.ind = T)[,2])]))
min.cor.ST[min.cor.ST$Ticker1==cor.ticker,]

min.cor.LT <- data.frame(Ticker1 = rownames(corel.LT[as.numeric(which(abs(corel.LT)<cor.max,arr.ind = T)[,1]),as.numeric(which(abs(corel.LT)<cor.max,arr.ind = T)[,2])]),
                         Ticker2 = colnames(corel.LT[as.numeric(which(abs(corel.LT)<cor.max,arr.ind = T)[,1]),as.numeric(which(abs(corel.LT)<cor.max,arr.ind = T)[,2])]))
min.cor.LT[min.cor.LT$Ticker1==cor.ticker,]


#### EOP ####

tk <- "AUDJPY"
tail(data.M05.xts[[tk]])
tail(data.D.xts[[tk]])
tail(data.Daily.xts[[tk]])

tail(BBands(data.M05.xts[[tk]][,c("High","Low","Close")]))
tail(BBands(data.D.xts[[tk]][,c("High","Low","Close")]))

windows()
chartSeries(tail(data.H4.xts[[tk]],tail.D),layout = NULL, TA = "addEMA(20);addEMA(50,col='red');addBBands(n=20,sd=0.5)")

# M05
i=3
as.numeric(sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$BB$up)), wind.atr, sum, na.rm=TRUE))==1)*1 +
as.numeric(sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$BB$dn)), wind.atr, sum, na.rm=TRUE))==-1)*-1
BBs
RSIs
tail(M05.Ind[[tk]]$BB$dn)
tail(M05.Ind[[tk]]$BB$up)
tail(BBs.Hist[[i]],15)
tail(H4.Ind[[i]]$BB,10)
tail(H4.Ind[[i]]$BB,10)

i <- "EURAUD"

test <- read.delim(unz(description=zip.loc.5m, filename = tolower(data.loc[data.loc$Ticker==list.name,"FiveMins"])), header=TRUE, sep=',', stringsAsFactors = FALSE)[1:3,]
str(test)

gsub(".F","",data.loc$Ticker[i], fixed = TRUE)
data.FiveMins[[i]][substr(data.FiveMins[[i]]$Date,1,10)==as.character(Sys.Date()-1),]
data.Hourly[[i]][substr(data.Hourly[[i]]$Date,1,10)==as.character(Sys.Date()-1),]
data.Daily[[i]][substr(data.Daily[[i]]$Date,1,10)==as.character(Sys.Date()-1),]
CL
tail(data.Daily[["USDCAD"]])
i <- "USDCAD"
tail(data.Daily.xts[[i]])
tail(data.Hourly.xts[[i]])
tail(data.FiveMins.xts[[i]])

tail(data.M05.xts[["NZDUSD"]])
(DS[DS$Ticker=="EURILS",]$TV.RON*80/DS[DS$Ticker=="EURILS",]$VAL.RON)*100
DS[DS$Ticker=="EURILS",]$SPR
