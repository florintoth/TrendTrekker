#### Indicators List ####

ind.tail <- 500
EMA.p1 <- 5
EMA.p2 <- 20
EMA.p3 <- 50
EMA.p4 <- 100
EMA.p5 <- 200
ADX.n <- 14
ATR.n <- 14
SAR.Accel <- 0.02
SAR.maxAccel <- 0.2
BB.n <- 20
BB.sd <- 2

M05.Ind <- list()
M15.Ind <- list()
H1.Ind <- list()
H4.Ind <- list()
D.Ind <- list()
W.Ind <- list()

for (i in 1:nrow(data.loc)) {
        list.name <- data.loc$Ticker[i]
        # M05
        M05.Ind[[list.name]]$EMAnr1 <-
                tail(EMA(data.M05.xts[[i]]$Close,EMA.p1),ind.tail)
        M05.Ind[[list.name]]$EMAnr2 <-
                tail(EMA(data.M05.xts[[i]]$Close,EMA.p2),ind.tail)
        M05.Ind[[list.name]]$EMAnr3 <-
                tail(EMA(data.M05.xts[[i]]$Close,EMA.p3),ind.tail)
#         M05.Ind[[list.name]]$EMAnr4 <-
#                 tail(EMA(data.M05.xts[[i]]$Close,EMA.p4),ind.tail)
#         M05.Ind[[list.name]]$EMAnr5 <-
#                 tail(EMA(data.M05.xts[[i]]$Close,EMA.p5),ind.tail)
        M05.Ind[[list.name]]$MACD <-
                tail(MACD(data.M05.xts[[i]]$Close),ind.tail)
        M05.Ind[[list.name]]$RSI <-
                tail(RSI(data.M05.xts[[i]]$Close),ind.tail)
        M05.Ind[[list.name]]$ADX <-
                tail(ADX(data.M05.xts[[i]][,c("High","Low","Close")],n = ADX.n),ind.tail)
        M05.Ind[[list.name]]$ATR <-
                tail(ATR(data.M05.xts[[i]][,c("High","Low","Close")],n = ATR.n),ind.tail)
        M05.Ind[[list.name]]$BB <-
                tail(BBands(data.M05.xts[[i]][,c("High","Low","Close")],n = BB.n,sd = BB.sd),ind.tail)
        M05.Ind[[list.name]]$SAR <-
                tail(SAR(data.M05.xts[[i]][,c("High","Low")],accel =  c(SAR.Accel, SAR.maxAccel)),ind.tail)
        # M15
        M15.Ind[[list.name]]$EMAnr1 <-
                tail(EMA(data.M15.xts[[i]]$Close,EMA.p1),ind.tail)
        M15.Ind[[list.name]]$EMAnr2 <-
                tail(EMA(data.M15.xts[[i]]$Close,EMA.p2),ind.tail)
        M15.Ind[[list.name]]$EMAnr3 <-
                tail(EMA(data.M15.xts[[i]]$Close,EMA.p3),ind.tail)
#         M15.Ind[[list.name]]$EMAnr4 <-
#                 tail(EMA(data.M15.xts[[i]]$Close,EMA.p4),ind.tail)
#         M15.Ind[[list.name]]$EMAnr5 <-
#                 tail(EMA(data.M15.xts[[i]]$Close,EMA.p5),ind.tail)
        M15.Ind[[list.name]]$MACD <-
                tail(MACD(data.M15.xts[[i]]$Close),ind.tail)
        M15.Ind[[list.name]]$RSI <-
                tail(RSI(data.M15.xts[[i]]$Close),ind.tail)
        M15.Ind[[list.name]]$ADX <-
                tail(ADX(data.M15.xts[[i]][,c("High","Low","Close")],n = ADX.n),ind.tail)
        M15.Ind[[list.name]]$ATR <-
                tail(ATR(data.M15.xts[[i]][,c("High","Low","Close")],n = ATR.n),ind.tail)
        M15.Ind[[list.name]]$BB <-
                tail(BBands(data.M15.xts[[i]][,c("High","Low","Close")],n = BB.n,sd = BB.sd),ind.tail)
        M15.Ind[[list.name]]$SAR <-
                tail(SAR(data.M15.xts[[i]][,c("High","Low")],accel =  c(SAR.Accel, SAR.maxAccel)),ind.tail)
        # H1
        H1.Ind[[list.name]]$EMAnr1 <-
                tail(EMA(data.H1.xts[[i]]$Close,EMA.p1),ind.tail)
        H1.Ind[[list.name]]$EMAnr2 <-
                tail(EMA(data.H1.xts[[i]]$Close,EMA.p2),ind.tail)
        H1.Ind[[list.name]]$EMAnr3 <-
                tail(EMA(data.H1.xts[[i]]$Close,EMA.p3),ind.tail)
#         H1.Ind[[list.name]]$EMAnr4 <-
#                 tail(EMA(data.H1.xts[[i]]$Close,EMA.p4),ind.tail)
#         H1.Ind[[list.name]]$EMAnr5 <-
#                 tail(EMA(data.H1.xts[[i]]$Close,EMA.p5),ind.tail)
        H1.Ind[[list.name]]$MACD <-
                tail(MACD(data.H1.xts[[i]]$Close),ind.tail)
        H1.Ind[[list.name]]$RSI <-
                tail(RSI(data.H1.xts[[i]]$Close),ind.tail)
        H1.Ind[[list.name]]$ADX <-
                tail(ADX(data.H1.xts[[i]][,c("High","Low","Close")],n = ADX.n),ind.tail)
        H1.Ind[[list.name]]$ATR <-
                tail(ATR(data.H1.xts[[i]][,c("High","Low","Close")],n = ATR.n),ind.tail)
        H1.Ind[[list.name]]$BB <-
                tail(BBands(data.H1.xts[[i]][,c("High","Low","Close")],n = BB.n,sd = BB.sd),ind.tail)
        H1.Ind[[list.name]]$SAR <-
                tail(SAR(data.H1.xts[[i]][,c("High","Low")],accel =  c(SAR.Accel, SAR.maxAccel)),ind.tail)
        # H4
        H4.Ind[[list.name]]$EMAnr1 <-
                tail(EMA(data.H4.xts[[i]]$Close,EMA.p1),ind.tail)
        H4.Ind[[list.name]]$EMAnr2 <-
                tail(EMA(data.H4.xts[[i]]$Close,EMA.p2),ind.tail)
        H4.Ind[[list.name]]$EMAnr3 <-
                tail(EMA(data.H4.xts[[i]]$Close,EMA.p3),ind.tail)
#         H4.Ind[[list.name]]$EMAnr4 <-
#                 tail(EMA(data.H4.xts[[i]]$Close,EMA.p4),ind.tail)
#         H4.Ind[[list.name]]$EMAnr5 <-
#                 tail(EMA(data.H4.xts[[i]]$Close,EMA.p5),ind.tail)
        H4.Ind[[list.name]]$MACD <-
                tail(MACD(data.H4.xts[[i]]$Close),ind.tail)
        H4.Ind[[list.name]]$RSI <-
                tail(RSI(data.H4.xts[[i]]$Close),ind.tail)
        H4.Ind[[list.name]]$ADX <-
                tail(ADX(data.H4.xts[[i]][,c("High","Low","Close")],n = ADX.n),ind.tail)
        H4.Ind[[list.name]]$ATR <-
                tail(ATR(data.H4.xts[[i]][,c("High","Low","Close")],n = ATR.n),ind.tail)
        H4.Ind[[list.name]]$BB <-
                tail(BBands(data.H4.xts[[i]][,c("High","Low","Close")],n = BB.n,sd = BB.sd),ind.tail)
        H4.Ind[[list.name]]$SAR <-
                tail(SAR(data.H4.xts[[i]][,c("High","Low")],accel =  c(SAR.Accel, SAR.maxAccel)),ind.tail)
        # Daily
        D.Ind[[list.name]]$EMAnr1 <-
                tail(EMA(data.D.xts[[i]]$Close,EMA.p1),ind.tail)
        D.Ind[[list.name]]$EMAnr2 <-
                tail(EMA(data.D.xts[[i]]$Close,EMA.p2),ind.tail)
        D.Ind[[list.name]]$EMAnr3 <-
                tail(EMA(data.D.xts[[i]]$Close,EMA.p3),ind.tail)
#         D.Ind[[list.name]]$EMAnr4 <-
#                 tail(EMA(data.D.xts[[i]]$Close,EMA.p4),ind.tail)
#         D.Ind[[list.name]]$EMAnr5 <-
#                 tail(EMA(data.D.xts[[i]]$Close,EMA.p5),ind.tail)
        D.Ind[[list.name]]$MACD <-
                tail(MACD(data.D.xts[[i]]$Close),ind.tail)
        D.Ind[[list.name]]$RSI <-
                tail(RSI(data.D.xts[[i]]$Close),ind.tail)
        D.Ind[[list.name]]$ADX <-
                tail(ADX(data.D.xts[[i]][,c("High","Low","Close")],n = ADX.n),ind.tail)
        D.Ind[[list.name]]$ATR <-
                tail(ATR(data.D.xts[[i]][,c("High","Low","Close")],n = ATR.n),ind.tail)
        D.Ind[[list.name]]$BB <-
                tail(BBands(data.D.xts[[i]][,c("High","Low","Close")],n = BB.n,sd = BB.sd),ind.tail)
        D.Ind[[list.name]]$SAR <-
                tail(SAR(data.D.xts[[i]][,c("High","Low")],accel =  c(SAR.Accel, SAR.maxAccel)),ind.tail)
        # Weekly
        W.Ind[[list.name]]$EMAnr1 <-
                tail(EMA(data.W.xts[[i]]$Close,EMA.p1),ind.tail)
        W.Ind[[list.name]]$EMAnr2 <-
                tail(EMA(data.W.xts[[i]]$Close,EMA.p2),ind.tail)
        W.Ind[[list.name]]$EMAnr3 <-
                tail(EMA(data.W.xts[[i]]$Close,EMA.p3),ind.tail)
#         W.Ind[[list.name]]$EMAnr4 <-
#                 tail(EMA(data.W.xts[[i]]$Close,EMA.p4),ind.tail)
#         W.Ind[[list.name]]$EMAnr5 <-
#                 tail(EMA(data.W.xts[[i]]$Close,EMA.p5),ind.tail)
        W.Ind[[list.name]]$MACD <-
                tail(MACD(data.W.xts[[i]]$Close),ind.tail)
        W.Ind[[list.name]]$RSI <-
                tail(RSI(data.W.xts[[i]]$Close),ind.tail)
        W.Ind[[list.name]]$ADX <-
                tail(ADX(data.W.xts[[i]][,c("High","Low","Close")],n = ADX.n),ind.tail)
        W.Ind[[list.name]]$ATR <-
                tail(ATR(data.W.xts[[i]][,c("High","Low","Close")],n = ATR.n),ind.tail)
        W.Ind[[list.name]]$BB <-
                tail(BBands(data.W.xts[[i]][,c("High","Low","Close")],n = BB.n,sd = BB.sd),ind.tail)
        W.Ind[[list.name]]$SAR <-
                tail(SAR(data.W.xts[[i]][,c("High","Low")],accel =  c(SAR.Accel, SAR.maxAccel)),ind.tail)
}

#### ATR & SAR & %BB Data Frames ####

# ATR

ATR <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

for (i in 1:nrow(data.loc)) {
        ATR[i,"Ticker"] <- data.loc$Ticker[i]
        ATR[i,"M05"] <- last(as.numeric(M05.Ind[[i]]$ATR$atr))
        ATR[i,"M15"] <- last(as.numeric(M15.Ind[[i]]$ATR$atr))
        ATR[i,"H1"] <- last(as.numeric(H1.Ind[[i]]$ATR$atr))
        ATR[i,"H4"] <- last(as.numeric(H4.Ind[[i]]$ATR$atr))
        ATR[i,"D"] <- last(as.numeric(D.Ind[[i]]$ATR$atr))
        ATR[i,"W"] <- last(as.numeric(W.Ind[[i]]$ATR$atr))
}

ATR.MM <- ATR[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),ATR$Ticker),]
ATR.MM[, 2:7] <- ATR.MM[, 2:7]/10
ATR.MM$Ticker <- SpecsMini$Ticker
ATR.MM <- rbind(ATR,ATR.MM)
write.csv(ATR.MM,"./data/Stooq/results/ATR.csv")

# SAR

SAR <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

for (i in 1:nrow(data.loc)) {
        SAR[i,"Ticker"] <- data.loc$Ticker[i]
        SAR[i,"M05"] <- last(as.numeric(M05.Ind[[i]]$SAR))
        SAR[i,"M15"] <- last(as.numeric(M15.Ind[[i]]$SAR))
        SAR[i,"H1"] <- last(as.numeric(H1.Ind[[i]]$SAR))
        SAR[i,"H4"] <- last(as.numeric(H4.Ind[[i]]$SAR))
        SAR[i,"D"] <- last(as.numeric(D.Ind[[i]]$SAR))
        SAR[i,"W"] <- last(as.numeric(W.Ind[[i]]$SAR))
}

# %BB

BB <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

for (i in 1:nrow(data.loc)) {
        BB[i,"Ticker"] <- data.loc$Ticker[i]
        BB[i,"M05"] <- last(as.numeric(M05.Ind[[i]]$BB$pctB))
        BB[i,"M15"] <- last(as.numeric(M15.Ind[[i]]$BB$pctB))
        BB[i,"H1"] <- last(as.numeric(H1.Ind[[i]]$BB$pctB))
        BB[i,"H4"] <- last(as.numeric(H4.Ind[[i]]$BB$pctB))
        BB[i,"D"] <- last(as.numeric(D.Ind[[i]]$BB$pctB))
        BB[i,"W"] <- last(as.numeric(W.Ind[[i]]$BB$pctB))
}

BB.MM <- BB[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),BB$Ticker),]
BB.MM$Ticker <- SpecsMini$Ticker
BB.MM <- rbind(BB,BB.MM)
write.csv(BB.MM,"./data/Stooq/results/BB.csv")

#### DS Trade Update ####

CL <- data.frame(
        Ticker = character(),
        Close = numeric(),
        Delta = numeric(),
        T1 = character(),
        T0 = character(),
        stringsAsFactors = FALSE
)

for (i in 1:nrow(data.loc)) {
        CL[i,"Ticker"] <- data.loc$Ticker[i]
        CL[i,"Close"] <- as.numeric(last(data.M05.xts[[i]]$Close))
        CL[i,"Delta"] <- as.numeric(last(data.M05.xts[[i]]$Close))/as.numeric(data.FiveMins.xts[[i]]$Close[nrow(data.FiveMins.xts[[i]])])-1
        CL[i,"T1"] <- as.character(index(last(data.M05.xts[[i]]$Close)))
        CL[i,"T0"] <- as.character(index(last(data.FiveMins.xts[[i]]$Close)))
}

# for (i in 1:nrow(data.loc)) {
#         CL[i,"Ticker"] <- data.loc$Ticker[i]
#         CL[i,"Close"] <- as.numeric(last(data.M05.xts[[i]]$Close))
#         CL[i,"Delta"] <- as.numeric(last(data.M05.xts[[i]]$Close))/as.numeric(data.D.xts[[i]]$Close[nrow(data.D.xts[[i]])-1])-1
#         CL[i,"T1"] <- as.character(index(last(data.M05.xts[[i]]$Close)))
#         CL[i,"T0"] <- as.character(index(last(data.Daily.xts[[i]]$Close)))
# }

for (i in 1:nrow(Specs)){
        Specs$Close[i] <- CL[CL$Ticker==Specs$Ticker[i],]$Close
        
}

for (i in 1:nrow(SpecsMini)){
        SpecsMini$Close[i] <- CL[CL$Ticker==gsub("m", "", SpecsMini$Ticker[i], fixed = TRUE),]$Close/10
        
}

DS <- rbind(Specs,SpecsMini)
FX.USD <- DS[(substr(DS$Ticker,4,6)=="USD" | substr(DS$Ticker,1,3)=="USD") & DS$Market=="FX", "Ticker"]

for (i in 1:nrow(DS)) {
        if (DS$CCY[i]=="USD") {
                DS$TV.USD[i] <- DS$LotSize[i]/DS$Pips[i]
        } else if (DS$CCY[i]!="USD" & sum(substr(FX.USD,4,6)==DS$CCY[i])==1) {
                DS$TV.USD[i] <- 1/DS$Close[DS$Ticker == FX.USD[substr(FX.USD,4,6)==DS$CCY[i]]]*DS$LotSize[i]/DS$Pips[i]
        } else if (DS$CCY[i]!="USD" & sum(substr(FX.USD,1,3)==DS$CCY[i])==1) {
                DS$TV.USD[i] <- DS$Close[DS$Ticker == FX.USD[substr(FX.USD,1,3)==DS$CCY[i]]]*DS$LotSize[i]/DS$Pips[i]
        }
}

DS$TV.RON <- DS$TV.USD * DS$Close[DS$Ticker=="USDRON"]
DS$VAL.RON <- DS$Close * DS$TV.RON * DS$Pips
DS$MG.RON <- DS$VAL.RON * DS$Margin
DS$SPR <- DS$Spread / DS$Pips / DS$Close
DS$Update <- (!(DS$Ticker %in% ticker.not.intraday))*1

write.csv(DS,"./data/Stooq/TickerList/DS.csv")
