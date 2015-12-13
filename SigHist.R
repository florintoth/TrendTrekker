#### Definitions ####

hist.tail <- 100
wind.adx <- 5
wind.bb <- 5
wind.sar <- 2

EMAs.Hist <- list()
MACDs.Hist <- list()
RSIs.Hist <- list()
ADXs.Hist <- list()
BBs.Hist <- list()
SARs.Hist <- list()

EMAs <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        EMA = numeric(),
        EMA.s = numeric(),
        EMA.l = numeric(),
        stringsAsFactors = FALSE
)

MACDs <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        MACD = numeric(),
        MACD.s = numeric(),
        MACD.l = numeric(),
        stringsAsFactors = FALSE
)

RSIs <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        RSI = numeric(),
        RSI.s = numeric(),
        RSI.l = numeric(),
        stringsAsFactors = FALSE
)

ADXs <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        ADX = numeric(),
        ADX.s = numeric(),
        ADX.l = numeric(),
        stringsAsFactors = FALSE
)

BBs <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        BB = numeric(),
        BB.s = numeric(),
        BB.l = numeric(),
        stringsAsFactors = FALSE
)

SARs <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        SAR = numeric(),
        SAR.s = numeric(),
        SAR.l = numeric(),
        stringsAsFactors = FALSE
)


EMAs.ch <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

MACDs.ch <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

RSIs.ch <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

ADXs.ch <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

BBs.ch <- data.frame(
        Ticker = character(),
        M05 = numeric(),
        M15 = numeric(),
        H1 = numeric(),
        H4 = numeric(),
        D = numeric(),
        W = numeric(),
        stringsAsFactors = FALSE
)

SARs.ch <- data.frame(
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
        # Check EMAnr1, EMAnr2 & EMAnr3 ####
        list.name <- data.loc$Ticker[i]
        EMAs[i,"Ticker"] <- data.loc$Ticker[i]
        EMAs.ch[i,"Ticker"] <- data.loc$Ticker[i]
        
        # M05
        temp <- as.numeric(((M05.Ind[[i]]$EMAnr1 > M05.Ind[[i]]$EMAnr2) & (M05.Ind[[i]]$EMAnr2 > M05.Ind[[i]]$EMAnr3))*1) +
                as.numeric(((M05.Ind[[i]]$EMAnr1 < M05.Ind[[i]]$EMAnr2) & (M05.Ind[[i]]$EMAnr2 < M05.Ind[[i]]$EMAnr3))*-1)
        tmp.M05 <- data.frame(M05=tail(temp[!is.na(temp)], hist.tail))
        # M15
        temp <- as.numeric(((M15.Ind[[i]]$EMAnr1 > M15.Ind[[i]]$EMAnr2) & (M15.Ind[[i]]$EMAnr2 > M15.Ind[[i]]$EMAnr3))*1) +
                as.numeric(((M15.Ind[[i]]$EMAnr1 < M15.Ind[[i]]$EMAnr2) & (M15.Ind[[i]]$EMAnr2 < M15.Ind[[i]]$EMAnr3))*-1)
        tmp.M15 <- data.frame(M15=tail(temp[!is.na(temp)], hist.tail))
        # H1
        temp <- as.numeric(((H1.Ind[[i]]$EMAnr1 > H1.Ind[[i]]$EMAnr2) & (H1.Ind[[i]]$EMAnr2 > H1.Ind[[i]]$EMAnr3))*1) +
                as.numeric(((H1.Ind[[i]]$EMAnr1 < H1.Ind[[i]]$EMAnr2) & (H1.Ind[[i]]$EMAnr2 < H1.Ind[[i]]$EMAnr3))*-1)
        tmp.H1 <- data.frame(H1=tail(temp[!is.na(temp)], hist.tail))
        # H4
        temp <- as.numeric(((H4.Ind[[i]]$EMAnr1 > H4.Ind[[i]]$EMAnr2) & (H4.Ind[[i]]$EMAnr2 > H4.Ind[[i]]$EMAnr3))*1) +
                as.numeric(((H4.Ind[[i]]$EMAnr1 < H4.Ind[[i]]$EMAnr2) & (H4.Ind[[i]]$EMAnr2 < H4.Ind[[i]]$EMAnr3))*-1)
        tmp.H4 <- data.frame(H4=tail(temp[!is.na(temp)], hist.tail))
        # Daily
        temp <- as.numeric(((D.Ind[[i]]$EMAnr1 > D.Ind[[i]]$EMAnr2) & (D.Ind[[i]]$EMAnr2 > D.Ind[[i]]$EMAnr3))*1) +
                as.numeric(((D.Ind[[i]]$EMAnr1 < D.Ind[[i]]$EMAnr2) & (D.Ind[[i]]$EMAnr2 < D.Ind[[i]]$EMAnr3))*-1)
        tmp.D <- data.frame(D=tail(temp[!is.na(temp)], hist.tail))
        # Weekly
        temp <- as.numeric(((W.Ind[[i]]$EMAnr1 > W.Ind[[i]]$EMAnr2) & (W.Ind[[i]]$EMAnr2 > W.Ind[[i]]$EMAnr3))*1) +
                as.numeric(((W.Ind[[i]]$EMAnr1 < W.Ind[[i]]$EMAnr2) & (W.Ind[[i]]$EMAnr2 < W.Ind[[i]]$EMAnr3))*-1)
        tmp.W <- data.frame(W=tail(temp[!is.na(temp)], hist.tail))
        
        tmp.EMA <- tmp.M05 + tmp.M15 + tmp.H1 + tmp.H4 + tmp.D + tmp.W
        names(tmp.EMA) <- "EMA"
        tmp.EMA.s <- tmp.M05 + tmp.M15 + tmp.H1
        names(tmp.EMA.s) <- "EMA.s"
        tmp.EMA.l <- tmp.H4 + tmp.D + tmp.W
        names(tmp.EMA.l) <- "EMA.l"
        EMAs.Hist[[list.name]] <- cbind.data.frame(tmp.M05,tmp.M15,tmp.H1,tmp.H4,tmp.D,tmp.W,tmp.EMA,tmp.EMA.s,tmp.EMA.l)
        EMAs[i,c(2:10)] <- last(as.data.frame(EMAs.Hist[[list.name]]))
        
        Pos.M05 <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$M05), EMAs.Hist[[list.name]]$M05, right=T)
        Pos.M15 <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$M15), EMAs.Hist[[list.name]]$M15, right=T)
        Pos.H1 <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$H1), EMAs.Hist[[list.name]]$H1, right=T)
        Pos.H4 <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$H4), EMAs.Hist[[list.name]]$H4, right=T)
        Pos.D <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$D), EMAs.Hist[[list.name]]$D, right=T)
        Pos.W <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$W), EMAs.Hist[[list.name]]$W, right=T)
        
        if (last(EMAs.Hist[[list.name]]$EMA) == 0) {Pos.EMA <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$EMA), EMAs.Hist[[list.name]]$EMA, right=T)}
        else { Pos.EMA <- Position(function(x) sign(x)==-sign(last(EMAs.Hist[[list.name]]$EMA)), EMAs.Hist[[list.name]]$EMA, right=T)}
        if (last(EMAs.Hist[[list.name]]$EMA.s) == 0) {Pos.EMA.s <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$EMA.s), EMAs.Hist[[list.name]]$EMA.s, right=T)}
        else {Pos.EMA.s <- Position(function(x) sign(x)==-sign(last(EMAs.Hist[[list.name]]$EMA.s)), EMAs.Hist[[list.name]]$EMA.s, right=T)}
        if (last(EMAs.Hist[[list.name]]$EMA.l) == 0) {Pos.EMA.l <- Position(function(x) x!=last(EMAs.Hist[[list.name]]$EMA.l), EMAs.Hist[[list.name]]$EMA.l, right=T)}
        else {Pos.EMA.l <- Position(function(x) sign(x)==-sign(last(EMAs.Hist[[list.name]]$EMA.l)), EMAs.Hist[[list.name]]$EMA.l, right=T)}

        if (is.na(Pos.M05)) { Pos.M05 <- 0 }
        if (is.na(Pos.M15)) { Pos.M15 <- 0 }
        if (is.na(Pos.H1)) { Pos.H1 <- 0 }
        if (is.na(Pos.H4)) { Pos.H4 <- 0 }
        if (is.na(Pos.D)) { Pos.D <- 0 }
        if (is.na(Pos.W)) { Pos.W <- 0 }
        if (is.na(Pos.EMA)) { Pos.EMA <- 0 }
        if (is.na(Pos.EMA.s)) { Pos.EMA.s <- 0 }
        if (is.na(Pos.EMA.l)) { Pos.EMA.l <- 0 }
        
        EMAs.ch[i,"M05"] <- hist.tail - Pos.M05
        EMAs.ch[i,"M15"] <- hist.tail - Pos.M15
        EMAs.ch[i,"H1"] <- hist.tail - Pos.H1
        EMAs.ch[i,"H4"] <- hist.tail - Pos.H4
        EMAs.ch[i,"D"] <- hist.tail - Pos.D
        EMAs.ch[i,"W"] <- hist.tail - Pos.W
        if (Pos.EMA!=0) {
                EMAs.ch[i,"EMA"] <- (hist.tail - Pos.EMA) * sign(EMAs.Hist[[list.name]]$EMA[Pos.EMA])
        } else {EMAs.ch[i,"EMA"] <- (hist.tail - Pos.EMA) + hist.tail}
        if (Pos.EMA.s!=0) {
                EMAs.ch[i,"EMA.s"] <- (hist.tail - Pos.EMA.s) * sign(EMAs.Hist[[list.name]]$EMA.s[Pos.EMA.s])
        } else {EMAs.ch[i,"EMA.s"] <- (hist.tail - Pos.EMA.s) + hist.tail}
        if (Pos.EMA.l!=0) {
                EMAs.ch[i,"EMA.l"] <- (hist.tail - Pos.EMA.l) * sign(EMAs.Hist[[list.name]]$EMA.l[Pos.EMA.l])
        } else {EMAs.ch[i,"EMA.l"] <- (hist.tail - Pos.EMA.l) + hist.tail}

        # Check MACD ####
        MACDs[i,"Ticker"] <- data.loc$Ticker[i]
        MACDs.ch[i,"Ticker"] <- data.loc$Ticker[i]
        
        # M05
        temp <- as.numeric(((M05.Ind[[i]]$MACD$macd > 0) & (M05.Ind[[i]]$MACD$macd > M05.Ind[[i]]$MACD$signal))*1) +
                as.numeric(((M05.Ind[[i]]$MACD$macd < 0) & (M05.Ind[[i]]$MACD$macd < M05.Ind[[i]]$MACD$signal))*-1)
        tmp.M05 <- data.frame(M05=tail(temp[!is.na(temp)], hist.tail))
        # M15
        temp <- as.numeric(((M15.Ind[[i]]$MACD$macd > 0) & (M15.Ind[[i]]$MACD$macd > M15.Ind[[i]]$MACD$signal))*1) +
                as.numeric(((M15.Ind[[i]]$MACD$macd < 0) & (M15.Ind[[i]]$MACD$macd < M15.Ind[[i]]$MACD$signal))*-1)
        tmp.M15 <- data.frame(M15=tail(temp[!is.na(temp)], hist.tail))
        # H1
        temp <- as.numeric(((H1.Ind[[i]]$MACD$macd > 0) & (H1.Ind[[i]]$MACD$macd > H1.Ind[[i]]$MACD$signal))*1) +
                as.numeric(((H1.Ind[[i]]$MACD$macd < 0) & (H1.Ind[[i]]$MACD$macd < H1.Ind[[i]]$MACD$signal))*-1)
        tmp.H1 <- data.frame(H1=tail(temp[!is.na(temp)], hist.tail))
        # H4
        temp <- as.numeric(((H4.Ind[[i]]$MACD$macd > 0) & (H4.Ind[[i]]$MACD$macd > H4.Ind[[i]]$MACD$signal))*1) +
                as.numeric(((H4.Ind[[i]]$MACD$macd < 0) & (H4.Ind[[i]]$MACD$macd < H4.Ind[[i]]$MACD$signal))*-1)
        tmp.H4 <- data.frame(H4=tail(temp[!is.na(temp)], hist.tail))
        # Daily
        temp <- as.numeric(((D.Ind[[i]]$MACD$macd > 0) & (D.Ind[[i]]$MACD$macd > D.Ind[[i]]$MACD$signal))*1) +
                as.numeric(((D.Ind[[i]]$MACD$macd < 0) & (D.Ind[[i]]$MACD$macd < D.Ind[[i]]$MACD$signal))*-1)
        tmp.D <- data.frame(D=tail(temp[!is.na(temp)], hist.tail))
        # Weekly
        temp <- as.numeric(((W.Ind[[i]]$MACD$macd > 0) & (W.Ind[[i]]$MACD$macd > W.Ind[[i]]$MACD$signal))*1) +
                as.numeric(((W.Ind[[i]]$MACD$macd < 0) & (W.Ind[[i]]$MACD$macd < W.Ind[[i]]$MACD$signal))*-1)
        tmp.W <- data.frame(W=tail(temp[!is.na(temp)], hist.tail))
        
        tmp.MACD <- tmp.M05 + tmp.M15 + tmp.H1 + tmp.H4 + tmp.D + tmp.W
        names(tmp.MACD) <- "MACD"
        tmp.MACD.s <- tmp.M05 + tmp.M15 + tmp.H1
        names(tmp.MACD.s) <- "MACD.s"
        tmp.MACD.l <- tmp.H4 + tmp.D + tmp.W
        names(tmp.MACD.l) <- "MACD.l"
        MACDs.Hist[[list.name]] <- cbind.data.frame(tmp.M05,tmp.M15,tmp.H1,tmp.H4,tmp.D,tmp.W,tmp.MACD,tmp.MACD.s,tmp.MACD.l)
        MACDs[i,c(2:10)] <- last(as.data.frame(MACDs.Hist[[list.name]]))

        Pos.M05 <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$M05), MACDs.Hist[[list.name]]$M05, right=T)
        Pos.M15 <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$M15), MACDs.Hist[[list.name]]$M15, right=T)
        Pos.H1 <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$H1), MACDs.Hist[[list.name]]$H1, right=T)
        Pos.H4 <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$H4), MACDs.Hist[[list.name]]$H4, right=T)
        Pos.D <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$D), MACDs.Hist[[list.name]]$D, right=T)
        Pos.W <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$W), MACDs.Hist[[list.name]]$W, right=T)
        if (last(MACDs.Hist[[list.name]]$MACD) == 0) {Pos.MACD <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$MACD), MACDs.Hist[[list.name]]$MACD, right=T)}
        else { Pos.MACD <- Position(function(x) sign(x)==-sign(last(MACDs.Hist[[list.name]]$MACD)), MACDs.Hist[[list.name]]$MACD, right=T)}
        if (last(MACDs.Hist[[list.name]]$MACD.s) == 0) {Pos.MACD.s <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$MACD.s), MACDs.Hist[[list.name]]$MACD.s, right=T)}
        else {Pos.MACD.s <- Position(function(x) sign(x)==-sign(last(MACDs.Hist[[list.name]]$MACD.s)), MACDs.Hist[[list.name]]$MACD.s, right=T)}
        if (last(MACDs.Hist[[list.name]]$MACD.l) == 0) {Pos.MACD.l <- Position(function(x) x!=last(MACDs.Hist[[list.name]]$MACD.l), MACDs.Hist[[list.name]]$MACD.l, right=T)}
        else {Pos.MACD.l <- Position(function(x) sign(x)==-sign(last(MACDs.Hist[[list.name]]$MACD.l)), MACDs.Hist[[list.name]]$MACD.l, right=T)}

        if (is.na(Pos.M05)) { Pos.M05 <- 0 }
        if (is.na(Pos.M15)) { Pos.M15 <- 0 }
        if (is.na(Pos.H1)) { Pos.H1 <- 0 }
        if (is.na(Pos.H4)) { Pos.H4 <- 0 }
        if (is.na(Pos.D)) { Pos.D <- 0 }
        if (is.na(Pos.W)) { Pos.W <- 0 }
        if (is.na(Pos.MACD)) { Pos.MACD <- 0 }
        if (is.na(Pos.MACD.s)) { Pos.MACD.s <- 0 }
        if (is.na(Pos.MACD.l)) { Pos.MACD.l <- 0 }
        
        MACDs.ch[i,"M05"] <- hist.tail - Pos.M05
        MACDs.ch[i,"M15"] <- hist.tail - Pos.M15
        MACDs.ch[i,"H1"] <- hist.tail - Pos.H1
        MACDs.ch[i,"H4"] <- hist.tail - Pos.H4
        MACDs.ch[i,"D"] <- hist.tail - Pos.D
        MACDs.ch[i,"W"] <- hist.tail - Pos.W
        if (Pos.MACD!=0) {
                MACDs.ch[i,"MACD"] <- (hist.tail - Pos.MACD) * sign(MACDs.Hist[[list.name]]$MACD[Pos.MACD])
        } else {MACDs.ch[i,"MACD"] <- (hist.tail - Pos.MACD) + hist.tail}
        if (Pos.MACD.s!=0) {
                MACDs.ch[i,"MACD.s"] <- (hist.tail - Pos.MACD.s) * sign(MACDs.Hist[[list.name]]$MACD.s[Pos.MACD.s])
        } else {MACDs.ch[i,"MACD.s"] <- (hist.tail - Pos.MACD.s) + hist.tail}
        if (Pos.MACD.l!=0) {
                MACDs.ch[i,"MACD.l"] <- (hist.tail - Pos.MACD.l) * sign(MACDs.Hist[[list.name]]$MACD.l[Pos.MACD.l])
        } else {MACDs.ch[i,"MACD.l"] <- (hist.tail - Pos.MACD.l) + hist.tail}

        # Check RSI ####
        RSIs[i,"Ticker"] <- data.loc$Ticker[i]
        RSIs.ch[i,"Ticker"] <- data.loc$Ticker[i]
        
        # M05
        temp <- as.numeric(((M05.Ind[[i]]$RSI > 60))*1) + as.numeric(((M05.Ind[[i]]$RSI < 40))*-1)
        tmp.M05 <- data.frame(M05=tail(temp[!is.na(temp)], hist.tail))
        # M15
        temp <- as.numeric(((M15.Ind[[i]]$RSI > 60))*1) + as.numeric(((M15.Ind[[i]]$RSI < 40))*-1)
        tmp.M15 <- data.frame(M15=tail(temp[!is.na(temp)], hist.tail))
        # H1
        temp <- as.numeric(((H1.Ind[[i]]$RSI > 60))*1) + as.numeric(((H1.Ind[[i]]$RSI < 40))*-1)
        tmp.H1 <- data.frame(H1=tail(temp[!is.na(temp)], hist.tail))
        # H4
        temp <- as.numeric(((H4.Ind[[i]]$RSI > 60))*1) + as.numeric(((H4.Ind[[i]]$RSI < 40))*-1)
        tmp.H4 <- data.frame(H4=tail(temp[!is.na(temp)], hist.tail))
        # Daily
        temp <- as.numeric(((D.Ind[[i]]$RSI > 60))*1) + as.numeric(((D.Ind[[i]]$RSI < 40))*-1)
        tmp.D <- data.frame(D=tail(temp[!is.na(temp)], hist.tail))
        # Weekly
        temp <- as.numeric(((W.Ind[[i]]$RSI > 60))*1) + as.numeric(((W.Ind[[i]]$RSI < 40))*-1)
        tmp.W <- data.frame(W=tail(temp[!is.na(temp)], hist.tail))
  
        tmp.RSI <- tmp.M05 + tmp.M15 + tmp.H1 + tmp.H4 + tmp.D + tmp.W
        names(tmp.RSI) <- "RSI"
        tmp.RSI.s <- tmp.M05 + tmp.M15 + tmp.H1
        names(tmp.RSI.s) <- "RSI.s"
        tmp.RSI.l <- tmp.H4 + tmp.D + tmp.W
        names(tmp.RSI.l) <- "RSI.l"
        RSIs.Hist[[list.name]] <- cbind.data.frame(tmp.M05,tmp.M15,tmp.H1,tmp.H4,tmp.D,tmp.W,tmp.RSI,tmp.RSI.s,tmp.RSI.l)
        RSIs[i,c(2:10)] <- last(as.data.frame(RSIs.Hist[[list.name]]))

        Pos.M05 <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$M05), RSIs.Hist[[list.name]]$M05, right=T)
        Pos.M15 <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$M15), RSIs.Hist[[list.name]]$M15, right=T)
        Pos.H1 <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$H1), RSIs.Hist[[list.name]]$H1, right=T)
        Pos.H4 <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$H4), RSIs.Hist[[list.name]]$H4, right=T)
        Pos.D <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$D), RSIs.Hist[[list.name]]$D, right=T)
        Pos.W <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$W), RSIs.Hist[[list.name]]$W, right=T)
        if (last(RSIs.Hist[[list.name]]$RSI) == 0) {Pos.RSI <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$RSI), RSIs.Hist[[list.name]]$RSI, right=T)}
        else { Pos.RSI <- Position(function(x) sign(x)==-sign(last(RSIs.Hist[[list.name]]$RSI)), RSIs.Hist[[list.name]]$RSI, right=T)}
        if (last(RSIs.Hist[[list.name]]$RSI.s) == 0) {Pos.RSI.s <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$RSI.s), RSIs.Hist[[list.name]]$RSI.s, right=T)}
        else {Pos.RSI.s <- Position(function(x) sign(x)==-sign(last(RSIs.Hist[[list.name]]$RSI.s)), RSIs.Hist[[list.name]]$RSI.s, right=T)}
        if (last(RSIs.Hist[[list.name]]$RSI.l) == 0) {Pos.RSI.l <- Position(function(x) x!=last(RSIs.Hist[[list.name]]$RSI.l), RSIs.Hist[[list.name]]$RSI.l, right=T)}
        else {Pos.RSI.l <- Position(function(x) sign(x)==-sign(last(RSIs.Hist[[list.name]]$RSI.l)), RSIs.Hist[[list.name]]$RSI.l, right=T)}

        if (is.na(Pos.M05)) { Pos.M05 <- 0 }
        if (is.na(Pos.M15)) { Pos.M15 <- 0 }
        if (is.na(Pos.H1)) { Pos.H1 <- 0 }
        if (is.na(Pos.H4)) { Pos.H4 <- 0 }
        if (is.na(Pos.D)) { Pos.D <- 0 }
        if (is.na(Pos.W)) { Pos.W <- 0 }
        if (is.na(Pos.RSI)) { Pos.RSI <- 0 }
        if (is.na(Pos.RSI.s)) { Pos.RSI.s <- 0 }
        if (is.na(Pos.RSI.l)) { Pos.RSI.l <- 0 }
        
        if (Pos.M05!=0) {
                if (RSIs.Hist[[list.name]]$M05[Pos.M05]==0){
                        RSIs.ch[i,"M05"] <- (hist.tail - Pos.M05) + hist.tail        
                } else {
                        RSIs.ch[i,"M05"] <- (hist.tail - Pos.M05) * RSIs.Hist[[list.name]]$M05[Pos.M05]
                }
        } else RSIs.ch[i,"M05"] <- (hist.tail - Pos.M05) + hist.tail
        if (Pos.M15!=0) {
                if (RSIs.Hist[[list.name]]$M15[Pos.M15]==0){
                        RSIs.ch[i,"M15"] <- (hist.tail - Pos.M15) + hist.tail        
                } else {
                        RSIs.ch[i,"M15"] <- (hist.tail - Pos.M15) * RSIs.Hist[[list.name]]$M15[Pos.M15]
                }
        } else RSIs.ch[i,"M15"] <- (hist.tail - Pos.M15) + hist.tail
        if (Pos.H1!=0) {
                if (RSIs.Hist[[list.name]]$H1[Pos.H1]==0){
                        RSIs.ch[i,"H1"] <- (hist.tail - Pos.H1) + hist.tail        
                } else {
                        RSIs.ch[i,"H1"] <- (hist.tail - Pos.H1) * RSIs.Hist[[list.name]]$H1[Pos.H1]
                }
        } else RSIs.ch[i,"H1"] <- (hist.tail - Pos.H1) + hist.tail
        if (Pos.H4!=0) {
                if (RSIs.Hist[[list.name]]$H4[Pos.H4]==0){
                        RSIs.ch[i,"H4"] <- (hist.tail - Pos.H4) + hist.tail        
                } else {
                        RSIs.ch[i,"H4"] <- (hist.tail - Pos.H4) * RSIs.Hist[[list.name]]$H4[Pos.H4]
                }
        } else RSIs.ch[i,"H4"] <- (hist.tail - Pos.H4) + hist.tail
        if (Pos.D!=0) {
                if (RSIs.Hist[[list.name]]$D[Pos.D]==0){
                        RSIs.ch[i,"D"] <- (hist.tail - Pos.D) + hist.tail        
                } else {
                        RSIs.ch[i,"D"] <- (hist.tail - Pos.D) * RSIs.Hist[[list.name]]$D[Pos.D]
                }
        } else RSIs.ch[i,"D"] <- (hist.tail - Pos.D) + hist.tail
        if (Pos.W!=0) {
                if (RSIs.Hist[[list.name]]$W[Pos.W]==0){
                        RSIs.ch[i,"W"] <- (hist.tail - Pos.W) + hist.tail        
                } else {
                        RSIs.ch[i,"W"] <- (hist.tail - Pos.W) * RSIs.Hist[[list.name]]$W[Pos.W]
                }
        } else RSIs.ch[i,"W"] <- (hist.tail - Pos.W) + hist.tail
        if (Pos.RSI!=0) {
                RSIs.ch[i,"RSI"] <- (hist.tail - Pos.RSI) * sign(RSIs.Hist[[list.name]]$RSI[Pos.RSI])
        } else {RSIs.ch[i,"RSI"] <- (hist.tail - Pos.RSI) + hist.tail}
        if (Pos.RSI.s!=0) {
                RSIs.ch[i,"RSI.s"] <- (hist.tail - Pos.RSI.s) * sign(RSIs.Hist[[list.name]]$RSI.s[Pos.RSI.s])
        } else {RSIs.ch[i,"RSI.s"] <- (hist.tail - Pos.RSI.s) + hist.tail}
        if (Pos.RSI.l!=0) {
                RSIs.ch[i,"RSI.l"] <- (hist.tail - Pos.RSI.l) * sign(RSIs.Hist[[list.name]]$RSI.l[Pos.RSI.l])
        } else {RSIs.ch[i,"RSI.l"] <- (hist.tail - Pos.RSI.l) + hist.tail}

        # Check ADX ####
        ADXs[i,"Ticker"] <- data.loc$Ticker[i]
        ADXs.ch[i,"Ticker"] <- data.loc$Ticker[i]

        # M05
        temp <- as.numeric(((M05.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                (as.numeric(M05.Ind[[i]]$ADX$DIp) > as.numeric(M05.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*1) +
                        as.numeric(((M05.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                        sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                        (as.numeric(M05.Ind[[i]]$ADX$DIp) < as.numeric(M05.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*-1)
        tmp.M05 <- data.frame(M05=tail(temp[!is.na(temp)], hist.tail))
        # M15
        temp <- as.numeric(((M15.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                sign(rollapply(as.numeric(ROC(M15.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                (as.numeric(M15.Ind[[i]]$ADX$DIp) > as.numeric(M15.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*1) +
                        as.numeric(((M15.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                        sign(rollapply(as.numeric(ROC(M15.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                        (as.numeric(M15.Ind[[i]]$ADX$DIp) < as.numeric(M15.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*-1)
        tmp.M15 <- data.frame(M15=tail(temp[!is.na(temp)], hist.tail))
        # H1
        temp <- as.numeric(((H1.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                sign(rollapply(as.numeric(ROC(H1.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                (as.numeric(H1.Ind[[i]]$ADX$DIp) > as.numeric(H1.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*1) +
                        as.numeric(((H1.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                        sign(rollapply(as.numeric(ROC(H1.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                        (as.numeric(H1.Ind[[i]]$ADX$DIp) < as.numeric(H1.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*-1)
        tmp.H1 <- data.frame(H1=tail(temp[!is.na(temp)], hist.tail))
        # H4
        temp <- as.numeric(((H4.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                sign(rollapply(as.numeric(ROC(H4.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                (as.numeric(H4.Ind[[i]]$ADX$DIp) > as.numeric(H4.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*1) +
                        as.numeric(((H4.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                        sign(rollapply(as.numeric(ROC(H4.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==-1 &
                        (as.numeric(H4.Ind[[i]]$ADX$DIp) < as.numeric(H4.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*-1)
        tmp.H4 <- data.frame(H4=tail(temp[!is.na(temp)], hist.tail))
        # Daily
        temp <- as.numeric(((D.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                sign(rollapply(as.numeric(ROC(D.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                (as.numeric(D.Ind[[i]]$ADX$DIp) > as.numeric(D.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*1) +
                        as.numeric(((D.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                        sign(rollapply(as.numeric(ROC(D.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                        (as.numeric(D.Ind[[i]]$ADX$DIp) < as.numeric(D.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*-1)
        tmp.D <- data.frame(D=tail(temp[!is.na(temp)], hist.tail))
        # Weekly
        temp <- as.numeric(((W.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                sign(rollapply(as.numeric(ROC(W.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                (as.numeric(W.Ind[[i]]$ADX$DIp) > as.numeric(W.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*1) +
                        as.numeric(((W.Ind[[i]]$ADX$ADX > 20)[-rep(1:(wind.adx-1))] &  
                        sign(rollapply(as.numeric(ROC(W.Ind[[i]]$ADX$ADX)), wind.adx, sum, na.rm=TRUE))==1 &
                        (as.numeric(W.Ind[[i]]$ADX$DIp) < as.numeric(W.Ind[[i]]$ADX$DIn))[-rep(1:(wind.adx-1))])*-1)
        tmp.W <- data.frame(W=tail(temp[!is.na(temp)], hist.tail))
        
        tmp.ADX <- tmp.M05 + tmp.M15 + tmp.H1 + tmp.H4 + tmp.D + tmp.W
        names(tmp.ADX) <- "ADX"
        tmp.ADX.s <- tmp.M05 + tmp.M15 + tmp.H1
        names(tmp.ADX.s) <- "ADX.s"
        tmp.ADX.l <- tmp.H4 + tmp.D + tmp.W
        names(tmp.ADX.l) <- "ADX.l"
        ADXs.Hist[[list.name]] <- cbind.data.frame(tmp.M05,tmp.M15,tmp.H1,tmp.H4,tmp.D,tmp.W,tmp.ADX,tmp.ADX.s,tmp.ADX.l)
        ADXs[i,c(2:10)] <- last(as.data.frame(ADXs.Hist[[list.name]]))

        Pos.M05 <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$M05), ADXs.Hist[[list.name]]$M05, right=T)
        Pos.M15 <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$M15), ADXs.Hist[[list.name]]$M15, right=T)
        Pos.H1 <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$H1), ADXs.Hist[[list.name]]$H1, right=T)
        Pos.H4 <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$H4), ADXs.Hist[[list.name]]$H4, right=T)
        Pos.D <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$D), ADXs.Hist[[list.name]]$D, right=T)
        Pos.W <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$W), ADXs.Hist[[list.name]]$W, right=T)

        if (last(ADXs.Hist[[list.name]]$ADX) == 0) {Pos.ADX <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$ADX), ADXs.Hist[[list.name]]$ADX, right=T)}
        else { Pos.ADX <- Position(function(x) sign(x)==-sign(last(ADXs.Hist[[list.name]]$ADX)), ADXs.Hist[[list.name]]$ADX, right=T)}
        if (last(ADXs.Hist[[list.name]]$ADX.s) == 0) {Pos.ADX.s <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$ADX.s), ADXs.Hist[[list.name]]$ADX.s, right=T)}
        else {Pos.ADX.s <- Position(function(x) sign(x)==-sign(last(ADXs.Hist[[list.name]]$ADX.s)), ADXs.Hist[[list.name]]$ADX.s, right=T)}
        if (last(ADXs.Hist[[list.name]]$ADX.l) == 0) {Pos.ADX.l <- Position(function(x) x!=last(ADXs.Hist[[list.name]]$ADX.l), ADXs.Hist[[list.name]]$ADX.l, right=T)}
        else {Pos.ADX.l <- Position(function(x) sign(x)==-sign(last(ADXs.Hist[[list.name]]$ADX.l)), ADXs.Hist[[list.name]]$ADX.l, right=T)}

        if (is.na(Pos.M05)) { Pos.M05 <- 0 }
        if (is.na(Pos.M15)) { Pos.M15 <- 0 }
        if (is.na(Pos.H1)) { Pos.H1 <- 0 }
        if (is.na(Pos.H4)) { Pos.H4 <- 0 }
        if (is.na(Pos.D)) { Pos.D <- 0 }
        if (is.na(Pos.W)) { Pos.W <- 0 }
        if (is.na(Pos.ADX)) { Pos.ADX <- 0 }
        if (is.na(Pos.ADX.s)) { Pos.ADX.s <- 0 }
        if (is.na(Pos.ADX.l)) { Pos.ADX.l <- 0 }
        
        ADXs.ch[i,"M05"] <- hist.tail - Pos.M05
        ADXs.ch[i,"M15"] <- hist.tail - Pos.M15
        ADXs.ch[i,"H1"] <- hist.tail - Pos.H1
        ADXs.ch[i,"H4"] <- hist.tail - Pos.H4
        ADXs.ch[i,"D"] <- hist.tail - Pos.D
        ADXs.ch[i,"W"] <- hist.tail - Pos.W
        if (Pos.ADX!=0) {
                ADXs.ch[i,"ADX"] <- (hist.tail - Pos.ADX) * sign(ADXs.Hist[[list.name]]$ADX[Pos.ADX])
        } else {ADXs.ch[i,"ADX"] <- (hist.tail - Pos.ADX) + hist.tail}
        if (Pos.ADX.s!=0) {
                ADXs.ch[i,"ADX.s"] <- (hist.tail - Pos.ADX.s) * sign(ADXs.Hist[[list.name]]$ADX.s[Pos.ADX.s])
        } else {ADXs.ch[i,"ADX.s"] <- (hist.tail - Pos.ADX.s) + hist.tail}
        if (Pos.ADX.l!=0) {
                ADXs.ch[i,"ADX.l"] <- (hist.tail - Pos.ADX.l) * sign(ADXs.Hist[[list.name]]$ADX.l[Pos.ADX.l])
        } else {ADXs.ch[i,"ADX.l"] <- (hist.tail - Pos.ADX.l) + hist.tail}

        if (data.loc$Ticker[i]=="XAGUSD" | data.loc$Ticker[i]=="XAUUSD"){ADXs[i,"D"] <- 0}
        if (data.loc$Ticker[i]=="XAUUSD"){ADXs[i,"W"] <- 0}

        # Check BB ####
        BBs[i,"Ticker"] <- data.loc$Ticker[i]
        BBs.ch[i,"Ticker"] <- data.loc$Ticker[i]
         
        # M05
        temp <- (as.numeric(M05.Ind[[i]]$BB$pctB >= 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$BB$up)), wind.bb, sum, na.rm=TRUE))==1))*1 +
                (as.numeric(M05.Ind[[i]]$BB$pctB < 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$BB$dn)), wind.bb, sum, na.rm=TRUE))==-1))*-1
        tmp.M05 <- data.frame(M05=tail(temp[!is.na(temp)], hist.tail))
        # M15
        temp <- (as.numeric(M15.Ind[[i]]$BB$pctB >= 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(M15.Ind[[i]]$BB$up)), wind.bb, sum, na.rm=TRUE))==1))*1 +
                (as.numeric(M15.Ind[[i]]$BB$pctB < 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(M15.Ind[[i]]$BB$dn)), wind.bb, sum, na.rm=TRUE))==-1))*-1
        tmp.M15 <- data.frame(M15=tail(temp[!is.na(temp)], hist.tail))
        # H1
        temp <- (as.numeric(H1.Ind[[i]]$BB$pctB >= 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(H1.Ind[[i]]$BB$up)), wind.bb, sum, na.rm=TRUE))==1))*1 +
                (as.numeric(H1.Ind[[i]]$BB$pctB < 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(H1.Ind[[i]]$BB$dn)), wind.bb, sum, na.rm=TRUE))==-1))*-1
        tmp.H1 <- data.frame(H1=tail(temp[!is.na(temp)], hist.tail))
        # H4
        temp <- (as.numeric(H4.Ind[[i]]$BB$pctB >= 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(H4.Ind[[i]]$BB$up)), wind.bb, sum, na.rm=TRUE))==1))*1 +
                (as.numeric(H4.Ind[[i]]$BB$pctB < 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(H4.Ind[[i]]$BB$dn)), wind.bb, sum, na.rm=TRUE))==-1))*-1
        tmp.H4 <- data.frame(H4=tail(temp[!is.na(temp)], hist.tail))
        # Daily
        temp <- (as.numeric(D.Ind[[i]]$BB$pctB >= 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(D.Ind[[i]]$BB$up)), wind.bb, sum, na.rm=TRUE))==1))*1 +
                (as.numeric(D.Ind[[i]]$BB$pctB < 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(D.Ind[[i]]$BB$dn)), wind.bb, sum, na.rm=TRUE))==-1))*-1
        tmp.D <- data.frame(D=tail(temp[!is.na(temp)], hist.tail))
        # Weekly
        temp <- (as.numeric(W.Ind[[i]]$BB$pctB >= 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(W.Ind[[i]]$BB$up)), wind.bb, sum, na.rm=TRUE))==1))*1 +
                (as.numeric(W.Ind[[i]]$BB$pctB < 0.5)[-rep(1:(wind.bb-1))] & 
                         as.numeric(sign(rollapply(as.numeric(ROC(W.Ind[[i]]$BB$dn)), wind.bb, sum, na.rm=TRUE))==-1))*-1
        tmp.W <- data.frame(W=tail(temp[!is.na(temp)], hist.tail))
        
        tmp.BB <- tmp.M05 + tmp.M15 + tmp.H1 + tmp.H4 + tmp.D + tmp.W
        names(tmp.BB) <- "BB"
        tmp.BB.s <- tmp.M05 + tmp.M15 + tmp.H1
        names(tmp.BB.s) <- "BB.s"
        tmp.BB.l <- tmp.H4 + tmp.D + tmp.W
        names(tmp.BB.l) <- "BB.l"
        BBs.Hist[[list.name]] <- cbind.data.frame(tmp.M05,tmp.M15,tmp.H1,tmp.H4,tmp.D,tmp.W,tmp.BB,tmp.BB.s,tmp.BB.l)
        BBs[i,c(2:10)] <- last(as.data.frame(BBs.Hist[[list.name]]))

        Pos.M05 <- Position(function(x) x!=last(BBs.Hist[[list.name]]$M05), BBs.Hist[[list.name]]$M05, right=T)
        Pos.M15 <- Position(function(x) x!=last(BBs.Hist[[list.name]]$M15), BBs.Hist[[list.name]]$M15, right=T)
        Pos.H1 <- Position(function(x) x!=last(BBs.Hist[[list.name]]$H1), BBs.Hist[[list.name]]$H1, right=T)
        Pos.H4 <- Position(function(x) x!=last(BBs.Hist[[list.name]]$H4), BBs.Hist[[list.name]]$H4, right=T)
        Pos.D <- Position(function(x) x!=last(BBs.Hist[[list.name]]$D), BBs.Hist[[list.name]]$D, right=T)
        Pos.W <- Position(function(x) x!=last(BBs.Hist[[list.name]]$W), BBs.Hist[[list.name]]$W, right=T)
        if (last(BBs.Hist[[list.name]]$BB) == 0) {Pos.BB <- Position(function(x) x!=last(BBs.Hist[[list.name]]$BB), BBs.Hist[[list.name]]$BB, right=T)}
        else { Pos.BB <- Position(function(x) sign(x)==-sign(last(BBs.Hist[[list.name]]$BB)), BBs.Hist[[list.name]]$BB, right=T)}
        if (last(BBs.Hist[[list.name]]$BB.s) == 0) {Pos.BB.s <- Position(function(x) x!=last(BBs.Hist[[list.name]]$BB.s), BBs.Hist[[list.name]]$BB.s, right=T)}
        else {Pos.BB.s <- Position(function(x) sign(x)==-sign(last(BBs.Hist[[list.name]]$BB.s)), BBs.Hist[[list.name]]$BB.s, right=T)}
        if (last(BBs.Hist[[list.name]]$BB.l) == 0) {Pos.BB.l <- Position(function(x) x!=last(BBs.Hist[[list.name]]$BB.l), BBs.Hist[[list.name]]$BB.l, right=T)}
        else {Pos.BB.l <- Position(function(x) sign(x)==-sign(last(BBs.Hist[[list.name]]$BB.l)), BBs.Hist[[list.name]]$BB.l, right=T)}

        if (is.na(Pos.M05)) { Pos.M05 <- 0 }
        if (is.na(Pos.M15)) { Pos.M15 <- 0 }
        if (is.na(Pos.H1)) { Pos.H1 <- 0 }
        if (is.na(Pos.H4)) { Pos.H4 <- 0 }
        if (is.na(Pos.D)) { Pos.D <- 0 }
        if (is.na(Pos.W)) { Pos.W <- 0 }
        if (is.na(Pos.BB)) { Pos.BB <- 0 }
        if (is.na(Pos.BB.s)) { Pos.BB.s <- 0 }
        if (is.na(Pos.BB.l)) { Pos.BB.l <- 0 }
        
        BBs.ch[i,"M05"] <- hist.tail - Pos.M05
        BBs.ch[i,"M15"] <- hist.tail - Pos.M15
        BBs.ch[i,"H1"] <- hist.tail - Pos.H1
        BBs.ch[i,"H4"] <- hist.tail - Pos.H4
        BBs.ch[i,"D"] <- hist.tail - Pos.D
        BBs.ch[i,"W"] <- hist.tail - Pos.W
        if (Pos.BB!=0) {
                BBs.ch[i,"BB"] <- (hist.tail - Pos.BB) * sign(BBs.Hist[[list.name]]$BB[Pos.BB])
        } else {BBs.ch[i,"BB"] <- (hist.tail - Pos.BB) + hist.tail}
        if (Pos.BB.s!=0) {
                BBs.ch[i,"BB.s"] <- (hist.tail - Pos.BB.s) * sign(BBs.Hist[[list.name]]$BB.s[Pos.BB.s])
        } else {BBs.ch[i,"BB.s"] <- (hist.tail - Pos.BB.s) + hist.tail}
        if (Pos.BB.l!=0) {
                BBs.ch[i,"BB.l"] <- (hist.tail - Pos.BB.l) * sign(BBs.Hist[[list.name]]$BB.l[Pos.BB.l])
        } else {BBs.ch[i,"BB.l"] <- (hist.tail - Pos.BB.l) + hist.tail}

        # Check SAR ####
        SARs[i,"Ticker"] <- data.loc$Ticker[i]
        SARs.ch[i,"Ticker"] <- data.loc$Ticker[i]
        
        # M05
        temp <- as.numeric(sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==1)*1 +
                as.numeric(sign(rollapply(as.numeric(ROC(M05.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==-1)*-1
        tmp.M05 <- data.frame(M05=tail(temp[!is.na(temp)], hist.tail))
        # M15
        temp <- as.numeric(sign(rollapply(as.numeric(ROC(M15.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==1)*1 +
                as.numeric(sign(rollapply(as.numeric(ROC(M15.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==-1)*-1
        tmp.M15 <- data.frame(M15=tail(temp[!is.na(temp)], hist.tail))
        # H1
        temp <- as.numeric(sign(rollapply(as.numeric(ROC(H1.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==1)*1 +
                as.numeric(sign(rollapply(as.numeric(ROC(H1.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==-1)*-1
        tmp.H1 <- data.frame(H1=tail(temp[!is.na(temp)], hist.tail))
        # H4
        temp <- as.numeric(sign(rollapply(as.numeric(ROC(H4.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==1)*1 +
                as.numeric(sign(rollapply(as.numeric(ROC(H4.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==-1)*-1
        tmp.H4 <- data.frame(H4=tail(temp[!is.na(temp)], hist.tail))
        # Daily
        temp <- as.numeric(sign(rollapply(as.numeric(ROC(D.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==1)*1 +
                as.numeric(sign(rollapply(as.numeric(ROC(D.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==-1)*-1
        tmp.D <- data.frame(D=tail(temp[!is.na(temp)], hist.tail))
        # Weekly
        temp <- as.numeric(sign(rollapply(as.numeric(ROC(W.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==1)*1 +
                as.numeric(sign(rollapply(as.numeric(ROC(W.Ind[[i]]$SAR)), wind.sar, sum, na.rm=TRUE))==-1)*-1
        tmp.W <- data.frame(W=tail(temp[!is.na(temp)], hist.tail))
        
        tmp.SAR <- tmp.M05 + tmp.M15 + tmp.H1 + tmp.H4 + tmp.D + tmp.W
        names(tmp.SAR) <- "SAR"
        tmp.SAR.s <- tmp.M05 + tmp.M15 + tmp.H1
        names(tmp.SAR.s) <- "SAR.s"
        tmp.SAR.l <- tmp.H4 + tmp.D + tmp.W
        names(tmp.SAR.l) <- "SAR.l"
        SARs.Hist[[list.name]] <- cbind.data.frame(tmp.M05,tmp.M15,tmp.H1,tmp.H4,tmp.D,tmp.W,tmp.SAR,tmp.SAR.s,tmp.SAR.l)
        SARs[i,c(2:10)] <- last(as.data.frame(SARs.Hist[[list.name]]))
        SARs.Hist[["EURUSD"]]
        Pos.M05 <- Position(function(x) x!=last(SARs.Hist[[list.name]]$M05), SARs.Hist[[list.name]]$M05, right=T)
        Pos.M15 <- Position(function(x) x!=last(SARs.Hist[[list.name]]$M15), SARs.Hist[[list.name]]$M15, right=T)
        Pos.H1 <- Position(function(x) x!=last(SARs.Hist[[list.name]]$H1), SARs.Hist[[list.name]]$H1, right=T)
        Pos.H4 <- Position(function(x) x!=last(SARs.Hist[[list.name]]$H4), SARs.Hist[[list.name]]$H4, right=T)
        Pos.D <- Position(function(x) x!=last(SARs.Hist[[list.name]]$D), SARs.Hist[[list.name]]$D, right=T)
        Pos.W <- Position(function(x) x!=last(SARs.Hist[[list.name]]$W), SARs.Hist[[list.name]]$W, right=T)
        if (last(SARs.Hist[[list.name]]$SAR) == 0) {Pos.SAR <- Position(function(x) x!=last(SARs.Hist[[list.name]]$SAR), SARs.Hist[[list.name]]$SAR, right=T)}
                else { Pos.SAR <- Position(function(x) sign(x)==-sign(last(SARs.Hist[[list.name]]$SAR)), SARs.Hist[[list.name]]$SAR, right=T)}
        if (last(SARs.Hist[[list.name]]$SAR.s) == 0) {Pos.SAR.s <- Position(function(x) x!=last(SARs.Hist[[list.name]]$SAR.s), SARs.Hist[[list.name]]$SAR.s, right=T)}
                else {Pos.SAR.s <- Position(function(x) sign(x)==-sign(last(SARs.Hist[[list.name]]$SAR.s)), SARs.Hist[[list.name]]$SAR.s, right=T)}
        if (last(SARs.Hist[[list.name]]$SAR.l) == 0) {Pos.SAR.l <- Position(function(x) x!=last(SARs.Hist[[list.name]]$SAR.l), SARs.Hist[[list.name]]$SAR.l, right=T)}
                else {Pos.SAR.l <- Position(function(x) sign(x)==-sign(last(SARs.Hist[[list.name]]$SAR.l)), SARs.Hist[[list.name]]$SAR.l, right=T)}
        
        if (is.na(Pos.M05)) { Pos.M05 <- 0 }
        if (is.na(Pos.M15)) { Pos.M15 <- 0 }
        if (is.na(Pos.H1)) { Pos.H1 <- 0 }
        if (is.na(Pos.H4)) { Pos.H4 <- 0 }
        if (is.na(Pos.D)) { Pos.D <- 0 }
        if (is.na(Pos.W)) { Pos.W <- 0 }
        if (is.na(Pos.SAR)) { Pos.SAR <- 0 }
        if (is.na(Pos.SAR.s)) { Pos.SAR.s <- 0 }
        if (is.na(Pos.SAR.l)) { Pos.SAR.l <- 0 }
        
        SARs.ch[i,"M05"] <- hist.tail - Pos.M05
        SARs.ch[i,"M15"] <- hist.tail - Pos.M15
        SARs.ch[i,"H1"] <- hist.tail - Pos.H1
        SARs.ch[i,"H4"] <- hist.tail - Pos.H4
        SARs.ch[i,"D"] <- hist.tail - Pos.D
        SARs.ch[i,"W"] <- hist.tail - Pos.W
        if (Pos.SAR!=0) {
                SARs.ch[i,"SAR"] <- (hist.tail - Pos.SAR) * sign(SARs.Hist[[list.name]]$SAR[Pos.SAR])
        } else {SARs.ch[i,"SAR"] <- (hist.tail - Pos.SAR) + hist.tail}
        if (Pos.SAR.s!=0) {
                SARs.ch[i,"SAR.s"] <- (hist.tail - Pos.SAR.s) * sign(SARs.Hist[[list.name]]$SAR.s[Pos.SAR.s])
        } else {SARs.ch[i,"SAR.s"] <- (hist.tail - Pos.SAR.s) + hist.tail}
        if (Pos.SAR.l!=0) {
                SARs.ch[i,"SAR.l"] <- (hist.tail - Pos.SAR.l) * sign(SARs.Hist[[list.name]]$SAR.l[Pos.SAR.l])
        } else {SARs.ch[i,"SAR.l"] <- (hist.tail - Pos.SAR.l) + hist.tail}
}

rm(temp, tmp.M05, tmp.M15, tmp.H1, tmp.H4, tmp.D, tmp.W, tmp.EMA,tmp.EMA.s,tmp.EMA.l,
   tmp.MACD,tmp.MACD.s,tmp.MACD.l,tmp.RSI,tmp.RSI.s,tmp.RSI.l,tmp.ADX,tmp.ADX.s,tmp.ADX.l,
   tmp.BB,tmp.BB.s,tmp.BB.l,tmp.SAR,tmp.SAR.s,tmp.SAR.l)

#### Add Statistics ####

Trade <- cbind(CL[,c(1,2)], EMAs, MACDs, RSIs, ADXs, BBs)
Trade$Sig.l <- round(EMAs$EMA.l/9 + ADXs$ADX.l/9 + MACDs$MACD.l/9,2)
Trade$Sig.s <- round(EMAs$EMA.s/9 + ADXs$ADX.s/9 + MACDs$MACD.s/9,2)
Trade$Sig.d <- round((Trade$Sig.l+10)/(Trade$Sig.s+10),2)
Trade$Sig <- round(EMAs$EMA/18 + ADXs$ADX/18 + MACDs$MACD/18,2)

EMAs.chl <- EMAs.ch
EMAs.chl$M05 <- round(EMAs.chl$M05*5/60,2)
EMAs.chl$M15 <- round(EMAs.chl$M15*15/60,2)
EMAs.chl$H4 <- round(EMAs.chl$H4*4/24,2)
EMAs.chl$W <- EMAs.chl$W*5

MACDs.chl <- MACDs.ch
MACDs.chl$M05 <- round(MACDs.chl$M05*5/60,2)
MACDs.chl$M15 <- round(MACDs.chl$M15*15/60,2)
MACDs.chl$H4 <- round(MACDs.chl$H4*4/24,2)
MACDs.chl$W <- MACDs.chl$W*5

RSIs.chl <- RSIs.ch
RSIs.chl$M05 <- round(RSIs.chl$M05*5/60,2)
RSIs.chl$M15 <- round(RSIs.chl$M15*15/60,2)
RSIs.chl$H4 <- round(RSIs.chl$H4*4/24,2)
RSIs.chl$W <- RSIs.chl$W*5

ADXs.chl <- ADXs.ch
ADXs.chl$M05 <- round(ADXs.chl$M05*5/60,2)
ADXs.chl$M15 <- round(ADXs.chl$M15*15/60,2)
ADXs.chl$H4 <- round(ADXs.chl$H4*4/24,2)
ADXs.chl$W <- ADXs.chl$W*5

BBs.chl <- BBs.ch
BBs.chl$M05 <- round(BBs.chl$M05*5/60,2)
BBs.chl$M15 <- round(BBs.chl$M15*15/60,2)
BBs.chl$H4 <- round(BBs.chl$H4*4/24,2)
BBs.chl$W <- BBs.chl$W*5

SARs.chl <- SARs.ch
SARs.chl$M05 <- round(SARs.chl$M05*5/60,2)
SARs.chl$M15 <- round(SARs.chl$M15*15/60,2)
SARs.chl$H4 <- round(SARs.chl$H4*4/24,2)
SARs.chl$W <- SARs.chl$W*5

CHL <- data.frame(Ticker=EMAs.chl$Ticker,
                  EMA.H1=round((EMAs.chl$M05+EMAs.chl$M15+EMAs.chl$H1)/3,2),
                  EMA.D=round((EMAs.chl$H4+EMAs.chl$D+EMAs.chl$W)/3,2),
                  MACD.H1=round((MACDs.chl$M05+MACDs.chl$M15+MACDs.chl$H1)/3,2),
                  MACD.D=round((MACDs.chl$H4+MACDs.chl$D+MACDs.chl$W)/3,2),
                  RSI.H1=round((RSIs.chl$M05+RSIs.chl$M15+RSIs.chl$H1)/3,2),
                  RSI.D=round((RSIs.chl$H4+RSIs.chl$D+RSIs.chl$W)/3,2),
                  ADX.H1=round((ADXs.chl$M05+ADXs.chl$M15+ADXs.chl$H1)/3,2),
                  ADX.D=round((ADXs.chl$H4+ADXs.chl$D+ADXs.chl$W)/3,2),
                  BB.H1=round((BBs.chl$M05+BBs.chl$M15+BBs.chl$H1)/3,2),
                  BB.D=round((BBs.chl$H4+BBs.chl$D+BBs.chl$W)/3,2),
                  SAR.H1=round((SARs.chl$M05+SARs.chl$M15+SARs.chl$H1)/3,2),
                  SAR.D=round((SARs.chl$H4+SARs.chl$D+SARs.chl$W)/3,2))
CHL$ST.H1=round((CHL$EMA.H1+CHL$MACD.H1+CHL$ADX.H1)/3,2)
CHL$LT.D=round((CHL$EMA.D+CHL$MACD.D+CHL$ADX.D)/3,2)
CHL <- CHL[, c(1,14,15,2:13)]
CHL.MM <- CHL[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),CHL$Ticker),]
CHL.MM$Ticker <- SpecsMini$Ticker
CHL.MM <- rbind(CHL,CHL.MM)
write.csv(CHL.MM,"./data/Stooq/results/CHL.csv")

Trade.ch <- cbind(EMAs.ch, MACDs.ch, RSIs.ch, ADXs.ch, BBs.ch)

SAR <- cbind(SAR,SARs,SARs.ch)
SAR <- SAR[, c(1:9,19,10,20,11,21,12,22,13,23,14,24,15,25,16,26,17,27)]
SAR.MM <- SAR[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),SAR$Ticker),]
SAR.MM[, 2:7] <- SAR.MM[, 2:7]/10
SAR.MM$Ticker <- SpecsMini$Ticker
SAR.MM <- rbind(SAR,SAR.MM)
write.csv(SAR.MM,"./data/Stooq/results/SAR.csv")

# BB <- cbind(BB,BBs,BBs.ch)
# BB <- BB[, c(1:9,19,10,20,11,21,12,22,13,23,14,24,15,25,16,26,17,27)]
# BB.MM <- BB[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),BB$Ticker),]
# BB.MM$Ticker <- SpecsMini$Ticker
# BB.MM <- rbind(BB,BB.MM)
# write.csv(BB.MM,"./data/Stooq/results/BB.csv")

SIG.ALL <- cbind(Trade[, c("Ticker","Sig.s","Sig.l","Sig.d","Sig")],CHL[, c("ST.H1","LT.D")])
SIG.ALL <- SIG.ALL[, c(1,3,7,2,6,4,5)]

ST <- Trade[abs(Trade$Sig.s)  > 0.5, c("Ticker","Sig.s","Sig.l","Sig.d")]
ST.L <- Trade[Trade$Sig.s  > 0.5, c("Ticker","Sig.s","Sig.l","Sig.d")]
ST.S <- Trade[Trade$Sig.s  < -0.5, c("Ticker","Sig.s","Sig.l","Sig.d")]

LT <- Trade[abs(Trade$Sig.l)  > 0.5, c("Ticker","Sig.l","Sig.s","Sig.d")]
LT.L <- Trade[Trade$Sig.l  > 0.5, c("Ticker","Sig.l","Sig.s","Sig.d")]
LT.S <- Trade[Trade$Sig.l  < -0.5, c("Ticker","Sig.l","Sig.s","Sig.d")]

ticker.FLT <- as.data.frame((Trade$Ticker %in% ST.L$Ticker)*1 +
                            (Trade$Ticker %in% LT.L$Ticker)*10 +
                            (Trade$Ticker %in% ST.S$Ticker)*-1 +
                            (Trade$Ticker %in% LT.S$Ticker)*-10)

names(ticker.FLT) <- "FLT"

sig.hist[[as.character(Sys.time())]] <- cbind(Trade,ticker.FLT)

if(sum(ST.L$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE))!=0){
        ST.L.Mini <- ST.L[ST.L$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE),]
        ST.L.Mini$Ticker <- paste0(ST.L.Mini$Ticker,"m")
        ST.L <- rbind(ST.L,ST.L.Mini)
}
if(sum(ST.S$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE))!=0){
        ST.S.Mini <- ST.S[ST.S$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE),]
        ST.S.Mini$Ticker <- paste0(ST.S.Mini$Ticker,"m")
        ST.S <- rbind(ST.S,ST.S.Mini)
}
if(sum(LT.L$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE))!=0){
        LT.L.Mini <- LT.L[LT.L$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE),]
        LT.L.Mini$Ticker <- paste0(LT.L.Mini$Ticker,"m")
        LT.L <- rbind(LT.L,LT.L.Mini)
}
if(sum(LT.S$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE))!=0){
        LT.S.Mini <- LT.S[LT.S$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE),]
        LT.S.Mini$Ticker <- paste0(LT.S.Mini$Ticker,"m")
        LT.S <- rbind(LT.S,LT.S.Mini)
}
if(sum(SIG.ALL$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE))!=0){
        SIG.ALL.Mini <- SIG.ALL[SIG.ALL$Ticker %in% gsub("m", "", SpecsMini$Ticker, fixed = TRUE),]
        SIG.ALL.Mini$Ticker <- paste0(SIG.ALL.Mini$Ticker,"m")
        SIG.ALL <- rbind(SIG.ALL,SIG.ALL.Mini)
}

FLT <- function(invest,risk,ATRx,SLATR.ST,SLATR.LT,SARTF.ST,SARTF.LT) {

#### Filter Inputs ####

invest <- invest
risk <- risk
risk.ST <- risk * 0.5
ATRx <- ATRx
SLATR.ST <- SLATR.ST
SLATR.LT <- SLATR.LT
SARTF.ST <- SARTF.ST
SARTF.LT <- SARTF.LT

#### Filtru ST Long ####

STL.added <- cbind.data.frame(DS$Market,DS$Update)
names(STL.added) <- c("Market","ID")
STL <- cbind(MM(cap = invest, instr = DS$Ticker, lots = 0, TrP = 0, SLT = 0, risk = risk.ST, ATRx = ATRx, SARTF = SARTF.ST, SLATR = SLATR.ST),
             STL.added)
rm(STL.added)

STL <- STL[DS$Ticker %in% ST.L$Ticker, ]
STL$Sig.s <- ST.L$Sig.s
STL$Sig.l <- ST.L$Sig.l
STL$Sig.d <- ST.L$Sig.d
STL <- STL[STL$Lots!=0 ,c(37:38,1:3,5:9,11:17,39:41,30:35,18:29)]
write.csv(STL,"./data/Stooq/results/STL.csv")

#### Filtru ST Short ####

STS.added <- cbind.data.frame(DS$Market,DS$Update)
names(STS.added) <- c("Market","ID")
STS <- cbind(MM(cap = invest, instr = DS$Ticker, lots = 0, TrP = 0, SLT = 0, risk = risk.ST, ATRx = ATRx, SARTF = SARTF.ST, SLATR = -SLATR.ST),
             STS.added)
rm(STS.added)

STS <- STS[DS$Ticker %in% ST.S$Ticker, ]
STS$Sig.s <- ST.S$Sig.s
STS$Sig.l <- ST.S$Sig.l
STS$Sig.d <- ST.S$Sig.d
STS <- STS[STS$Lots!=0 ,c(37:38,1:3,5:9,11:17,39:41,30:35,18:29)]
write.csv(STS,"./data/Stooq/results/STS.csv")

#### Filtru LT Long ####

LTL.added <- cbind.data.frame(DS$Market,DS$Update)
names(LTL.added) <- c("Market","ID")
LTL <- cbind(MM(cap = invest, instr = DS$Ticker, lots = 0, TrP = 0, SLT = 0, risk = risk, ATRx = ATRx, SARTF = SARTF.LT, SLATR = SLATR.LT),
             LTL.added)
rm(LTL.added)

LTL <- LTL[DS$Ticker %in% LT.L$Ticker, ]
LTL$Sig.s <- LT.L$Sig.s
LTL$Sig.l <- LT.L$Sig.l
LTL$Sig.d <- LT.L$Sig.d
LTL <- LTL[LTL$Lots!=0 ,c(37:38,1:3,5:9,11:17,39:41,30:35,18:29)]
write.csv(LTL,"./data/Stooq/results/LTL.csv")

#### Filtru LT Short ####

LTS.added <- cbind.data.frame(DS$Market,DS$Update)
names(LTS.added) <- c("Market","ID")
LTS <- cbind(MM(cap = invest, instr = DS$Ticker, lots = 0, TrP = 0, SLT = 0, risk = risk, ATRx = ATRx, SARTF = SARTF.LT, SLATR = -SLATR.LT),
             LTS.added)
rm(LTS.added)

LTS <- LTS[DS$Ticker %in% LT.S$Ticker, ]
LTS$Sig.s <- LT.S$Sig.s
LTS$Sig.l <- LT.S$Sig.l
LTS$Sig.d <- LT.S$Sig.d
LTS <- LTS[LTS$Lots!=0 ,c(37:38,1:3,5:9,11:17,39:41,30:35,18:29)]
write.csv(LTS,"./data/Stooq/results/LTS.csv")
}

#### Write CSV ####

# FLT(invest = 2000,risk = 5,ATRx = 2,SLATR.ST = 4,SLATR.LT = 5,SARTF.ST = 3,SARTF.LT = 5)

Trade.csv <- cbind(Trade,Trade.ch)
Trade.csv <- Trade.csv[, c(1:4,58,5,59,6,60,7,61,8,62,9,63,10,64,11,65,12,66,13:14,68,15,69,16,70,17,71,18,72,19,73,20,74,21,75,22,76,23:24,78,25,79,26,80,27,81,28,82,29,83,30,84,31,85,32,86,
                           33:34,88,35,89,36,90,37,91,38,92,39,93,40,94,41,95,42,96,43:44,98,45,99,46,100,47,101,48,102,49,103,50,104,51,105,52,106,53:56)]

Trade.M05 <- Trade.csv[, c(3,4:5,18:21,23:24,37:40,42:43,56:59,61:62,75:78,80:81,94:97)]
Trade.M15 <- Trade.csv[, c(3,6:7,18:21,25:26,37:40,44:45,56:59,63:64,75:78,82:83,94:97)]
Trade.H1 <- Trade.csv[, c(3,8:9,18:21,27:28,37:40,46:47,56:59,65:66,75:78,84:85,94:97)]
Trade.H4 <- Trade.csv[, c(3,10:11,18:21,29:30,37:40,48:49,56:59,67:68,75:78,86:87,94:97)]
Trade.D <- Trade.csv[, c(3,12:13,18:21,31:32,37:40,50:51,56:59,69:70,75:78,88:89,94:97)]
Trade.W <- Trade.csv[, c(3,14:15,18:21,33:34,37:40,52:53,56:59,71:72,75:78,90:91,94:97)]

Trade.csv <- cbind(Trade.csv,Trade.M05,Trade.M15,Trade.H1,Trade.H4,Trade.D,Trade.W)
if (length(sig.hist)==1) {
        Trade.csv1 <- cbind(Trade.csv, CL[,c(1,2)], ticker.FLT, data.frame(names(sig.hist[length(sig.hist)]),names(sig.hist[length(sig.hist)]),CL[,3:5]))
        Trade.csv1.MM <- Trade.csv1[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),Trade.csv1$Ticker),]
        Trade.csv1.MM[,c(2,289)] <- Trade.csv1.MM[,c(2,289)]/10
        Trade.csv1.MM$Ticker <- SpecsMini$Ticker
        Trade.csv1.MM <- rbind(Trade.csv1,Trade.csv1.MM)
        } else {
        Trade.csv2 <- cbind(Trade.csv, sig.hist[[length(sig.hist)-1]][c(1,2)], ticker.FLT, data.frame(names(sig.hist[length(sig.hist)]),names(sig.hist[length(sig.hist)-1]),CL[,3:5]))
        Trade.csv2.MM <- Trade.csv2[match(gsub("m", "", SpecsMini$Ticker, fixed = TRUE),Trade.csv2$Ticker),]
        Trade.csv2.MM[,c(2,289)] <- Trade.csv2.MM[,c(2,289)]/10
        Trade.csv2.MM$Ticker <- SpecsMini$Ticker
        Trade.csv2.MM <- rbind(Trade.csv2,Trade.csv2.MM)
        }
# write.csv(Trade.csv,"trades.csv")

# if (length(sig.hist)==1) {
#         write.csv(cbind(Trade.csv.MM, CL[,c(1,2)], ticker.FLT, data.frame(names(sig.hist[length(sig.hist)]),names(sig.hist[length(sig.hist)]),CL[,3:5])),"./data/Stooq/results/trdata.csv")
# } else {write.csv(cbind(Trade.csv.MM, sig.hist[[length(sig.hist)-1]][c(1,2)], ticker.FLT, data.frame(names(sig.hist[length(sig.hist)]),names(sig.hist[length(sig.hist)-1]),CL[,3:5])),"./data/Stooq/results/trdata.csv")
# }

if (length(sig.hist)==1) {
        write.csv(Trade.csv1.MM,"./data/Stooq/results/trdata.csv")
} else {write.csv(Trade.csv2.MM,"./data/Stooq/results/trdata.csv")}

print("ADX XAGUSD D+W & XAUUSD D+W is.na?", quote = FALSE)
print(c(is.na(last(D.Ind$XAGUSD$ADX$ADX)),is.na(last(D.Ind$XAUUSD$ADX$ADX)),is.na(last(W.Ind$XAGUSD$ADX$ADX)),is.na(last(W.Ind$XAUUSD$ADX$ADX))))

# for (i in 1:nrow(data.loc)) {
#         print(W.Ind[[i]]$ADX$ADX)
# }

# last(ADX(data.D.xts[[21]][,c("High","Low","Close")])$ADX)
# last(ADX(data.D.xts[[22]][,c("High","Low","Close")])$ADX)
# last(ADX(data.W.xts[[21]][,c("High","Low","Close")])$ADX)
# last(ADX(data.W.xts[[22]][,c("High","Low","Close")])$ADX)
