invest <- 2000
risk.tr <- 5
Pos <- data.frame()
Pos <- rbind(Pos
# ,MM(cap = invest, instr = "EURCAD", lots = 4, TrP = 1.42624, SLT = 1.4270, risk = risk.tr, 
#         ATRx = 2, SARTF = 5, SLATR = 4)
# ,MM(cap = invest, instr = "EURAUD", lots = 3, TrP = 1.5119, SLT = 1.5050, risk = risk.tr, 
#         ATRx = 2, SARTF = 5, SLATR = 4)
# # ,MM(cap = invest, instr = "AUDJPY", lots = -4, TrP = 90.337, SLT = 90.65, risk = risk.tr, 
#         ATRx = 2, SARTF = 5, SLATR = 4)
# ,MM(cap = invest, instr = "SPX", lots = -2, TrP = 2093.77, SLT = 2100.00, risk = risk.tr, 
#         ATRx = 2, SARTF = 3, SLATR = 3)
# ,MM(cap = invest, instr = "DAXm", lots = -2, TrP = 1133.12, SLT = 1133.00, risk = risk.tr, 
#         ATRx = 2, SARTF = 3, SLATR = 3)
# ,cbind(MM(cap = invest, instr = "USDCHF", lots = 4, TrP = 0.96271, SLT = 0.9570, risk = risk.tr, 
#     ATRx = 2, SARTF = 3, SLATR = 4),data.frame(TradeDate="Date"))
)
if (nrow(Pos) != 0){
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
if (nrow(Pos)>1) {
Lot.upd <- numeric()
for (i in 1:nrow(Pos)){
        Lot.tmp <-
                MM(cap = invest, instr = Pos$Ticker[i], lots = 0, TrP = 0, SLT = 0, risk = risk.tr, 
                   ATRx = abs(Pos$ATR[i]-Pos$Close[i])/ATRx.in[Pos$Ticker[i]==ATRx.in$Ticker,(ATR.in[i,2])+1],
                   SARTF = as.numeric(SAR.in[i,2]), SLATR = as.numeric(ATR.in[i,2]))$Lot.Risk * sign(Pos$Lots[i])
        Lot.upd <- c(Lot.upd,Lot.tmp)
        }
} else {
Lot.upd <- MM(cap = invest, instr = Pos$Ticker[1], lots = 0, TrP = 0, SLT = 0, risk = risk.tr, 
                   ATRx = abs(Pos$ATR[1]-Pos$Close[1])/ATRx.in[Pos$Ticker[1]==ATRx.in$Ticker,(ATR.in[2,1])+1],
                   SARTF = as.numeric(SAR.in[2,1]), SLATR = as.numeric(ATR.in[2,1]))$Lot.Risk * sign(Pos$Lots[1])
}
                
Pos$LotUpd <- Lot.upd
Pos$Sig.l <- SIG.in$Sig.l
Pos$LT.D <- SIG.in$LT.D
Pos$Sig.s <- SIG.in$Sig.s
Pos$ST.H1 <- SIG.in$ST.H1
Pos$Sig.d <- SIG.in$Sig.d
Pos$Sig <- SIG.in$Sig
}
# Write csv
write.csv(Pos,"./data/Stooq/Results/POS.csv")
Pos.file <- paste0("./data/Stooq/History/POS_", gsub(":","-",Sys.time()),".csv")
write.csv(Pos,Pos.file)

if (!exists("PosHist")) {PosHist <- data.frame()}
PosHist <- rbind(PosHist,Pos)
PosHist.file <- paste0("./data/Stooq/History/POS_", Sys.Date(),".csv")
write.csv(PosHist,PosHist.file)


