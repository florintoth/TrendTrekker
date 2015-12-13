MM <- function(cap=numeric(),instr=character(),lots=numeric(),TrP=numeric(),SLT=numeric(),risk=numeric(),ATRx=numeric(),SLATR=numeric()){

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
        } else if (lots>0 | SLATR>0) {
                SLATRxM05 <- TrP - ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx
                SLATRxM15 <- TrP - ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx
                SLATRxH1 <- TrP - ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx
                SLATRxH4 <- TrP - ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx
                SLATRxD <- TrP - ATR.MM$D[ATR.MM$Ticker==instr]*ATRx
                SLATRxW <- TrP - ATR.MM$W[ATR.MM$Ticker==instr]*ATRx
        } else if (lots<0 | SLATR<0){
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
        
        lev.risk <- round( lot.2T / (cap / DS$VAL.RON[DS$Ticker==instr]),2)
        LEV <- round(DS$VAL.RON[DS$Ticker==instr] * lots / cap,2)
        
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
        
        round(risk.VAL <- risk/100*cap,2)
                 
        if (TrP!=0) {ifelse(SLT==0,risk.TVAL<-0,risk.TVAL <- round(DS$TV.RON[DS$Ticker==instr] * (abs(SLT-TrP)) * DS$Pips[DS$Ticker==instr] * abs(lots),2))}
                else {ifelse(SLT==0,risk.TVAL<-0,risk.TVAL <- round(DS$TV.RON[DS$Ticker==instr] * (abs(SLT-DS$Close[DS$Ticker==instr])) * DS$Pips[DS$Ticker==instr] * abs(lots),2))}

        M05 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$M05[ATR.MM$Ticker==instr]*ATRx),2)
        M15 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$M15[ATR.MM$Ticker==instr]*ATRx),2)
        H1 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$H1[ATR.MM$Ticker==instr]*ATRx),2)
        H4 <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$H4[ATR.MM$Ticker==instr]*ATRx),2)
        D <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$D[ATR.MM$Ticker==instr]*ATRx),2)
        W <- round(abs(SL.pips)/DS$Pips[DS$Ticker==instr]/(ATR.MM$W[ATR.MM$Ticker==instr]*ATRx),2)
        
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
                            Risk.T=risk.TVAL,
                            Leverage=LEV,
                            M2M=M2M,
                            PipSL.Risk=SL.pips, 
                            SL.Risk=SL,
                            Lot.Risk=lot.2T,
                            Lev.Risk=lev.risk,
                            Lot.NoL=lot.NoL,
                            Risk=risk.VAL,
                            Margin=round(DS$MG.RON[DS$Ticker==instr],2)*abs(lots),
                            ATRxM05=M05,ATRxM15=M15,ATRxH1=H1,ATRxH4=H4,ATRxD=D,ATRxW=W,
                            SL.ATRxM05=round(SLATRxM05,4),SL.ATRxM15=round(SLATRxM15,4),SL.ATRxH1=round(SLATRxH1,4),
                            SL.ATRxH4=round(SLATRxH4,4),SL.ATRxD=round(SLATRxD,4),SL.ATRxW=round(SLATRxW,4))
        return(df.res)
}



#############################################


