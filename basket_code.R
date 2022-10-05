
################################################################################
############################## Γράφημα 1 #######################################
################################################################################

library(BasketballAnalyzeR)

PbP <- PbPmanipulation(PbP.BDB)
 tm <- c("BOS","CLE","GSW","HOU")
  selTeams <- which(Tadd$team %in% tm)
   FF.sel <- fourfactors(Tbox[selTeams,], Obox[selTeams,])
    FF <- fourfactors(Tbox,Obox)

    listPlots <- plot(FF)
    library(gridExtra)
    grid.arrange(grobs=listPlots[1:2], ncol=1)
    plot(FF.sel)

################################################################################
############################## Γράφημα 2 #######################################
################################################################################
    
    X <- data.frame(Tbox, PTS.O=Obox$PTS, TOV.O=Obox$TOV,
                    CONF=Tadd$Conference)
    XW <- subset(X, CONF=="W")
    labs <- c("Steals","Blocks","Defensive Rebounds")
    barline(data=XW, id="Team", bars=c("STL","BLK","DREB"),
            line="TOV.O", order.by="PTS.O", labels.bars=labs)
    
    Pbox.HR <- subset(Pbox, Team=="Houston Rockets" &
                          MIN>=500)
    barline(data=Pbox.HR, id="Player",
              bars=c("P2p","P3p","FTp"), line="MIN",
              order.by="PM", labels.bars=c("2P%","3P%","FT%"),
              title="Houston Rockets")
    
################################################################################
########################### Γράφημα 3 και 4 ####################################
################################################################################
    
     Pbox.PG <- subset(Pbox, Player=="Russell Westbrook" |
                          Player=="Stephen Curry" |
                          Player=="Chris Paul" |
                          Player=="Kyrie Irving" |
                          Player=="Damian Lillard" |
                          Player=="Kyle Lowry" |
                          Player=="John Wall" |
                          Player=="Rajon Rondo" |
                          Player=="Kemba Walker")
     attach(Pbox.PG)
     X <- data.frame(P2M, P3M, FTM, REB=OREB+DREB, AST,
                    STL, BLK)/MIN
     detach(Pbox.PG)
     radialprofile(data=X, title=Pbox.PG$Player, std=FALSE)
    
# Για το γράφημα 4 
     radialprofile(data=X, title=Pbox.PG$Player, std=TRUE)
        
################################################################################
############################## Γράφημα 5 #######################################
################################################################################    
    
    
    
     Pbox.sel <- subset(Pbox, MIN>= 500)
     attach(Pbox.sel)
     X <- data.frame(AST, TOV, PTS)/MIN
     detach(Pbox.sel)
     mypal <- colorRampPalette(c("blue","yellow","red"))
     scatterplot(X, data.var=c("AST","TOV"), z.var="PTS",
                   labels=1:nrow(X), palette=mypal) 
     SAS <- which(Pbox.sel$Team=="San Antonio Spurs")

################################################################################
############################## Γράφημα 6 #######################################
################################################################################     
    
     attach(Tbox)
     X <- data.frame(T=Team, P2p, P3p, FTp, AS=P2A+P3A+FTA)
     detach(Tbox)
     labs <- c("2-point shots (% made)",
                 "3-point shots (% made)",
                 "free throws (% made)",
                 "Total shots attempted")
     bubbleplot(X, id="T", x="P2p", y="P3p", col="FTp",
                 size="AS", labels=labs)
    
    
################################################################################
############################## Γράφημα 7 #######################################
################################################################################    
     
     Pbox.GSW.CC <- subset(Pbox,
                             (Team=="Golden State Warriors" |
                                Team =="Cleveland Cavaliers") &
                               MIN>=500)
     attach(Pbox.GSW.CC)
     X <- data.frame(ID=Player, Team, V1=DREB/MIN, V2=STL/MIN,
                       V3=BLK/MIN, V4=MIN)
     detach(Pbox.GSW.CC)
     labs <- c("Defensive Rebounds","Steals","Blocks",
                 "Total minutes played")
     bubbleplot(X, id="ID", x="V1", y="V2", col="V3",
                  size="V4", text.col="Team", labels=labs,
                  title="GSW and CC during the regular season",
                  text.legend=TRUE, text.size=3.5, scale=FALSE)
     
     
################################################################################
############################## Πίνακας 5 #######################################
################################################################################     
     
     
     Pbox.OKC <- subset(Pbox, Team=="Oklahoma City Thunder"
                          & MIN>=500)
    
     new <-Pbox.OKC[,c(2,10,11)] 
     
     res<-matrix(0,2,5)
     res[1,2]<-v1<-var(new$P3A)
     res[2,2]<-v2<-var(new$P3p)
     res[1,3]<-sd1<-sqrt(v1)
     res[2,3]<-sd2<-sqrt(v2)
     res[1,1]<-m1<-mean(new$P3A)
     res[2,1]<-m2<-mean(new$P3p)
     res[1,4]<-cv1<-sd1/m1
     res[2,4]<-cv2<-sd2/m2
     res[1,5]<-r1<-range(new$P3A)[2]-range(new$P3A)[1]
     res[2,5]<-r2<-range(new$P3p)[2]-range(new$P3p)[1]
     
     res<-data.frame(res)
     
     names(res)[1]<-"Mean"
     names(res)[2]<-"Var"
     names(res)[3]<-"SD"
     names(res)[4]<-"CV"
     names(res)[5]<-"Range"
     
     res
  
     
################################################################################
############################## Γράφημα 8 #######################################
################################################################################     
     
     
     Pbox.OKC <- subset(Pbox, Team=="Oklahoma City Thunder"
                        & MIN>=500)
     
     vrb1 <- variability(data=Pbox.OKC, data.var="P3p",
                         size.var="P3A")
     vrb2 <- variability(data=Pbox.OKC,
                         data.var=c("P2p","P3p","FTp"),
                         size.var=c("P2A","P3A","FTA"),
                         weight=TRUE)
     
     plot(vrb2, title="Variability diagram - OKC")     
     
     
     
################################################################################
############################## Πίνακας 6 #######################################
################################################################################       
     
     Pbox.OKC <- subset(Pbox, Team=="Oklahoma City Thunder")
     Pbox.OKC <- subset(Pbox.OKC, PTS>200)
     Pbox.OKC <- Pbox.OKC[order(Pbox.OKC$PTS),]
     Pbox.OKC$CPI <- 1:8
     Pbox.OKC <- Pbox.OKC[,c(2,5,23)]
     Pbox.OKC$CPTS <-cumsum(Pbox.OKC$PTS)
     Pbox.OKC$CPIpct <- 100*Pbox.OKC$CPI/8
     Pbox.OKC$CPTSpct <- round(100*Pbox.OKC$PTS/7997,2)
     Pbox.OKC$CPTSpct <- cumsum(Pbox.OKC$CPTSpct)
     Pbox.OKC
     g1 <- sum(Pbox.OKC$CPIpct-Pbox.OKC$CPTSpct)
     g2 <- sum(Pbox.OKC$CPIpct)
     100*g1/g2 
     
################################################################################
############################## Γράφημα 9 #######################################
################################################################################       
     
     Pbox.BN <- subset(Pbox, Team=="Brooklyn Nets")
     ineqBN <- inequality(Pbox.BN$PTS, nplayers=8)
     Pbox.MB <- subset(Pbox, Team=="Milwaukee Bucks")
     ineqMB <- inequality(Pbox.MB$PTS, nplayers=8)
     library(gridExtra)
     p1 <- plot(ineqBN, title="Brooklyn Nets")
     p2 <- plot(ineqMB, title="Milwaukee Bucks")
     grid.arrange(p1, p2, nrow=1)     
     
     
################################################################################
############################## Γράφημα 11 ######################################
################################################################################      
     
     subdata <- subset(PbP.BDB, player=="Kevin Durant")
     subdata$xx <- as.numeric(subdata$original_x)/10-25.25
     subdata$yy <- as.numeric(subdata$original_y)/10-41.75
     shotchart(data=subdata, x="xx", y="yy", z="points",
               num.sect=5, type="sectors", scatter=TRUE)     
     

################################################################################
############################## Πίνακας 7 #######################################
################################################################################           
     
     PbP.GSW <- subset(PbP, team=="GSW")  
     ev <- c("ejection","end of period","jump ball",
              "start of period","unknown","violation",
              "timeout","sub","foul","turnover")
     event.unsel <- which(PbP.GSW$event_type %in% ev)
     PbP.GSW.ev <- PbP.GSW[-event.unsel,]
     attach(PbP.GSW.ev)
     T <- table(oppTeam, event_type, exclude=ev)
     write.csv2(T,"C:\\Users\\user\\Downloads\\table.csv")
     detach(PbP.GSW.ev)     
     library(vcd)
     assocstats(T)

################################################################################
############################## Πίνακας 8 #######################################
################################################################################         
     
     
     library(dplyr)
     library(lsr)
     library(tibble)
     FF <- fourfactors(Tbox, Obox)
     attach(Tbox)
     attach(FF)
     X <- data.frame(PTS, P2M, P3M, FTM, REB=OREB+DREB, AST,
                       STL, BLK, ORtg, DRtg)
     detach(Tbox)
     detach(FF)
     Playoff <- Tadd$Playoff
     eta <- sapply(X, function(Y){
      cm <- round(tapply(Y, Playoff, mean), 1)
      eta2 <- etaSquared(aov(Y~Playoff))[1]*100
      c(cm, round(eta2, 2))
     }) %>%
      t() %>%
      as.data.frame() %>%
      rename(No=N, Yes=Y, eta2=V3) %>%
      rownames_to_column('rownm') %>%
      arrange(-eta2)
      column_to_rownames('rownm')
     
      
################################################################################
############################# Συσχετίσεις ######################################
################################################################################       
      
      
      data <- subset(Pbox, MIN>=500)
      attach(data)
      X <- data.frame(AST, TOV)/MIN
      detach(data)
      cor(X$AST, X$TOV)

      cor(X)
      
  
################################################################################
############################## Γράφημα 12 ######################################
################################################################################     
      
      
      
      data <- merge(Pbox, Tadd, by="Team")
      data <- subset(data, MIN>=500)
      attach(data)
      X <- data.frame(PTS, P3M, P2M, REB=(OREB+DREB), AST,
                        TOV, STL, BLK)/MIN
      X <- data.frame(X, Playoff=Playoff)
      detach(data)
      corrmatrix <- corranalysis(X[,1:8], threshold=0.5)
      plot(corrmatrix)
      
  
      
################################################################################
############################## Γράφημα 13 ######################################
################################################################################      
      
      
      PbP.GSW <- subset(PbP, team=="GSW")
      netdata <- assistnet(PbP.GSW)
      set.seed(7)
      plot(netdata)
      plot(netdata, layout="circle", edge.thr=20)
      
      
################################################################################
############################## Γράφημα 14 ######################################
################################################################################ 
      
      cols <- paste0(c("a","h"), rep(1:5,each=2))
      PbP.GSW.DG0 <- PbP.GSW[!apply(PbP.GSW[,cols], 1, "%in%",
                                      x="Draymond Green"),]
      netdata.DG0 <- assistnet(PbP.GSW.DG0)
      set.seed(1)
      plot(netdata.DG0)
      

      
      
      
      
      
      