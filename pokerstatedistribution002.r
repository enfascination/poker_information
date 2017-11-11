library(plyr)
`%ni%` = Negate(`%in%`) 
### http://stackoverflow.com/questions/5731116/equal-frequency-discretization-in-r
EqualFreq <-function(x,nbins,include.lowest=TRUE,...){
  nx <- length(x)    
  id <- round(c(1,(1:(nbins-1))*(nx/nbins),nx))
  breaks <- sort(x)[id]
  if( sum(duplicated(breaks))>0) stop("nbins is too large.")
  cut(x,breaks,include.lowest=include.lowest,...)
}
### calculate (probably aggregating) set(n)->set(n) function (like cut) over x excepting values of but, which should output repl
### runs a lot faster if x is all numeric or int
### > mean(c(1:10, rep(0,10))[sample(20)])
 ### [1] 2.75
### > allbut(c(1:10, rep(0,10))[sample(20)], mean, 0, 0)
 ### [1] 0.0 5.5 0.0 5.5 5.5 0.0 5.5 5.5 0.0 0.0 5.5 0.0 0.0 0.0 5.5 5.5 5.5 0.0 0.0 5.5
### > as.numeric(cut(c(1:10, rep(0,10))[sample(20)],2))
 ### [1] 1 1 1 1 2 1 1 2 1 1 1 2 1 1 1 1 2 1 2 1
### > allbut(c(1:10, rep(0,10))[sample(20)], function(x) cut(x,2), 0, 0)
 ### [1] 0 2 0 0 2 0 2 1 0 0 0 0 2 1 0 1 0 2 1 1
allbut <- function(x, func, but, repl=NA, ...) {
  set1 <- as.matrix(cbind(t=1:length(x), x=x, y=x))[x!=but,]
  set1[,"y"] <- func(set1[,"x"], ...)
  x <- rep(repl, length(x))
  x[set1[,"t"]] <- set1[,"y"]
  return(x)
}

### end helper functions

#barelyknower <- read.csv("poker_data/ABShands.csv", colClasses=list("character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "numeric", "numeric", "numeric", "numeric"))
#names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
#load(paste("mount_brutus/poker_information/poker_data/", "HHsubset_31102013_01.Rdata", sep='') )
barelyknower <- read.csv("poker_data/ABShands.csv")
names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
### order by game, blind, hand, player, and street
#all(barelyknower == barelyknower[order(barelyknower[,1], barelyknower[,2], barelyknower[,3], barelyknower[,5], barelyknower[,4]),])
barelyknower <- barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),] ###XXX tmp
all(barelyknower == barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),]) 
#### filtering
#for (i in 1:ncol(barelyknower)) { table(barelyknower[,i]) }
barelyknower[,"game"] <- 0
barelyknower[,"blnd"] <- as.numeric(as.character(barelyknower[,"blnd"]))
barelyknower[,"chps"] <- as.numeric(as.character(barelyknower[,"chps"]))
barelyknower[,"act"] <- as.numeric(barelyknower[,"act"])
#as.matrix(barelyknower[1:1000,])
barelyknower <- as.matrix(barelyknower[,])
#barelyknower <- barelyknower[barelyknower[, "hand"] != 30818900,]
#barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c(3041569636, 3029517634, 3038982044, 3078729526, 0),]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c( 0, 30381660, 3025498550, 3038982044, 3044421138, 3078112502, 3078729526),]
bad_hand <- barelyknower[is.na(barelyknower[,"haul"]),"hand"]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% bad_hand,]

### binning begins
#bktest <- barelyknower[11461001:11471000,]
#barelyknower[,"rank2"] <- ifelse(barelyknower[,"rank2"] != 170, barelyknower[,"rank2"], 0)
barelyknower <- cbind(barelyknower, skill=0, extr=0, intr=0, uIDs=0)
barelyknower[,"uIDs"] <- barelyknower[,"IDs"]+barelyknower[,"blnd"]/100
### get rid of O's, or don't.  yeah. Don't. DON'T
#barelyknower <- barelyknower[barelyknower[,"act"] != 38,]
### values of extrinsic are ternary (act=O)->0, low bet->1, and high bet=2.  
###  nice for us, there is a natural division that low and high bets are bets above and below the blind.
### later they can maybe be quaternary, taking into account both bet size and action
#by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2), 0, 0))
#barelyknower[,"extr"] <- allbut(barelyknower[,"bet"], function(x) EqualFreq(x,2), 0, 0)
barelyknower[,"extr"] <- unlist(by(barelyknower[,"bet"], barelyknower[,"blnd"], function(x) allbut(x, EqualFreq, but=0, repl=0, nbins=2)))
print("extrinsic hand strength cut at")
by(barelyknower[,"bet"], barelyknower[,"blnd"], function(x) table(EqualFreq(x[x != 0],2)))
#system.time(unlist(by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, EqualFreq, but=0, repl=0, nbins=2))))
#system.time(unlist(by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2), 0, 0))))
#table(EqualFreq(barelyknower[barelyknower[,"bet"]!=0,"bet"], 2))

### values of intrinsic are quaternary, out-and-invisible -> 0, in-and-invisible->1, bad hand=2 and good hand=3
### later they can maybe be quaternary, taking into account both bet size and action
barelyknower[,"intr"] <- unlist(by(barelyknower[,"rank1"], barelyknower[,"blnd"], function(x) allbut(x, EqualFreq, but=-1, repl=0, nbins=2)))
print("intrinsic strength cut at")
by(barelyknower[,"rank1"], barelyknower[,"blnd"], function(x) table(EqualFreq(x[x != -1],2)))
#ee <- EqualFreq(barelyknower[barelyknower[,"rank1"]!=-1,"rank1"], 2)
### values of skill are binary, net lost money and net gained money (actually equal freq bins over dist of mean hand earnings)
#bkskill <- matrix(unlist(by(bktest[bktest[,"st"] == 4,],  bktest[bktest[,"st"] == 4,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), blnd=unique(x[,"blnd"]), chps=mean(x[,"chps"]), skill_real=mean(x[,"haul"] - x[,"bet"]), skill=0))), ncol=5, byrow=T)
st_idx <- barelyknower[,"st"] == 4
bkskill <- matrix(unlist(by(barelyknower[st_idx,],  barelyknower[st_idx,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), blnd=unique(x[,"blnd"]), skill_real=mean(x[,"haul"] - x[,"bet"]), skill=0, hand_n=length(x[,"haul"])))), ncol=5, byrow=T)
colnames(bkskill) <- c("uIDs", "blnd", "skill_real", "skill", "hand_n")
bkskill[,"skill"] <-  unlist(by(bkskill[,"skill_real"], bkskill[,"blnd"], function(x) c(skill=as.integer(EqualFreq(x, nbins=2))-1)))
print("skill cut at")
by(bkskill[,"skill_real"], bkskill[,"blnd"], function(x) table(EqualFreq(x, nbins=2)))
#barelyknower <- merge(barelyknower, bkskill[,c(-3,-4)], by=c("blnd", "uIDs"))
#rm(bkskill)
todays.date <- "29102013_01"
save(barelyknower, file=paste("poker_data/", "HHprocessed_", todays.date, ".Rdata", sep=""))

### generate the actual distribution
#poker_dist_hands <- unique(barelyknower[,c("hand")])
#poker_dist <- matrix(NA, nrow=length(poker_dist_hands)*5, ncol=3+3*9)
#colnames(poker_dist) <- c("seats", "street", "blind", paste(c("skil", "intr", "extr"), c(matrix(rep(1:9,3),ncol=9,byrow=T)), sep=''))
#streets_n <- 5
#for (i in 1:length(poker_dist_hands)) {
  #for (k in 0:(streets_n-1)) {
    #row_data <- barelyknower[(barelyknower[,"hand"] == poker_dist_hands[i]) & (barelyknower[,"st"] == k),c("blnd", "uIDs", "intr", "extr" )]
    #poker_dist[(i-1)*streets_n+k+1, 1] <- nrow(row_data)
    #poker_dist[(i-1)*streets_n+k+1, 2] <- k
    #poker_dist[(i-1)*streets_n+k+1, 3] <- row_data[1,"blnd"]
    #for (j in 1:nrow(row_data)) {
      #poker_dist[(i-1)*streets_n+k+1,4+(j-1)*3] <- bkskill[bkskill[,"uIDs"]==row_data[j,"uIDs"],"skill"]
      #poker_dist[(i-1)*streets_n+k+1,4+(j-1)*3+1] <- row_data[j,"intr"]
      #poker_dist[(i-1)*streets_n+k+1,4+(j-1)*3+2] <- row_data[j,"extr"]
    #}
  #}
  #if (i %% 10 == 0) {print (i)}
#}

### generate the actual distribution
barelyknower <- barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"st"], barelyknower[,"uIDs"]),] ###XXX tmp
poker_dist_hands <- unique(barelyknower[,c("hand")])
poker_dist <- matrix(NA, nrow=length(poker_dist_hands)*5, ncol=4+3*9)
colnames(poker_dist) <- c("hand", "seats", "blind", "street", paste(c("skil", "intr", "extr"), c(matrix(rep(1:9,3),ncol=9,byrow=T)), sep=''))
row_data_tmp <- matrix(NA, nrow=9, ncol=3); colnames(row_data_tmp) <- c("skil", "intr", "extr")
dist_row <- 1
street_n <- barelyknower[1,"st"]
hand_id <- barelyknower[1,"hand"]
seat_n <- 1
for (data_row in 1:nrow(barelyknower)) {
  end_row <- F
  if (data_row == nrow(barelyknower)) { end_row <- T }
  #print(c(data_row, dist_row, street_n, hand_id, seat_n))
  row_data_tmp[seat_n, ] <- c( bkskill[bkskill[,"uIDs"]==barelyknower[data_row,"uIDs"],"skill"], barelyknower[data_row,c("intr", "extr")])
  if ((end_row) || (street_n != barelyknower[data_row+1, "st"])) {
    dd <- c(barelyknower[data_row,c("hand")], seat_n, barelyknower[data_row,c("blnd", "st")], c(t(row_data_tmp[order(row_data_tmp[,1], row_data_tmp[,2], row_data_tmp[,3], decreasing=T),])))
    #print (dd)
    poker_dist[dist_row,] <- dd
    if ((end_row) || (hand_id != barelyknower[data_row+1, "hand"])) {
      street_n <- 0 
      if (!end_row) { hand_id <- barelyknower[data_row+1, "hand"] }
    } else {street_n <- street_n + 1}
    dist_row <- dist_row + 1
    row_data_tmp[,] <- NA
    seat_n <- 1
  } else {
    seat_n <- seat_n + 1
  }
  #if (data_row %% 10 == 0) {print (i)}
}
write.csv(poker_dist, file=paste("poker_data/", "poker_dist", todays.date, ".csv", sep=""))

### TODO:
### DONE: table every column for sane values
### DONE: eliminate suspect games at lines: c(30818900, 30818901, 1104476, 7893496)
### DONE: matricize soon after input
### DONE: making values binary as necessary, bin separately within each blind
### one row per street per hand: extr and intr for all players with both, plus the blind (, plus chps?), 
### hand id, seats, street, blind, skill1, intr1, extr1, skill2, ... , extr9
### figure out how to manage the diff channels (rate players by a binning of net winnings, and action (check, raise, fold, call, etc))?
### eliminate hands with players whose IDs are integers rather than hashes?
### how much they actually bet or how much they were willing to bet (right now I include uncalled bets and raises)
### for later streets, should coding include the number of seats (originally) at the table or only the number of players (remaining) at the table.

### Key to actions:
### > unique(barelyknower[,7], sort=T)
 ### [1] F    O    A    R    C    XF   X    B    XC   CC   CF   BC   XR   BF   RF  
 ### [16] RC   BB   XB   BR   CRC  RCR  BRR  RRC  RR   BRF  XRR  CB   RB   XRF  XRB 
 ### [31] CR   CRB  CCF  XCF  CRR  BRC  XRC  CCC  BRB  RCF  XCC  RCB  XCR  RCC  XRRB
 ### [46] CRF  RRB  CCB  RRR  CRRB RRF  XRRR BRRC BCF  XCB  FF   XRRC BCC  BCB  CCR 
 ### [61] BCR  BRRB XCRB RRRC CRRC CRCB XCCB XCRC XRRF CRRR XCRR RCCC RRRB RCRC 6.0 
 ### [76]     
 ### 76 Levels:  6.0 A B BB BC BCB BCC BCF BCR BF BR BRB BRC BRF BRR BRRB BRRC ... XRRR
### > as.numeric(unique(barelyknower[,7], sort=T))
 ### [1] 36 38  3 39 19 67 56  4 58 21 26  6 68 11 48 41  5 57 12 29 46 16 51 49 15
 ### [26] 72 20 40 71 69 27 28 24 62 32 14 70 23 13 45 60 42 63 43 73 31 50 22 53 33
 ### [51] 52 76 18  9 59 37 74  8  7 25 10 17 64 55 34 30 61 65 75 35 66 44 54 47  2
 ### [76]  1


### bad lines: 27702963 
