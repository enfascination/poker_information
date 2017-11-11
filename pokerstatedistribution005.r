#!/usr/bin/env Rscript
library(optparse)
library(plyr)
library(infotheo)

### flag parsing from command line
option_list <- list(
                        make_option(c("-f", "--file"), 
                                            help="input file (output of HH2csv.py) [default %default]"),
                        make_option(c("-o", "--out"), default="pokerstateout.csv",
                                            help="output file (one row per hand per street) [default %default]")
                        )
opt <- parse_args(OptionParser(option_list=option_list))
file_input <- opt$file
#file_input <- "~/projecto_staid/poker_information/ABS005handssubset.csv"
file_output <- opt$out

### help functions
`%ni%` = Negate(`%in%`) 
### http://www.r-bloggers.com/ascii-code-table-in-r/
asc <- function(x) { strtoi(charToRaw(x),16L) }
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
#barelyknower <- read.csv("poker_data/ABShands.csv")
barelyknower <- read.csv(file_input, header=F)
names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
### filtering for this paper specificcally: only two player hands.   
###   actually don't do this.  I want all the hands to score players better; against all the data.
#barelyknower <- barelyknower[ barelyknower[,"seats"] == 2, ]

### play order
### assign seat numbers.  this isn't play order, but it can be used to calculate play order. This has to happen before I order or i will destroy some of the strucutre, which popped in here because i have multiple datapoints for particularu partuclar players.  afteri do this, I can retrieve play order
order_play <- matrix(0, ncol=8, nrow=nrow(barelyknower))
colnames(order_play) <- c("game", "IDs", "street", "seats", "bet", "order_seat", "order_play", "button")
order_play[,c("game")] <- as.numeric(barelyknower[,c("game")] )
order_play[,c("IDs")] <- as.numeric(barelyknower[,c("IDs")] )
order_play[,c("street")] <- as.numeric(barelyknower[,c("st")] )
order_play[,c("seats")] <- as.numeric(barelyknower[,c("seats")] )
order_play[,c("bet")] <- as.numeric(barelyknower[,c("bet")] )
order_seat_idx <- 1
order_play_idx <- 1
last_hand <- 0
sb <- as.numeric(barelyknower[1,"blnd"])/2
print("0 trying ot get order in")
for (i in (1+5):(nrow(order_play)-5)) {
  ### reconstruct seat order first
  if (order_play[i,"game"] != last_hand) { order_seat_idx <- 0}
  if (order_play[i,"street"] == 0) { order_seat_idx <- order_seat_idx + 1 }
  order_play[i,"order_seat"] <- order_seat_idx
  ### now reconstruct play order
  if (order_play[i,"street"] == 0) {
    if(order_play[i,"bet"] == sb) {
      order_play[(i):(i+4),"order_play"] <- 1
      ### for two players, this is enough
      if (order_play[i,"order_seat"] == 1) {
        order_play[(i+5):(i+9),"order_play"] <- 2
        if(order_play[i+5,"bet"] != sb*2) { print("ERROR JFDKFJDJJJ: should be the big blind (1)")}
      } else {
        if(order_play[i-5,"bet"] != sb*2) { print("ERROR ODUFOIDUFLDJFL: should be the big blind (2)")}
        order_play[(i-5):(i-1),"order_play"] <- 2
      }
      #order_play[(i):(i+4),"order_play"] <- 1
      #player_second_shift <- (order_play[i,"order_seat"])-order_play[i,"seats"])
      #player_fifth_shift <- (order_play[i,"order_seat"])+5-order_play[i,"seats"])
      #order_play[(i+5*player_second_shift):(i+5*player_second_shift+4),"order_play"] <- 1
    }
  }
}
### edge cases: only works for two players
if (order_play[1,"bet"] == 0.25) {
  order_play[1:5,"order_play"] <- 2
  order_play[6:10,"order_play"] <- 1
}
barelyknower <- cbind(barelyknower, order_play=order_play[,c("order_play")])
print("1 got order in")

### order by game, blind, hand, player, and street
#all(barelyknower == barelyknower[order(barelyknower[,1], barelyknower[,2], barelyknower[,3], barelyknower[,5], barelyknower[,4]),])
barelyknower <- barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),] ###XXX tmp
head(barelyknower)
#all(barelyknower == barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),]) 
#### more filtering
#for (i in 1:ncol(barelyknower)) { table(barelyknower[,i]) }
#table(barelyknower[,"act"])
barelyknower[,"game"] <- 0
barelyknower[,"blnd"] <- as.numeric(as.character(barelyknower[,"blnd"]))
barelyknower[,"chps"] <- as.numeric(as.character(barelyknower[,"chps"]))
head(barelyknower$act, 200)
actions <- as.character( barelyknower[,"act"] )
head(actions, 200)
actions <- sapply(actions, function(x) asc(substr(x, nchar(x), nchar(x))))
head(actions, 200)
### out is 0, fold is 1, everything else is 2
barelyknower[,"act"] <-  ifelse(actions == 79, 0, ifelse(actions == 70, 1, 2))
head(barelyknower[,"act"], 200)
#@barelyknower[,"act"] <- as.numeric(barelyknower[,"act"])
#@head(barelyknower$act, 200)
#as.matrix(barelyknower[1:1000,])
barelyknower <- as.matrix(barelyknower[,])
#barelyknower <- barelyknower[barelyknower[, "hand"] != 30818900,]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c(3038982044, 3078729526, 0),] ### has FF
#barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c(3041569636, 3029517634, 3038982044, 3078729526, 0),]
print("2 cleaning over")
#stopp
summary(barelyknower)
str(barelyknower)
table(barelyknower[,"st"])
table(barelyknower[,"seats"])
table(barelyknower[,"blnd"])
table(barelyknower[,"rank2"])
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c( 0, 30381660, 3025498550, 3038982044, 3044421138, 3078112502, 3078729526),]
bad_hand <- barelyknower[is.na(barelyknower[,"haul"]),"hand"]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% bad_hand,]

print("3 error correction starting")
### fix some errors introduced in earlier processing stages, errors that I wasn't able to fix  as easily in that structure as here (because i-1 is easy to find here)
betd <- rep(0, nrow(barelyknower))
ignore_win_fix <- F
for (i in 1:nrow(barelyknower)) {
  if (barelyknower[i,"st"] == 0) {
    betd[i] <- barelyknower[i,"bet"] 
  } else {
    #print(i)
    #print(barelyknower[i,])
    ### change bet to be bet delta
    betd[i] <- barelyknower[i,"bet"] - barelyknower[i-1,"bet"] 
    if (barelyknower[i,"st"] - barelyknower[i-1,"st"] != 1) {
      print("ERROR HORRIBLE AFDASFASDGASDFGASGD: are you sure that the matrix is ordered by player by street?") }
    
    ### if the player won in a round where he didn't have to bet, he got logged asbeing out, when he should get logged as checking.
    ###   keep in mind that "win" column has precognition and know if you're about to win
    if ((barelyknower[i,"haul"] > 0) & (barelyknower[i,"act"] == 0) & (barelyknower[i-1,"act"] != 0) & (!ignore_win_fix)) {
      barelyknower[i, "act"] <- 2
      ignore_win_fix <- T
    } else {
      ignore_win_fix <- F
    }
    
    ### make sure that bet is zero if the player is out
    if (barelyknower[i,"act"] == 0) {
      betd[i] <- 0
    }
    
    if (betd[i] < 0) {
      print(barelyknower[i,])
      print("ERROR HORRIBLE ASDFJJJJASDEEE: bets delta can't be negative.") }
    #print(barelyknower[i,])
  }
}
print("4 error correction over")

### binning begins
#bktest <- barelyknower[11461001:11471000,]
#barelyknower[,"rank2"] <- ifelse(barelyknower[,"rank2"] != 170, barelyknower[,"rank2"], 0)
barelyknower <- cbind(barelyknower, skill=0, extr1=0, extr2=0, intr=0, uIDs=0, beto=0)
barelyknower[,"uIDs"] <- barelyknower[,"IDs"]+barelyknower[,"blnd"]/100
### switch bets for bet deltas
barelyknower[,"beto"] <- barelyknower[,"bet"]
barelyknower[,"bet"] <- betd 
### get rid of O's, or don't.  yeah. Don't. DON'T
#barelyknower <- barelyknower[barelyknower[,"act"] != 38,]
### values of extrinsic are ternary (act=O)->0, low bet->1, and high bet=2.  
###  nice for us, there is a natural division that low and high bets are bets above and below the blind.
### later they can maybe be quaternary, taking into account both bet size and action
#by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2), 0, 0))
#barelyknower[,"extr1"] <- allbut(barelyknower[,"bet"], function(x) EqualFreq(x,2), 0, 0)
#barelyknower[,"extr1"] <- allbut(barelyknower[,"bet"], EqualFreq, but=0, repl=0, nbins=2)
barelyknower[,"extr1"] <- as.integer(discretize(barelyknower[,"bet"], nbins=2)[,1])
barelyknower[,"extr1"] <- ifelse(barelyknower[,"act"]==0, 0, barelyknower[,"extr1"])
barelyknower[,"extr2"] <- barelyknower[,"act"]
print("extrinsic hand strength cut at")
table(EqualFreq(barelyknower[barelyknower[,"bet"] != 0, "bet"],nbins=2))
table(EqualFreq(barelyknower[barelyknower[,"act"]!=0, "bet"],nbins=2))
table(barelyknower[,"extr1"])
table(barelyknower[,"bet"])
min(barelyknower[barelyknower[,"extr1"]==2,"bet"])
max(barelyknower[barelyknower[,"extr1"]==2,"bet"])
min(barelyknower[barelyknower[,"extr1"]==1,"bet"])
max(barelyknower[barelyknower[,"extr1"]==1,"bet"])
min(barelyknower[barelyknower[,"extr1"]==0,"bet"])
max(barelyknower[barelyknower[,"extr1"]==0,"bet"])
sum(barelyknower[,"bet"] != 0)
#system.time(unlist(by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, EqualFreq, but=0, repl=0, nbins=2))))
#system.time(unlist(by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2), 0, 0))))
#table(EqualFreq(barelyknower[barelyknower[,"bet"]!=0,"bet"], 2))

### values of intrinsic are quaternary, out-and-invisible -> 0, in-and-invisible->1, bad hand=2 and good hand=3
### later they can maybe be quaternary, taking into account both bet size and action
barelyknower[,"intr"] <- allbut(barelyknower[,"rank1"], EqualFreq, but=-1, repl=0, nbins=2)
print("intrinsic strength cut at")
table(EqualFreq(barelyknower[barelyknower[,"rank1"] != -1, "rank1"],nbins=2))
#ee <- EqualFreq(barelyknower[barelyknower[,"rank1"]!=-1,"rank1"], 2)
### values of skill are binary, net lost money and net gained money (actually equal freq bins over dist of mean hand earnings)
#bkskill <- matrix(unlist(by(bktest[bktest[,"st"] == 4,],  bktest[bktest[,"st"] == 4,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), blnd=unique(x[,"blnd"]), chps=mean(x[,"chps"]), skill_real=mean(x[,"haul"] - x[,"bet"]), skill=0))), ncol=5, byrow=T)
st_idx <- barelyknower[,"st"] == 4
bkskill <- matrix(unlist(by(barelyknower[st_idx,],  barelyknower[st_idx,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), blnd=unique(x[,"blnd"]), skill_real=mean(x[,"haul"] - x[,"beto"]), skill=0, hand_n=length(x[,"haul"])))), ncol=5, byrow=T)
colnames(bkskill) <- c("uIDs", "blnd", "skill_real", "skill", "hand_n")
### this makes three equal bins.  this is only sensible for two-player hands because winning money is more common here.
bkskill[,"skill"] <-  as.integer(EqualFreq(bkskill[,"skill_real"], nbins=3))-1
### this code ensurs that all winners are in the top bin: important if they are rare
#bkskill[,"skill"] <-  ifelse( bkskill[,"skill_real"] > 0, 1, bkskill[,"skill_real"] )
#bkskill[,"skill"] <- allbut(bkskill[,"skill"], function(x)discretize(x, nbins=2)[,1], but=1, repl=3)
print("skill cut at")
table(EqualFreq(bkskill[,"skill_real"], nbins=3))
table(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] )
median(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] )
#table(discretize(bkskill[bkskill[,"skill_real"] <= 0, "skill_real"],nbins=2)[,1])
#table(bkskill[, "skill"])
table(EqualFreq(bkskill[bkskill[,"skill_real"] <= 0, "skill_real"],nbins=2))
#barelyknower <- merge(barelyknower, bkskill[,c(-3,-4)], by=c("blnd", "uIDs"))
#rm(bkskill)
#todays.date <- "29102013_01"
#save(barelyknower, file=paste("poker_data/", "HHprocessed_", todays.date, ".Rdata", sep=""))

### generate the actual distribution
print(barelyknower[1,])
barelyknower <- barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"st"], barelyknower[,"order_play"]),] ###XXX tmp
poker_dist_hands <- unique(barelyknower[,c("hand")])
### why nine?  9 is the max number of payers at a table
poker_dist <- matrix(NA, nrow=length(poker_dist_hands)*5, ncol=4+4*9)
colnames(poker_dist) <- c("hand", "seats", "blind", "street", paste(c("skil", "intr", "extr1", "extr2"), c(matrix(rep(1:9,4),ncol=9,byrow=T)), sep=''))
row_data_tmp <- matrix(NA, nrow=4, ncol=9); rownames(row_data_tmp) <- c("skil", "intr", "extr1", "extr2")
dist_row <- 1
street_n <- barelyknower[1,"st"]
hand_id <- barelyknower[1,"hand"]
seat_n <- 1
for (data_row in 1:nrow(barelyknower)) {
  end_row <- F
  if (data_row == nrow(barelyknower)) { end_row <- T }
  #print(c(data_row, dist_row, street_n, hand_id, seat_n))
  row_data_tmp[, seat_n] <- c( bkskill[bkskill[,"uIDs"]==barelyknower[data_row,"uIDs"],"skill"], barelyknower[data_row,c("intr", "extr1", "extr2")])
  if ((end_row) || (street_n != barelyknower[data_row+1, "st"])) {
    dd <- c(barelyknower[data_row,c("hand")], seat_n, barelyknower[data_row,c("blnd", "st")], c(row_data_tmp))
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
#poker_dist <- poker_dist[order( poker_dist[,"seats"], poker_dist[,"hand"], poker_dist[,"street"]),] ###XXX tmp
write.csv(poker_dist, file=file_output)
#write.csv(poker_dist, file=paste("poker_data/", "poker_dist", todays.date, ".csv", sep=""))

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
