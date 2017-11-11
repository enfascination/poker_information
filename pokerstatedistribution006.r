#!/usr/bin/env Rscript
library(optparse)
library(plyr)
library(infotheo)
library(testit)
library(stringi)
### this is for use of fread.
library(data.table)
require(bit64)

### flag parsing from command line
option_list <- list(
                        make_option(c("-f", "--file"), 
                                            help="input file (output of HH2csv.py) [default %default]"),
                        make_option(c("-o", "--out"), default="pokerstateout",
                                            help="output file (one row per hand per street) [default %default]")
                        )
opt <- parse_args(OptionParser(option_list=option_list))
if( is.null(opt$file) ) {
    file_input <- "/Users/sfrey/projecto_staid/poker_information/PS0050pokerhandrank.csv"
    file_input <- "~/projecto_staid/poker_information/ABS005handssubset.csv"
    file_input <- "/Users/sfrey/projecto_staid/poker_information/PS0600pokerhandrank.csv"
    file_output_stem <- "pokerstateoutfromconsoleABS0050"
    file_output_stem <- "pokerstateoutfromconsolePS0600"
} else {
    file_input <- opt$file
    file_output_stem <- opt$out
}
file_output <- paste0( file_output_stem, ".csv")
file_output_player_counts <- paste0( file_output_stem, "playercounts", ".csv")

### help functions
`%ni%` = Negate(`%in%`) 
asdf <- as.data.frame
### http://www.r-bloggers.com/ascii-code-table-in-r/
asc <- function(x) { strtoi(charToRaw(x),16L) }
EqualFreq <-function(x,nbins,include.lowest=TRUE,asFactor=FALSE,sketchyUniquePatch=FALSE,...){

    cutpoints <- quantile(x,probs=seq(0,1,length.out=nbins+1))
    ### this is to prevent crashes and keep the code running in a way that I can still be warned and go back and clean up later.
    if (sketchyUniquePatch) {
        cutpoints <- unique(cutpoints)
    }
    val <- cut(x,breaks=cutpoints,include.lowest=include.lowest,...)
    if (asFactor) {
        return(val)
    } else {
        return(as.numeric(val))
    }
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
### 10x performance improvement with data.table's fread
if (FALSE) {
    barelyknower <- read.csv(file_input, header=F)
    names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
} else {
    barelyknower_raw <- fread(file_input, header=F)
    barelyknower_raw[,V3:=as.numeric(V3)]
    names(barelyknower_raw) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
    ### I am now fixing act early, so that O really actually means "Out" (after folding, and this player's play is over)
    #bkdt <- barelyknower_raw[3112500:3115000]
    #bkdt <- barelyknower_raw[4000000:5000000]
    bkdt <- barelyknower_raw
    ### for each hand, find the street at which the last player folded.  
    ### check the winners action in that street.  if it is 'O', change it to 'D', otherwise, leave it be.
    ###   round is there because some hands don't have any folds, and max gets an empty string, and round aboivds a type error
    bkdt_passive_winning <- bkdt[,.(st_last_in_game=round(.SD[stri_detect_fixed( act, "F"),max(st)][1]), player_winning=.SD[haul>0,IDs][1]),by=.(hand)]
    setkey(bkdt_passive_winning, hand, st_last_in_game, player_winning)
    setkey(bkdt, hand, st, IDs)
    bkdt[J(bkdt_passive_winning[is.finite(st_last_in_game)]),act:=ifelse(act=='O','D',act)]
    barelyknower_raw[,game:=as.numeric(factor(game, levels=c("PS","ABS")))]
    barelyknower_raw[,act:=factor(act)]
    setkey(bkdt, hand, st, IDs)
    barelyknower <- asdf(barelyknower_raw)
    rm(bkdt)
    rm(bkdt_passive_winning)
    rm(barelyknower_raw)
}
print("The warnings here are because of an Inf that I am catching and controlling for post hoc")
#for (cc in 1:ncol(barelyknower2)) {
    #print( all(barelyknower[,cc] == barelyknower2[,cc]))
#}

### add this early (main "act" was use dorignally, and still has its uses, even if it needs a patch later)
barelyknower[,"act2"] <- "Passive" ### for plays that had nothing beyond the characters X,C,A,D
barelyknower[grep("[BR]", barelyknower[,"act"]),"act2"] <- "Aggressive"
barelyknower[grep("F", barelyknower[,"act"]),"act2"] <- "Fold"
barelyknower[grep("O", barelyknower[,"act"]),"act2"] <- "Out"
### factorize (for itnerpretability) and then instantly  defactorize (for later matrix-wise processing); (also subtract 1 because these will be indices of the third D in a 3x3x3 array)
barelyknower$act2 <- as.numeric(factor( barelyknower$act2, levels=c("Out", "Fold", "Passive", "Aggressive") )) - 1

### filtering for this paper specificcally: only two player hands.   
###   actually don't do this.  I want all the hands to score players better; against all the data.
###   actually, do this: first, I'm not binnign wage strengths anymore, 
###     second, restrincting to heads up allows that certain hands are stronger heads up than on larger tables
### actually actually actually, don't do this.  in calculating skill, experts mostly play large table, 
###   and they all get filtered out if I code on 2 player tables.  awk.
#barelyknower <- barelyknower[ barelyknower[,"seats"] == 2, ]

### play order
### assign seat numbers.  this isn't play order, but it can be used to calculate play order. This has to happen before I order or i will destroy some of the strucutre, which popped in here because i have multiple datapoints for particularu partuclar players.  afteri do this, I can retrieve play order
barelyknower <- asdf( as.data.table( barelyknower)[order(hand, IDs, st)])
order_play_names <- c("hand", "IDs", "street", "seats", "bet", "order_seat", "order_play", "button", "act2", "uncalled_bet_size")
order_play <- matrix(0, ncol=length(order_play_names), nrow=nrow(barelyknower))
colnames(order_play) <- order_play_names
order_play[,c("hand")] <- as.numeric(barelyknower[,c("hand")] )
order_play[,c("IDs")] <- as.numeric(barelyknower[,c("IDs")] )
order_play[,c("street")] <- as.numeric(barelyknower[,c("st")] )
order_play[,c("seats")] <- as.numeric(barelyknower[,c("seats")] )
order_play[,c("bet")] <- as.numeric(barelyknower[,c("bet")] )
order_play[,c("act2")] <- as.numeric(barelyknower[,c("act2")] )
order_seat_idx <- 0
last_hand <- 0
sb <- as.numeric(barelyknower[1,"blnd"])/2
prob_count <- 0
case_count <- 0
print("0 trying ot get order in")
### I need to analyze base don play or, not seat order.  The data is ordered on seat order, not play order. Those only match half the time. But whoever poseted the small blind was the dealer and, because this is heads up, was the first to act
for (i in 1:(nrow(order_play)-5)) {
#for (i in (1+5):(nrow(order_play)-5)) {
  ### reconstruct seat order first
  if (order_play[i,"hand"] != last_hand) { order_seat_idx <- 0}
  if (order_play[i,"street"] == 0) { order_seat_idx <- order_seat_idx + 1 }
  order_play[i,"order_seat"] <- order_seat_idx
  last_hand <- order_play[i,"hand"]
  ### now reconstruct play order
  if ((order_play[i,"seats"] == 2) && (order_play[i,"street"] == 0) && (order_play[i,"bet"] == sb)) {
      order_play[(i):(i+4),"order_play"] <- 1
      ### for two players, this is enough
      if (order_play[i,"order_seat"] == 1) {
        order_play[(i+5):(i+9),"order_play"] <- 2
        testit::assert(order_play[i+5,"bet"] == sb*2,  fact=paste0("ERROR JFDKFJDJJJ: should be the big blind (1)", paste0(order_play[i+5,]), "and", sb, "and", sb*2))
        testit::assert( order_play[i,"hand"] == order_play[i+5,"hand"],  fact=paste0("ERROR ASDFUIUE: different hand 1"))
      } else {
        order_play[(i-5):(i-1),"order_play"] <- 2
        testit::assert(order_play[i-5,"bet"] == sb*2,  fact=paste0("ERROR JFDKFJDJJJ: should be the big blind (2)", paste0(order_play[i-5,]), "and", sb, "and", sb*2))
        testit::assert( order_play[i,"hand"] == order_play[i-5,"hand"],  fact=paste0("ERROR ASDFUIUE: different hand 2"))
      }
      #order_play[(i):(i+4),"order_play"] <- 1
      #player_second_shift <- (order_play[i,"order_seat"])-order_play[i,"seats"])
      #player_fifth_shift <- (order_play[i,"order_seat"])+5-order_play[i,"seats"])
      #order_play[(i+5*player_second_shift):(i+5*player_second_shift+4),"order_play"] <- 1
    }
}
### edge case: last 5 rows have seat number equals to number of seats
testit::assert( ( min(order_play[,"order_seat"]) == 1) | (tail(order_play[,"order_seat"], 5) == 0),  fact=paste0("ERROR TEUEGFSKZFD: edge case: last 5 rows have seat number equals to number of seats"))
### and ... fixed:
order_play[,"order_seat"] <- ifelse(order_play[,"order_seat"] == 0, order_play[,"seats"], order_play[,"order_seat"])

### add in this loop a way to detect uncalled bets
###   (can't depend on order_seat, which is only solved for two players)
###   (can't depend on act, which, until further down, has a problem when player won by betting nothing)
### first,, get it in the right order
order_play <- as.matrix( as.data.table( order_play)[order(hand, street, IDs)])
barelyknower <- asdf( as.data.table( barelyknower)[order(hand, st, IDs)])
for (i in 1:(nrow(order_play)-5)) {
  if (order_play[i,"order_seat"] == 1) {
      seats <- order_play[i,"seats"]
      theTable <- i:(i+seats-1)
      acts <- order_play[theTable,"act2"]
      if (sum(acts > 1) == 1) { # if this is the street where a player won by posting an uncalled bet
        bets <- order_play[theTable,"bet"]
        i_winner_offset <- which(acts>1,arr.ind=TRUE) - 1
        #XXXMAKE THIS HAPPEN. ALL ELSE WILL WORK SND YOU CANGET  BACK TO SUMMING SILL TO 0.
        #XXX YOU KNOW A BET WAS UNCALLED IF ALL OTHER PLAYERS IN THAT ROUND WERE oUT OR fOLD (MAYBE AND AND AT LEAST ONE WAS fOLD).
        #XXX IN THESE CASES ALL OTHER BETS SHOULD BE ZERO, OR SHOUDL THEY?
        #XXX DIAGNOSE VIOLATIONS (NON-ZERONESSES OF OTHER BETS: NUMERBER AND FREQUENCY AND SEVERITY), LIKE IN THAT WERID ALL-IN HAND WHERE IT COMES OUT LOOKING LIKE A NON-WINNER HAD AN UNCALLED BET
        uncalled_bet_size <- bets[i_winner_offset+1]
        order_play[i+i_winner_offset,"uncalled_bet_size"] <- uncalled_bet_size 
        if (((sort(bets,decreasing=TRUE)[1] - sort(bets,decreasing=TRUE)[2]) < 0 ) | (uncalled_bet_size != max(bets))) {
            prob_count <- prob_count + 1
            print(paste0("PROBLEM OIDHGLL: bet was negative (bet wasn't largest bet). problem count:", prob_count, " ", i_winner_offset, " ", uncalled_bet_size, " ", case_count, " ", ((sort(bets,decreasing=TRUE)[1] - sort(bets,decreasing=TRUE)[2]) < 0 ) ," ", (uncalled_bet_size != max(bets))))
            print(order_play[theTable,])
        }
        case_count <- case_count + 1
      }
  }
}

barelyknower <- cbind(barelyknower, order_play=order_play[,c("order_play")], seat=order_play[,c("order_seat")], uncalled_bet_size=order_play[,c("uncalled_bet_size")])
rm(order_play)
print("1 got order in")

### order by game, blind, hand, player, and street
#all(barelyknower == barelyknower[order(barelyknower[,1], barelyknower[,2], barelyknower[,3], barelyknower[,5], barelyknower[,4]),])
barelyknower <- asdf( as.data.table( barelyknower)[order(blnd, hand, IDs, st)])
head(barelyknower)
#all(barelyknower == barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),]) 
#### more filtering
#for (i in 1:ncol(barelyknower)) { table(barelyknower[,i]) }
#table(barelyknower[,"act"])
barelyknower[,"blnd"] <- as.numeric(as.character(barelyknower[,"blnd"]))
barelyknower[,"chps"] <- as.numeric(as.character(barelyknower[,"chps"]))
### try action based extr coding (don't code on wager size, but behavior)
###  as.character is important because otherwise we'd be testing for F somewhere in the whole column, rather that in each entry of the column.
#head(barelyknower$act, 200)
actions <- as.character( barelyknower[,"act"] )
#head(actions, 200)
actions <- sapply(actions, function(x) asc(substr(x, nchar(x), nchar(x))))
#head(actions, 200)
### out is 0, fold is 1, everything else is 2
barelyknower[,"act"] <-  ifelse(actions == 79, 0, ifelse(actions == 70, 1, 2))
#head(barelyknower[,"act"], 200)
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
#table(barelyknower[,"rank2"])
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c( 0, 30381660, 3025498550, 3038982044, 3044421138, 3078112502, 3078729526),]
bad_hand <- barelyknower[is.na(barelyknower[,"haul"]),"hand"]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% bad_hand,]

print("3 error correction starting")
### fix some errors introduced in earlier processing stages, errors that I wasn't able to fix  as easily in that structure as here (because i-1 is easy to find here)
### remember: i am making a distinction between wager as a signal of hand strenth and wager as a basis for caluclating how much the losing player lost.  these diverge when bets go uncalled.
betd <- rep(0, nrow(barelyknower))
betd_signal <- rep(0, nrow(barelyknower))
ignore_win_fix <- F
for (i in 1:nrow(barelyknower)) {
  #if (barelyknower[i,"hand"] != 3038053444.00) {
      #next
  #}
  st <- barelyknower[i,"st"]
  if (st == 0) {
    betd[i] <- barelyknower[i,"bet"] 
    betd_signal[i] <- betd[i]
  } else {
    #print(i)
    #print(barelyknower[i,])
    ### change bet to be bet delta
    if (i==1) { #### first row?
        betd[i] <- barelyknower[i,"bet"]
        betd_signal[i] <- betd[i]
    } else {
        betd[i] <- barelyknower[i,"bet"] - barelyknower[i-1,"bet"] 
        betd_signal[i] <- betd[i]
        if (barelyknower[i,"uncalled_bet_size"] > 0 ) { ### special case: was bet uncalled?
            betd[i] <- betd[i-1] ### XXX this is just an approximation: if the player caled then made an uncalled raise, this misses the size of the call.
            if( barelyknower[i,"bet"] != barelyknower[i,"uncalled_bet_size"]) {
                print("PROBLEM: I'm confused (that's the problem)")
                print(c(barelyknower[i,],betd[i]))
            }
        }
    }
    if ((barelyknower[i,"hand"] != barelyknower[i-1,"hand"]) | (barelyknower[i,"st"] - barelyknower[i-1,"st"] != 1)) {
      print("ERROR HORRIBLE AFDASFASDGASDFGASGD: are you sure that the matrix is ordered by player by street?") }
    
    ### if the player won in a round where he didn't have to bet, he got logged asbeing out, when he should get logged as checking.
    ###   keep in mind that "win" column has precognition and know if you're about to win
    ###   keep in mind not to mark as an action the round after I won by no one calling my bet (or some false positive condition like that).  that's the second term 
    ###   example: hand 59938878480
    if ((barelyknower[i,"haul"] > 0) & (barelyknower[i,"bet"] == barelyknower[i-1,"bet"]) & (barelyknower[i,"act"] == 0) & (barelyknower[i-1,"act"] != 0) & (!ignore_win_fix) & (barelyknower[i,"uncalled_bet_size"] != 0)) {
      barelyknower[i, "act"] <- 2
      ignore_win_fix <- T
      print(paste0("ERROR YIAGFJD: AS of now, with uptop fix to act, I should never enter this condition again",i))
    } else {
      ignore_win_fix <- F
    }
    
    ### make sure that bet is zero if the player is out
    if (barelyknower[i,"act"] == 0) {
      betd[i] <- 0
    }
    
    if (betd[i] < 0) {
        if (betd[i] == -sb & barelyknower[i,"st"] == 1 & barelyknower[i,"seats"] > 2) {
            print(paste0("WARNING: Negative bet  due likely to some wierd (house?) rule of absolute poker regarding preflop play at large tables.  on hand: ", barelyknower[i,"hand"])) 
        } else {
            print(barelyknower[i,])
            print("ERROR HORRIBLE ASDFJJJJASDEEE: bets delta can't be negative.") 
        }
    }
  }
    #print(c(barelyknower[i,],betd[i]))
}
print("4 error correction over")
barelyknower <- as.matrix( as.data.table( barelyknower)[order(blnd, hand, IDs, st)])

### binning begins
#bktest <- barelyknower[11461001:11471000,]
#barelyknower[,"rank2"] <- ifelse(barelyknower[,"rank2"] != 170, barelyknower[,"rank2"], 0)
barelyknower <- cbind(barelyknower, skill=0, extr1=0, extr2=0, intr=0, intr_prosp=0, uIDs=0, beto=0, bet_signal=0)
barelyknower[,"uIDs"] <- barelyknower[,"IDs"]+barelyknower[,"blnd"]/100
### switch bets for bet deltas
barelyknower[,"beto"] <- barelyknower[,"bet"]
barelyknower[,"bet"] <- betd 
barelyknower[,"bet_signal"] <- betd_signal
### get rid of O's, or don't.  yeah. Don't. DON'T
#barelyknower <- barelyknower[barelyknower[,"act"] != 38,]
### values of extrinsic are ternary (act=O)->0, low bet->1, and high bet=2.  
###  nice for us, there is a natural division that low and high bets are bets above and below the blind.
### later they can maybe be quaternary, taking into account both bet size and action
#by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2), 0, 0))
#barelyknower[,"extr1"] <- allbut(barelyknower[,"bet"], function(x) EqualFreq(x,2), 0, 0)
#barelyknower[,"extr1"] <- allbut(barelyknower[,"bet"], EqualFreq, but=0, repl=0, nbins=2)
### FORMER: barelyknower[,"extr1"] <- as.integer(discretize(barelyknower[,"bet"], nbins=2)[,1])
### FORMER: barelyknower[,"extr1"] <- ifelse(barelyknower[,"act"]==0, 0, barelyknower[,"extr1"])
### NOW: bin within blind, within street, within player on fractions of the pot at the moment of play
### before binning wager size, scale wagers into multiples of the size of pot at the point that it reaches each player
###    I can only approximate this for tables lacking seat order (>2 players): as the size of the pot at the end of the round.
barelyknower <- data.matrix({
    bk <- as.data.table(barelyknower)
    setkey(bk, hand, st, order_play, uIDs)
    bk[,':='(pot=.SD[,sum(beto)], potd=.SD[,sum(bet_signal)]),by=.(hand,st)]
    bk[seats == 2,':='(pot=pot-potd)] ### start by making pot what it was at beginning of round (instead of at the end)
    bk[seats == 2,':='(pot=c(pot[1], pot[1]+.SD[1,bet_signal])),by=.(hand,st)]  ## this depends on ordering by order_play.  the weird indexing is because I'm assigning a vector instead of two scalars, because I want access to all the information in the grouping.
    bk[seats == 2,':='(pot2=shift(pot + bet_signal, fill=pot[1])),by=.(hand,st)]  ## this depends on ordering by order_play.  the weird indexing is because I'm assigning a vector instead of two scalars, because I want access to all the information in the grouping.
    { ### testing
        #library(microbenchmark)
        #microbenchmark(bk[1:100000][seats == 2,':='(pot=c(pot[1], pot[1]+.SD[1,bet_signal])),by=.(hand,st)], bk[1:100000][seats == 2,':='(pot2=shift(pot + bet_signal, fill=pot)),by=.(hand,st)])
        #assert(all(bk[1:100000][seats == 2,':='(pot=c(pot[1], pot[1]+.SD[1,bet_signal])),by=.(hand,st)][,pot] == bk[1:100000][seats == 2,':='(pot2=shift(pot + bet_signal, fill=pot[1])),by=.(hand,st)][,pot]))
    }
    ### THEN: test to make sure that all new variables are non-null 
    ### NVRMND: ("pot" is more than the haul, because the haul doesn't include winnings from uncalled bets because it doesn't count them as decremented from your earnings until they are called) NVRMND: and also test that final pot is "equal" to haul (haul is less than but more than half  of pot; it may always be within one big blind of the pot; we'll see)
    assert( bk[,all(!is.na(pot))] )
    assert( bk[,all(!is.na(potd))] )
    bk[,bet_frac:=bet_signal/pot]
    bk[st==0,bet_frac:=ifelse(order_play==1,blnd/2,blnd)]
    assert( bk[st>0,all(!is.na(bet_frac))] )
    assert( bk[,all((bet_frac >= 0) | (game==2))] ) ### because of an edge case qurik, bets can look negative in absolute poker
    bk ### return value of this code block
})
rm(bk)
### NOW: bin hand strength (as pot fractions) within-street
### if you bet a little before folding, 
###   then you were taking a stand and those bets should get binned into your hand strength.  
###   This means that some actions that are fold (extr2) have non-zero extr1
###   I'm basing fractions on uncorrected (uncalled) bet because I'm not interested in how much the player lost (bet) but what they signaled. So i should include uncalled bets
barelyknower <- data.matrix({
    bk <- as.data.table(barelyknower)
    bk[,extr1:= 0] ### 0 for all bets of size zero
    if ( any(bk[st > 0 & bet_signal > 0,length(unique(quantile(bet_frac,probs=seq(0,1,length.out=3))))] != 3)) {
        print("PROBLEM ISHGFDD: WITH BIN UNQIEUNESS (bet discretization broken bc modal value is more than half the data)")
        print( bk[st > 0 & bet_signal > 0,quantile(x,probs=seq(0,1,length.out=3)),by=st]) 
        bk[st > 0 & bet_signal > 0,EqualFreq(bet_frac,nbins=2, sketchyUniquePatch=TRUE),by=st]
    } else {
        bk[st > 0 & bet_signal > 0,extr1:= EqualFreq(bet_frac,nbins=2),by=st]
    }
    bk[st==0,extr1:=ifelse( bet_frac == blnd/2, 1, 2)] ### preflop, on bets are small blind and big blind.  these get omitted later, but have to be accounted for now.
    print("before/after bins of singalled hand  strength (wagers) over all (old) and by street (new)")
    print( bk[st > 0 & bet_signal > 0,list(list(quantile(bet_frac,probs=seq(0,1,length.out=3))))])
    print( bk[order(st)][st > 0 & bet_signal > 0,list(list(quantile(bet_frac,probs=seq(0,1,length.out=3)))),by=st])
    print( bk[order(blnd,st,extr1),.N,by=.(st,extr1)])
    assert( bk[,all(!is.na(extr1))] )
    bk ### return value of this code block
})
rm(bk)
### also treat action as a signal of hand strength, for a parallel analysis in terms of action rather than bet size
barelyknower[,"extr2"] <- barelyknower[,"act2"]
#barelyknower[,"extr3"] <- ifelse(barelyknower[,"act"]==0, 0, barelyknower[,"extr1"])
print("extrinsic hand strength cut at")
table(EqualFreq(barelyknower[barelyknower[,"bet"] != 0, "bet"],nbins=2,asFactor=TRUE))
#table(EqualFreq(barelyknower[barelyknower[,"act"]!=0, "bet"],nbins=2,asFactor=TRUE))
table(barelyknower[,"extr1"])
#table(barelyknower[,"bet"])
min(barelyknower[barelyknower[,"extr1"]==2,"bet"])
max(barelyknower[barelyknower[,"extr1"]==2,"bet"])
min(barelyknower[barelyknower[,"extr1"]==1,"bet"])
max(barelyknower[barelyknower[,"extr1"]==1,"bet"])
min(barelyknower[barelyknower[,"extr1"]==0,"bet"])
max(barelyknower[barelyknower[,"extr1"]==0,"bet"])
sum(barelyknower[,"bet"] != 0)
#system.time(unlist(by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, EqualFreq, but=0, repl=0, nbins=2,asFactor=TRUE))))
#system.time(unlist(by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2,asFactor=TRUE), 0, 0))))
#table(EqualFreq(barelyknower[barelyknower[,"bet"]!=0,"bet"], 2,asFactor=TRUE))

### values of intrinsic are quaternary, out-and-invisible -> 0, in-and-invisible->1, bad hand=2 and good hand=3
### later they can maybe be quaternary, taking into account both bet size and action
barelyknower[,"intr"] <- allbut(barelyknower[,"rank1"], EqualFreq, but=-1, repl=0, nbins=2)
barelyknower[,"intr_prosp"] <- allbut(barelyknower[,"rank2"], EqualFreq, but=-1, repl=0, nbins=2, sketchyUniquePatch=TRUE)  #(this gives prospective value, but only for hands that went to showdown and didn't muck)
print("intrinsic strength cut at")
table(EqualFreq(barelyknower[barelyknower[,"rank1"] != -1, "rank1"],nbins=2,asFactor=TRUE))
#ee <- EqualFreq(barelyknower[barelyknower[,"rank1"]!=-1,"rank1"], 2,asFactor=TRUE)
### values of skill are binary, net lost money and net gained money (actually equal freq bins over dist of mean hand earnings)
#bkskill <- matrix(unlist(by(bktest[bktest[,"st"] == 4,],  bktest[bktest[,"st"] == 4,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), blnd=unique(x[,"blnd"]), chps=mean(x[,"chps"]), skill_real=mean(x[,"haul"] - x[,"bet"]), skill=0))), ncol=5, byrow=T)

### to aid looking at last streets, propagate size of eventually uncalled bet to all streets
bkdttmp <- as.data.table(barelyknower) ### make this show on all of that player's streets ### collect all instances of an uncalled bet ### looking at those players whose bets were uncalled and the hands they were uncalled in ### set all of that players streets to have that value of the uncalled bet
setkey(bkdttmp , hand, IDs)
plays_to_watch_out_for <- bkdttmp[uncalled_bet_size != 0, .(hand,IDs,uncalled_bet_size)]
setkey(plays_to_watch_out_for , hand, IDs)
#bkdttmp[J(plays_to_watch_out_for),sum(uncalled_bet_size==0)]
bkdttmp[,':='(uncalled_beto_size=0)]
bkdttmp[J(plays_to_watch_out_for),':='(uncalled_beto_size=uncalled_bet_size, uncalled_bet_size=-diff(sort(unique(beto), decreasing=TRUE)[1:2])),by=.(hand,IDs)]
bkdttmp[is.na(uncalled_bet_size),uncalled_bet_size:=0]
#bkdttmp[J(plays_to_watch_out_for),sum(uncalled_bet_size==0)]
#bkdttmp[,sum(uncalled_bet_size==0)]
### also use this to identify poeple going all in but not losing all their money because the winner went all in but had a smaller pot
###  thoe code below found 1000 (out of 9 million) rows at $6 blinds. together they set my finals off by not more than 700000. not that the analysis below depends on it, but for the curious, i didn't find this happening more than once in a hand (at, say, a gutsy multihand table). 
bkdttmp[,final_haul:=.SD[,max(haul)],by=hand]
bkdttmp[,beto_corrected:=beto]
### correction to final_haul/2 is an upper bound. actual losses could have been lower, but not realistically higher.  Depsite being an upper bound, this is a worthwhile, and possibly %100 accurate correction. ceiling is because the rake creates decimals that can be rounded up.
bkdttmp[(haul == 0) & (beto > final_haul),beto_corrected:=final_haul/2]
barelyknower <- as.matrix( bkdttmp[order(blnd, hand, IDs, st)])
rm(bkdttmp)
### resurface
#### calculating skill (based on actual wins/losses, not signals)
### look just at last streets
#st_idx <- (barelyknower[,"st"] == 4 ) & (barelyknower[,"seats"] == 2 )
st_idx <- (barelyknower[,"st"] == 4 )
### in calculating haul, don't subtract posted bet: it's already substracted by fpdb when it should be and not when it shouldn't be (when a bet was uncalled: https://github.com/sigmike/fpdb/commit/4c37877c7fea98e3911cb25280be9f041d22b94c)
bkskill <- matrix(unlist(by(barelyknower[st_idx,],  barelyknower[st_idx,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), IDs=unique(x[,"IDs"]), blnd=unique(x[,"blnd"]), skill_real=sum(x[,"haul"] - (x[,"beto_corrected"] - x[,"uncalled_bet_size"])), skill_real_abs=sum(abs(x[,"haul"] - (x[,"beto_corrected"] - x[,"uncalled_bet_size"]))), skill_real_uncorrected=sum(x[,"haul"]-x[,"beto"]), skill=0, hand_n=length(x[,"haul"])))), ncol=8, byrow=T)
colnames(bkskill) <- c("uIDs", "IDs", "blnd", "skill_real", "skill_real_abs", "skill_real_uncorrected", "skill", "hand_n")
print("if bets are a symbol of hand strength, this should be positive because of all ins, but if bets are a description of losses, this should be zero, or as close to as possible:")
apply(bkskill[,c("skill_real", "skill_real_abs", "skill_real_uncorrected")], 2, sum)#print("skill cut at")
### this makes three equal bins.  this is only sensible for two-player hands because winning money is more common here.
#bkskill[,"skill"] <-  as.integer(EqualFreq(bkskill[,"skill_real"], nbins=3,asFactor=TRUE))-1
### this code ensurs that all winners are in the top bin: important if they are rare
#bkskill[,"skill"] <-  ifelse( bkskill[,"skill_real"] <= 0 & bkskill[,"skill"] == 2, 1, bkskill[,"skill"] )
#bkskill[,"skill"] <-  ifelse( bkskill[,"skill_real"] > 0, 1, bkskill[,"skill_real"] )
#bkskill[,"skill"] <- allbut(bkskill[,"skill"], function(x)discretize(x, nbins=2)[,1], but=1, repl=3) - 1
table(EqualFreq(bkskill[,"skill_real"], nbins=3,asFactor=TRUE))
#table(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] )
#median(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] )
print("skill cut at 0")
bkskill[,"skill"] <-  ifelse( bkskill[,"hand_n"] < 100, 1, ifelse( bkskill[,"skill_real"] <= 0, 0, 2 ))
print(paste0( "# players: ", nrow(bkskill) ))
print(paste0( "# players with < 100 hands played: ", length(bkskill[bkskill[,"hand_n"] <100,"skill_real"] )))
print(paste0( "# profitable players: ", length(bkskill[bkskill[,"skill_real"] >= 0,"skill_real"] ) ))
print(paste0( "# profitable players with >= 100 hands: ", length(bkskill[bkskill[,"skill_real"] >= 0 & bkskill[,"hand_n"] >=100,"skill_real"] ) ))
print(paste0( "# unprofitable players with >= 100 hands: ", length(bkskill[bkskill[,"skill_real"] < 0 & bkskill[,"hand_n"] >=100,"skill_real"] )))
#table(discretize(bkskill[bkskill[,"skill_real"] <= 0, "skill_real"],nbins=2)[,1])
table(bkskill[, "skill"])
table(EqualFreq(bkskill[bkskill[,"skill_real"] <= 0, "skill_real"],nbins=2,asFactor=TRUE))
library(sqldf); bkskdf <- asdf(bkskill); sqldf("SELECT blnd, skill, COUNT(*) AS n_players, AVG(hand_n), MEDIAN(hand_n), MIN(hand_n), MAX(hand_n)  FROM bkskdf GROUP BY blnd, skill;")
#rm(bkskill)
#todays.date <- "29102013_01"
#save(barelyknower, file=paste("poker_data/", "HHprocessed_", todays.date, ".Rdata", sep=""))
write.csv(bkskill, file=file_output_player_counts)

### with dt
bk <- as.data.table(barelyknower)
setnames(bk, c("blnd", "st"), c("blind", "street"))
bks <- as.data.table(bkskill)
setnames(bks, c("skill", "blnd"), c("skil", "blind"))
#barelyknower <- merge(barelyknower, bkskill[,c("blnd", "uIDs", "skill")], by=c("blnd", "uIDs"))
bk <- merge(bk, bks[,c("blind", "uIDs", "skil"),with=F], by=c("blind", "uIDs"))
### XXX two player hack for backwards compatibility with slower method:
###  specifically, I only calculated order of play for 2 player tables, and have to have 1 values for each seat number in order to get the wide up-to-9-player-with-NA-cols-as-becessary structure for the next step fo the analysis
bk[order_play==0,order_play:=seat]
bkw <- dcast(bk, hand+seats+blind+street~order_play, value.var=c("skil", "intr", "extr1", "extr2"), sep='')
### rearrange things fo the old order
setcolorder(bkw,c(1:4,sapply(5:(5+9-1), function(x) seq(x,by=9,length.out=4))))
bkw <- as.matrix(bkw)


write.csv(bkw, file=file_output)
#write.csv(bkw, file=paste("poker_data/", "poker_dist", todays.date, ".csv", sep=""))


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
