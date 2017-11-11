#!/usr/bin/env Rscript
library(optparse)
library(plyr)
library(infotheo)

### flag parsing from command line
option_list <- list(
                        make_option(c("-f", "--file"), 
                                            help="input file (output of HH2csv.py) [default %default]")
                        )
opt <- parse_args(OptionParser(option_list=option_list))
file_input <- opt$file
#file_input <- "~/projecto_staid/poker_information/ABS005handssubset.csv"

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

print("loading all hands")
#barelyknower <- read.csv("poker_data/ABShands.csv", colClasses=list("character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "numeric", "numeric", "numeric", "numeric"))
#names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
#load(paste("mount_brutus/poker_information/poker_data/", "HHsubset_31102013_01.Rdata", sep='') )
#barelyknower <- read.csv("poker_data/ABShands.csv")
barelyknower <- read.csv(file_input, nrows=1000000)
names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
barelyknower <- barelyknower[barelyknower[,"seats"] == 2,]
### order by game, blind, hand, player, and street
#all(barelyknower == barelyknower[order(barelyknower[,1], barelyknower[,2], barelyknower[,3], barelyknower[,5], barelyknower[,4]),])
### reorder hands
#print("reordering hands")
#barelyknower <- barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),] ###XXX tmp
#all(barelyknower == barelyknower[order( barelyknower[,"blnd"], barelyknower[,"hand"], barelyknower[,"IDs"], barelyknower[,"st"]),]) 
#### filtering
print("filtering")
#for (i in 1:ncol(barelyknower)) { table(barelyknower[,i]) }
#table(barelyknower[,"act"])
barelyknower[,"game"] <- 0
barelyknower[,"blnd"] <- as.numeric(as.character(barelyknower[,"blnd"]))
barelyknower[,"chps"] <- as.numeric(as.character(barelyknower[,"chps"]))
#head(barelyknower$act, 200)
actions <- as.character( barelyknower[,"act"] )
#head(actions, 200)
actions <- sapply(actions, function(x) asc(substr(x, nchar(x), nchar(x))))
#head(actions, 200)
barelyknower[,"act"] <-  ifelse(actions == 79, 0, ifelse(actions == 70, 1, 2))
#head(barelyknower[,"act"], 200)
#@barelyknower[,"act"] <- as.numeric(barelyknower[,"act"])
#@head(barelyknower$act, 200)
#as.matrix(barelyknower[1:1000,])
barelyknower <- as.matrix(barelyknower[,])
#barelyknower <- barelyknower[barelyknower[, "hand"] != 30818900,]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c(3038982044, 3078729526, 0),] ### has FF
#barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c(3041569636, 3029517634, 3038982044, 3078729526, 0),]
#stopp
summary(barelyknower)
#str(barelyknower)
#table(barelyknower[,"st"])
#table(barelyknower[,"seats"])
#table(barelyknower[,"blnd"])
#table(barelyknower[,"rank2"])
#barelyknower <- barelyknower[barelyknower[, "hand"] %ni% c( 0, 30381660, 3025498550, 3038982044, 3044421138, 3078112502, 3078729526),]
bad_hand <- barelyknower[is.na(barelyknower[,"haul"]),"hand"]
barelyknower <- barelyknower[barelyknower[, "hand"] %ni% bad_hand,]

### binning begins
#bktest <- barelyknower[11461001:11471000,]
#barelyknower[,"rank2"] <- ifelse(barelyknower[,"rank2"] != 170, barelyknower[,"rank2"], 0)
barelyknower <- cbind(barelyknower, skill=0, extr1=0, extr2=0, intr=0, uIDs=0)
barelyknower[,"uIDs"] <- barelyknower[,"IDs"]+barelyknower[,"blnd"]/100

print("stats")
print("table sizes")
table(barelyknower[,"seats"])

print(table(barelyknower[,"bet"]))
barelyknower <- barelyknower[barelyknower[,"st"] == 0,]
print(table(barelyknower[,"bet"]))

### skill
st_idx <- barelyknower[,"st"] == 4
bkskill <- matrix(unlist(by(barelyknower[st_idx,],  barelyknower[st_idx,c( "uIDs")], function(x) c(uIDs=unique(x[,"uIDs"]), blnd=unique(x[,"blnd"]), skill_real=mean(x[,"haul"] - x[,"bet"]), skill=0, hand_n=length(x[,"haul"])))), ncol=5, byrow=T)
colnames(bkskill) <- c("uIDs", "blnd", "skill_real", "skill", "hand_n")
#bkskill[,"skill"] <-  as.integer(EqualFreq(bkskill[,"skill_real"], nbins=3))-1
bkskill[,"skill"] <-  ifelse( bkskill[,"skill_real"] > 0, 1, bkskill[,"skill_real"] )
bkskill[,"skill"] <- allbut(bkskill[,"skill"], function(x)discretize(x, nbins=2)[,1], but=1, repl=3)
quit()
print("median profits of winner")
barelyknower[1,"blnd"]
table(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] )
median(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] )
median(bkskill[bkskill[,"skill_real"] > 0,"skill_real"] ) / barelyknower[1,"blnd"]
print("skill cut at")
table(bkskill[, "skill"])
table(EqualFreq(bkskill[bkskill[,"skill_real"] <= 0, "skill_real"],nbins=2))
table(EqualFreq(bkskill[,"skill_real"], nbins=3))
#table(discretize(bkskill[bkskill[,"skill_real"] <= 0, "skill_real"],nbins=2)[,1])
#barelyknower <- merge(barelyknower, bkskill[,c(-3,-4)], by=c("blnd", "uIDs"))
#rm(bkskill)


### get rid of O's, or don't.  yeah. Don't. DON'T
#barelyknower <- barelyknower[barelyknower[,"act"] != 38,]
### values of extrinsic are ternary (act=O)->0, low bet->1, and high bet=2.  
###  nice for us, there is a natural division that low and high bets are bets above and below the blind.
### later they can maybe be quaternary, taking into account both bet size and action
#by(bktest[,"bet"], bktest[,"blnd"], function(x) allbut(x, function(x) EqualFreq(x,2), 0, 0))
#barelyknower[,"extr1"] <- allbut(barelyknower[,"bet"], function(x) EqualFreq(x,2), 0, 0)
barelyknower[,"extr1"] <- allbut(barelyknower[,"bet"], EqualFreq, but=0, repl=0, nbins=2)
barelyknower[,"extr2"] <- barelyknower[,"act"]
print("extrinsic hand strength cut at")
table(EqualFreq(barelyknower[barelyknower[,"bet"] != 0, "bet"],nbins=2))
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
#todays.date <- "29102013_01"
#save(barelyknower, file=paste("poker_data/", "HHprocessed_", todays.date, ".Rdata", sep=""))

### generate the actual distribution
