#!/usr/bin/env Rscript
#setwd("~/projecto/research_projects/poker_information/")
source("local_settings.r")
source(paste0(pathLocal, "info_decomp_fns006.r"))

library(data.table)
require(bit64) ### to suppress warning
library(sqldf)


###PATHS
increment_me <- "63"
path_poker_actions_data <- paste0(pathLocal, "hash_actions", "_", increment_me, "/")
#path_poker_wagers_data <- paste0(pathLocal, "hash_wagers", "_", increment_me, "/")
path_poker_unordered_data <- paste0(pathLocal, "wagers_unordered", "_", increment_me, "/")
path_poker_shownonly_data <- paste0(pathLocal, "wagers_shownhands", "_", increment_me, "/")

path_poker <- pathLocal
sk <- rbind( fread(paste0( path_poker, "distrPS0050playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0100playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0200playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0400playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0600playercounts.csv"))
           , fread(paste0( path_poker, "distrPS1000playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0025playercounts.csv"))
           )


print("TABLE 1")
### USE:
#print(names(sk))
#sqldf("SELECT blnd, skill, COUNT(hand_n), AVG(hand_n), MEDIAN(hand_n), MIN(hand_n), MAX(hand_n), ROUND(MIN(skill_real)), ROUND(MAX(skill_real))  FROM sk GROUP BY blnd, skill;")
View(sqldf("SELECT skill, COUNT(hand_n), ROUND(MIN(skill_real)), ROUND(MAX(skill_real)), MIN(hand_n), MAX(hand_n), AVG(hand_n), MEDIAN(hand_n)  FROM sk WHERE skill != 1 AND blnd >= 0.5 GROUP BY skill ;"))
View(sqldf("SELECT COUNT(hand_n), ROUND(MIN(skill_real)), ROUND(MAX(skill_real)), MIN(hand_n), MAX(hand_n) FROM sk WHERE blnd >= 0.5;"))
View(sqldf("SELECT COUNT(hand_n), ROUND(MIN(skill_real)), ROUND(MAX(skill_real)), MIN(hand_n), MAX(hand_n) FROM sk WHERE blnd >= 0.5;"))
sk[,length(unique(uIDs))]
sk[,length(unique(IDs))]

print("DESCRIPTIVE: % of data coming from players with more than 10,000 hands, (winners) , before and after excluding <100 ahnd players")
sk[,gt1000:=(hand_n > 1000)]
sk[,gt10000:=(hand_n > 10000)]
#sqldf("SELECT skill, sum(hand_n) FROM sk WHERE blnd >= 0.5 GROUP BY skill")
print("after excluding <100:")
sqldf("SELECT gt10000, sum(hand_n), count(hand_n) FROM sk WHERE skill > 1 AND blnd >= 0.5 GROUP BY gt10000;")
print("before excluding <100:")
sqldf("SELECT gt10000, sum(hand_n), count(hand_n) FROM sk WHERE skill >= 1 AND skill_real > 0 AND blnd >= 0.5 GROUP BY gt10000;")
print("")
print(paste0("total hands: ", sk[blnd >= 0.50, sum(hand_n)]))
print("(That is huge because it includes tables of all sizes, and because it counts each hand n times for table size n)")

load_game_data <- function(input){
    pdt <- fread(input)
	#print(names(pdt))
    #print(c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" ))
    names(pdt) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
    pdt[,hand:=as.character(hand)]
	return(pdt)
}
get_poker_player_stats <- function(pdt){
    ### do QC
    ### get values
    rtn <- get_player_count(pdt)
    return( rtn)
}
get_player_count <- function(pdt) {
    return( length( table( pdt[,.(nn=nrow(.SD[,uIDs,by=hand])),by=uIDs]$nn)))
}

#file_ABS_1000 <- paste(pathData, "poker_distributions_2p_ABS_1000_", increment_me, ".Rdata", sep='')
#file_ABS_0600 <- paste(pathData, "poker_distributions_2p_ABS_0600_", increment_me, ".Rdata", sep='')
#file_ABS_0400 <- paste(pathData, "poker_distributions_2p_ABS_0400_", increment_me, ".Rdata", sep='')
#file_ABS_0200 <- paste(pathData, "poker_distributions_2p_ABS_0200_", increment_me, ".Rdata", sep='')
#file_ABS_0100 <- paste(pathData, "poker_distributions_2p_ABS_0100_", increment_me, ".Rdata", sep='')
#file_ABS_0050 <- paste(pathData, "poker_distributions_2p_ABS_0050_", increment_me, ".Rdata", sep='')
file_PS_1000 <- paste(pathData, "PS1000pokerhandrank.csv", sep='/')
file_PS_0600 <- paste(pathData, "PS0600pokerhandrank.csv", sep='/')
file_PS_0400 <- paste(pathData, "PS0400pokerhandrank.csv", sep='/')
file_PS_0200 <- paste(pathData, "PS0200pokerhandrank.csv", sep='/')
file_PS_0100 <- paste(pathData, "PS0100pokerhandrank.csv", sep='/')
file_PS_0050 <- paste(pathData, "PS0050pokerhandrank.csv", sep='/')
file_PS_0025 <- paste(pathData, "PS0025pokerhandrank.csv", sep='/')
pdts <- list(
#pdt0600 = load_game_data(file_PS_0600),
pdt0400 = load_game_data(file_PS_0400),
#pdt0200 = load_game_data(file_PS_0200),
#pdt0100 = load_game_data(file_PS_0100),
#pdt0050 = load_game_data(file_PS_0050),
#pdt0025 = load_game_data(file_PS_0025),
pdt1000 = load_game_data(file_PS_1000)
)

print("going through blinds")
blinds_fine <- c("1000","0600","0400","0200", "0100", "0050", "0025")
blinds_coarse <- c("small", "large")
gt2players <- 0
gt2hands <- 0
eq2players_total <- 0
eq2hands_total <- 0
uids_big <- c()
hand_descs <- list()
for(i in 1:length(blinds_fine) ) {
	if (blinds_fine[i] != "0025") {
		file_PS <- paste0(pathData, "/", "PS", blinds_fine[i], "pokerhandrank.csv")
		pdt = load_game_data(file_PS)
	} else {
		pdt <- fread(paste0("zcat < ", pathData, "/", "PS", blinds_fine[i], "pokerhandrank.csv.gz"))
		names(pdt) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
		pdt[,hand:=as.character(hand)]
	}
	print(blinds_fine[i])
	#print( get_poker_player_stats(pdt) )
	print(pdt[seats==2,.(hands=length(unique(hand)), players=length(unique(IDs))), by=blnd])
	#print(pdt[seats==2,.(hands=length(unique(hand)), players=length(unique(IDs)))])
	#print(pdt[seats>2,.(hands=length(unique(hand)), players=length(unique(IDs)))])
	hand_descs[[i]] <- pdt[seats==2,.(hands=length(unique(hand)), players=length(unique(IDs))), by=blnd]
	gt2players <- gt2players + pdt[seats>2, length(unique(IDs))]
	gt2hands <- gt2hands + pdt[seats>2, length(unique(hand))]
	eq2players_total <- eq2players_total + pdt[seats==2, length(unique(IDs))]
	eq2hands_total <- eq2hands_total + pdt[seats==2, length(unique(hand))]
	uids_big <- unique(c(uids_big, pdt[,IDs]))
	rm(pdt)
}
print(paste0("number of players and hands at tables equal to 2: ", eq2players_total, " and ", eq2hands_total))
print(paste0("number of players and hands at tables larger tahn 2: ", gt2players, " and ", gt2hands))
print(paste0("number of players and hands at all tables: ", eq2players_total + gt2players, " and ", eq2hands_total+gt2hands))
print(paste0("number of unique players across blinds: ", length(unique(uids_big))))
print("main contents of table ST1:")
do.call(rbind, hand_descs)
