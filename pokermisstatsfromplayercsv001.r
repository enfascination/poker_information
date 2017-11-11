#!/usr/bin/env Rscript
source("~/projecto/research_projects/poker_information/info_decomp_fns006.r")

library(data.table)
require(bit64) ### to suppress warning

path_poker <- "~/projecto/research_projects/poker_information/"
sk <- rbind( fread(paste0( path_poker, "distrPS0050playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0100playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0200playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0400playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0600playercounts.csv"))
           , fread(paste0( path_poker, "distrPS1000playercounts.csv"))
           , fread(paste0( path_poker, "distrPS0025playercounts.csv"))
           )


### USE:
### library(sqldf)
sqldf("SELECT blnd, skill, AVG(hand_n), MEDIAN(hand_n), MIN(hand_n), MAX(hand_n)  FROM sk GROUP BY blnd, skill;")

get_poker_player_stats <- function(input){
    pdt <- fread(input)
    names(pdt) <- c("game", "blnd", "hand", "seats", "st", "upid", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
    pdt[,hand:=as.character(hand)]
    ### do QC
    ### get values
    rtn <- get_player_count(pdt)
    return( rtn)
}
get_player_count <- function(pdt) {
    return( table( pdt[,.(nn=nrow(.SD[,upid,by=hand])),by=upid]$nn))
}

path_poker <- "~/projecto/research_projects/poker_information/"
#file_ABS_1000 <- paste(path_poker, "poker_distributions_2p_ABS_1000_", increment_me, ".Rdata", sep='')
#file_ABS_0600 <- paste(path_poker, "poker_distributions_2p_ABS_0600_", increment_me, ".Rdata", sep='')
#file_ABS_0400 <- paste(path_poker, "poker_distributions_2p_ABS_0400_", increment_me, ".Rdata", sep='')
#file_ABS_0200 <- paste(path_poker, "poker_distributions_2p_ABS_0200_", increment_me, ".Rdata", sep='')
#file_ABS_0100 <- paste(path_poker, "poker_distributions_2p_ABS_0100_", increment_me, ".Rdata", sep='')
#file_ABS_0050 <- paste(path_poker, "poker_distributions_2p_ABS_0050_", increment_me, ".Rdata", sep='')
file_PS_1000 <- paste(path_poker, "poker_distributions_2p_PS_1000_", sep='')
file_PS_0600 <- paste(path_poker, "poker_distributions_2p_PS_0600_", sep='')
file_PS_0400 <- paste(path_poker, "PS727pokerhandrank.csv", sep='')
input <- file_PS_0400 
file_PS_0200 <- paste(path_poker, "poker_distributions_2p_PS_0200_", sep='')
file_PS_0100 <- paste(path_poker, "poker_distributions_2p_PS_0100_", sep='')
file_PS_0050 <- paste(path_poker, "poker_distributions_2p_PS_0050_", sep='')
file_PS_0025 <- paste(path_poker, "poker_distributions_2p_PS_0025_", sep='')

#get_poker_stats_on_dists (file_PS_0025)
#get_poker_stats_on_dists(file_PS_0050)
#get_poker_stats_on_dists(file_PS_0100)
#get_poker_stats_on_dists(file_PS_0200)
print( get_poker_player_stats(file_PS_0400) )
#get_poker_stats_on_dists(file_PS_0600)
#get_poker_stats_on_dists(file_PS_1000)
