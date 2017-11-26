#!/usr/bin/env Rscript
#setwd("~/projecto/research_projects/poker_information/")
source("local_settings.r")
source(paste0(pathLocal, "info_decomp_fns006.r"))

library(data.table)
require(bit64) ### to suppress warning
library(sqldf)


blinds_fine <- c("1000","0600","0400","0200", "0100", "0050", "0025")
blinds_coarse <- c("small", "large")

###PATHS
increment_me <- "63"
path_poker <- pathLocal
path_poker_actions_data <- paste0(pathLocal, "hash_actions", "_", increment_me, "/")
#path_poker_wagers_data <- paste0(pathLocal, "hash_wagers", "_", increment_me, "/")
path_poker_unordered_data <- paste0(pathLocal, "wagers_unordered", "_", increment_me, "/")
path_poker_shownonly_data <- paste0(pathLocal, "wagers_shownhands", "_", increment_me, "/")

dist_l  <- data.table()
for(ib in 1:(length(blinds_fine)-1)) { #exclude 0025 blinds
    print(blinds_fine[ib])
    in_file <- paste0(path_poker, "distrPS", blinds_fine[ib], ".csv")
	tmp <- fread(in_file)
	tmp[,hand:=as.integer64(hand)]
    print(paste0("number of hands: ", tmp[,length(unique(hand))]))
    print(paste0("rows: ", nrow(tmp)))
    dist_l <- rbind(dist_l, tmp)
	rm(tmp)
    print(paste0("total hands: ", dist_l[,length(unique(hand))]))
    print(paste0("total rows: ", nrow(dist_l)))
}


print("table ST2:")
# load  data
dist_lf <- as.data.table(dist_l_filtering_2p(data.matrix(dist_l), extrAsWagerOrAction="actions", keepID=TRUE) )
dist_lf[,.N,by=street]
View(dist_lf[,.N,by=street])
as.data.table(dist_l)[seats==2,length(unique(hand))]
dist_lf[,length(unique(hand))]
