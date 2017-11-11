#!/usr/bin/env Rscript
source("~/projecto/research_projects/poker_information/info_decomp_fns006.r")

get_single_subdist <- function(dist4,d) { return( apply(dist4,d,sum)/sum( apply(dist4,d,sum) )) }
get_mean_subdist <- function(dist4list, d, facet) {
  dist_n <- length(dist4list)
  dist_temp <- rep(0,3)
  for (i in 1:dist_n) {
    #print(names(dist4list[[i]]))
    #print(facet)
    #print(d)
    ##print(dist4list[[i]][facet])
    #print(apply(dist4list[[i]][[facet]],d,sum))
    dist_temp <- get_single_subdist(dist4list[[i]][[facet]], d)
  }
  #print(sum(dist_temp))
  #print((dist_temp))
  return(round(signif(dist_temp/sum(dist_temp),3),3))
}

### these are not useful numbers for the paper, just for the code
get_poker_totals <- function(input_file) {
  load(file=input_file)
  print(c((sum(distributions[[1]]$sa)+sum(distributions[[1]]$fa)),(sum(distributions[[2]]$sa)+sum(distributions[[2]]$fa))))
  print(c(sum(distributions[[1]]$sa)/(sum(distributions[[1]]$sa)+sum(distributions[[1]]$fa)),sum(distributions[[2]]$sa)/(sum(distributions[[2]]$sa)+sum(distributions[[2]]$fa))))
}
get_poker_stats_on_dists <- function(input_file) {
  load(file=input_file)
  n_dists <- length(distributions)
  #print(c(get_mean_subdist(distributions, 1, "ff"), get_mean_subdist(distributions, 3, "ff"), get_mean_subdist(distributions, 2, "ff")))
  #print(c(get_mean_subdist(distributions, 1, "sf"), get_mean_subdist(distributions, 3, "sf"), get_mean_subdist(distributions, 2, "sf")))
  print(c(99, get_mean_subdist(distributions, 1, "fa"), 99, get_mean_subdist(distributions, 3, "fa"), 99, get_mean_subdist(distributions, 2, "fa")))
  print(c(99, get_mean_subdist(distributions, 1, "sa"), 99, get_mean_subdist(distributions, 3, "sa"), 99, get_mean_subdist(distributions, 2, "sa")))
  #return(distributions)
}

increment_me <- "040"
#increment_me <- "021"

path_poker <- "~/projecto/research_projects/poker_information/"
#file_ABS_1000 <- paste(path_poker, "poker_distributions_2p_ABS_1000_", increment_me, ".Rdata", sep='')
#file_ABS_0600 <- paste(path_poker, "poker_distributions_2p_ABS_0600_", increment_me, ".Rdata", sep='')
#file_ABS_0400 <- paste(path_poker, "poker_distributions_2p_ABS_0400_", increment_me, ".Rdata", sep='')
#file_ABS_0200 <- paste(path_poker, "poker_distributions_2p_ABS_0200_", increment_me, ".Rdata", sep='')
#file_ABS_0100 <- paste(path_poker, "poker_distributions_2p_ABS_0100_", increment_me, ".Rdata", sep='')
#file_ABS_0050 <- paste(path_poker, "poker_distributions_2p_ABS_0050_", increment_me, ".Rdata", sep='')
file_PS_1000 <- paste(path_poker, "poker_distributions_2p_PS_1000_", increment_me, ".Rdata", sep='')
file_PS_0600 <- paste(path_poker, "poker_distributions_2p_PS_0600_", increment_me, ".Rdata", sep='')
file_PS_0400 <- paste(path_poker, "poker_distributions_2p_PS_0400_", increment_me, ".Rdata", sep='')
file_PS_0200 <- paste(path_poker, "poker_distributions_2p_PS_0200_", increment_me, ".Rdata", sep='')
file_PS_0100 <- paste(path_poker, "poker_distributions_2p_PS_0100_", increment_me, ".Rdata", sep='')
file_PS_0050 <- paste(path_poker, "poker_distributions_2p_PS_0050_", increment_me, ".Rdata", sep='')
file_PS_0025 <- paste(path_poker, "poker_distributions_2p_PS_0025_", increment_me, ".Rdata", sep='')

get_poker_stats_on_dists (file_PS_0025)
get_poker_stats_on_dists(file_PS_0050)
get_poker_stats_on_dists(file_PS_0100)
get_poker_stats_on_dists(file_PS_0200)
get_poker_stats_on_dists(file_PS_0400)
get_poker_stats_on_dists(file_PS_0600)
get_poker_stats_on_dists(file_PS_1000)
get_poker_totals (file_PS_0025)
get_poker_totals(file_PS_0050)
get_poker_totals(file_PS_0100)
get_poker_totals(file_PS_0200)
get_poker_totals(file_PS_0400)
get_poker_totals(file_PS_0600)
get_poker_totals(file_PS_1000)


#expr `cat ../../../projecto_staid/poker_information/distrPS0025_4.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0050_2.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0100_2.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0200_2.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0400_2.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0600_2.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS1000_2.csv | egrep -o  "^[^,]*?,[^,]*?,2,.*$" | wc -l` / 5

#expr `cat ../../../projecto_staid/poker_information/distrPS0025_4.csv | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0050_2.csv | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0100_2.csv | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0200_2.csv | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0400_2.csv | wc -l` / 5
#expr `cat ../../../projecto_staid/poker_information/distrPS0600_2.csv | wc -l` / 5
##expr `cat ../../../projecto_staid/poker_information/distrPS1000_2.csv | wc -l` / 5
