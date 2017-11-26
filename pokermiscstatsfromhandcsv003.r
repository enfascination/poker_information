#!/usr/bin/env Rscript
source("local_settings.r")
source(paste0(pathLocal, "info_decomp_fns006.r"))

### PATHS
#increment_me <- "021"
#increment_me <- "040"
increment_me <- "63"

path_poker_actions_data <- paste0(pathLocal, "hash_actions", "_", increment_me, "/")
#path_poker_wagers_data <- paste0(pathLocal, "hash_wagers", "_", increment_me, "/")
path_poker_unordered_data <- paste0(pathLocal, "wagers_unordered", "_", increment_me, "/")
path_poker_shownonly_data <- paste0(pathLocal, "wagers_shownhands", "_", increment_me, "/")

file_PS_small <- paste(path_poker_actions_data, "poker_distributions_2p_PS_small", ".Rdata", sep='')
file_PS_large <- paste(path_poker_actions_data, "poker_distributions_2p_PS_large", ".Rdata", sep='')


### HELPERS
# dims, in order: action of p1, cards of p1, action of p2, cards of p2. 
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

get_poker_stats_on_dists <- function(input_file) {
  load(file=input_file)
  n_dists <- length(distributions)
  #print(c(get_mean_subdist(distributions, 1, "ff"), get_mean_subdist(distributions, 3, "ff"), get_mean_subdist(distributions, 2, "ff")))
  #print(c(get_mean_subdist(distributions, 1, "sf"), get_mean_subdist(distributions, 3, "sf"), get_mean_subdist(distributions, 2, "sf")))
  print("Fish: P1 action, P2 action, P1 cards")
  print(c(sum(get_mean_subdist(distributions, 1, "fa")), get_mean_subdist(distributions, 1, "fa"), sum(get_mean_subdist(distributions, 3, "fa")), get_mean_subdist(distributions, 3, "fa"), sum(get_mean_subdist(distributions, 2, "fa")), get_mean_subdist(distributions, 2, "fa")))
  print("Sharks: P1 action, P2 action, P1 cards")
  print(c(sum(get_mean_subdist(distributions, 1, "sa")), get_mean_subdist(distributions, 1, "sa"), sum(get_mean_subdist(distributions, 3, "sa")), get_mean_subdist(distributions, 3, "sa"), sum(get_mean_subdist(distributions, 2, "sa")), get_mean_subdist(distributions, 2, "sa")))
  #return(distributions)
}
### these are not useful numbers for the paper, just for the code
get_poker_totals <- function(input_file) {
  load(file=input_file)
  print(c((sum(distributions[[1]]$sa)+sum(distributions[[1]]$fa)),(sum(distributions[[2]]$sa)+sum(distributions[[2]]$fa))))
  print(c(sum(distributions[[1]]$sa)/(sum(distributions[[1]]$sa)+sum(distributions[[1]]$fa)),sum(distributions[[2]]$sa)/(sum(distributions[[2]]$sa)+sum(distributions[[2]]$fa))))
}

print("stats: small stakes")
print("       small stakes dists")
get_poker_stats_on_dists(file_PS_small)
#print("       small stakes totals")
#get_poker_totals(file_PS_small)
print("stats: large stakes")
print("       large stakes dists")
get_poker_stats_on_dists(file_PS_large)
#print("       large stakes totals")
#get_poker_totals(file_PS_large)


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
