#!/usr/bin/env Rscript
source("~/projecto/research_projects/poker_information/info_decomp_fns.r")




### upload data
#dist_l  <- read.csv("~/projecto/research_projects/poker_information/distrABS005.csv")
#dist_l2 <- read.csv("~/projecto/research_projects/poker_information/distrABS010.csv")
#dist_l <- rbind(dist_l, dist_l2)
#rm(dist_l2)
#dist_l  <- read.csv("~/projecto/research_projects/poker_information/distrABS100.csv")
#dist_l2 <- read.csv("~/projecto/research_projects/poker_information/distrABS060.csv")
#dist_l3 <- read.csv("~/projecto/research_projects/poker_information/distrABS040.csv")
#dist_l <- rbind(dist_l, dist_l2, dist_l3)
#rm(dist_l2)
#rm(dist_l3)

reps <- 60
increment_me <- "014"

print("ABS 1000"); Sys.time()
statistics_2p_ABS_1000 <- build_poker_distribution_2p(read.csv("~/projecto/research_projects/poker_information/distrABS100.csv"), reps)
print("ABS 0600"); Sys.time()
statistics_2p_ABS_0600 <- build_poker_distribution_2p(read.csv("~/projecto/research_projects/poker_information/distrABS060.csv"), reps)
print("ABS 0400"); Sys.time()
statistics_2p_ABS_0400 <- build_poker_distribution_2p(read.csv("~/projecto/research_projects/poker_information/distrABS040.csv"), reps)
print("ABS 0200"); Sys.time()
statistics_2p_ABS_0200 <- build_poker_distribution_2p(read.csv("~/projecto/research_projects/poker_information/distrABS020.csv"), reps)
print("ABS 0100"); Sys.time()
statistics_2p_ABS_0100 <- build_poker_distribution_2p(read.csv("~/projecto/research_projects/poker_information/distrABS010.csv"), reps)
print("ABS 0050"); Sys.time()
statistics_2p_ABS_0050 <- build_poker_distribution_2p(read.csv("~/projecto/research_projects/poker_information/distrABS005.csv"), reps)
print("PS 1000"); Sys.time()
statistics_2p_PS_1000  <- build_poker_distribution_2p(read.csv("~/projecto_staid/poker_information/distrPS1000.csv"), reps)
print("PS 0600"); Sys.time()
statistics_2p_PS_0600  <- build_poker_distribution_2p(read.csv("~/projecto_staid/poker_information/distrPS0600.csv"), reps)
print("PS 0400"); Sys.time()
statistics_2p_PS_0400  <- build_poker_distribution_2p(read.csv("~/projecto_staid/poker_information/distrPS0400.csv"), reps)
print("PS 0200"); Sys.time()
statistics_2p_PS_0200  <- build_poker_distribution_2p(read.csv("~/projecto_staid/poker_information/distrPS0200.csv"), reps)
print("PS 0100"); Sys.time()
statistics_2p_PS_0100  <- build_poker_distribution_2p(read.csv("~/projecto_staid/poker_information/distrPS0100.csv"), reps)
print("PS 0050"); Sys.time()
statistics_2p_PS_0050  <- build_poker_distribution_2p(read.csv("~/projecto_staid/poker_information/distrPS0050.csv"), reps)

print("long form"); Sys.time()
stat_long_100_A <- build_stat_long_2p(statistics_2p_ABS_1000 )
stat_long_060_A <- build_stat_long_2p(statistics_2p_ABS_0600 )
stat_long_040_A <- build_stat_long_2p(statistics_2p_ABS_0400 )
stat_long_020_A <- build_stat_long_2p(statistics_2p_ABS_0200 )
stat_long_010_A <- build_stat_long_2p(statistics_2p_ABS_0100 )
stat_long_005_A <- build_stat_long_2p(statistics_2p_ABS_0050 )
stat_long_100_P <- build_stat_long_2p(statistics_2p_PS_1000 )
stat_long_060_P <- build_stat_long_2p(statistics_2p_PS_0600 )
stat_long_040_P <- build_stat_long_2p(statistics_2p_PS_0400 )
stat_long_020_P <- build_stat_long_2p(statistics_2p_PS_0200 )
stat_long_010_P <- build_stat_long_2p(statistics_2p_PS_0100 )
stat_long_005_P <- build_stat_long_2p(statistics_2p_PS_0050 )
print("long form done"); Sys.time()
stat_long <- rbind( cbind(stat_long_100_A, blind=10.00, ublind="ABS10.00", site="ABS") ,
                    cbind(stat_long_060_A, blind=6.00, ublind="ABS6.00", site="ABS") ,
                    cbind(stat_long_040_A, blind=4.00, ublind="ABS4.00", site="ABS") ,
                    cbind(stat_long_020_A, blind=2.00, ublind="ABS2.00", site="ABS") ,
                    cbind(stat_long_010_A, blind=1.00, ublind="ABS1.00", site="ABS") ,
                    cbind(stat_long_005_A, blind=0.50, ublind="ABS0.50", site="ABS") ,
                    cbind(stat_long_100_P, blind=10.00, ublind="PS10.00", site="PS") ,
                    cbind(stat_long_060_P, blind=6.00, ublind="PS6.00", site="PS") ,
                    cbind(stat_long_040_P, blind=4.00, ublind="PS4.00", site="PS") ,
                    cbind(stat_long_020_P, blind=2.00, ublind="PS2.00", site="PS") ,
                    cbind(stat_long_010_P, blind=1.00, ublind="PS1.00", site="PS") ,
                    cbind(stat_long_005_P, blind=0.50, ublind="PS0.50", site="PS") 
                    )

Sys.time()
save(stat_long , file=paste("~/projecto/research_projects/poker_information/poker_graph_ready_data_", increment_me, ".Rdata", sep=''))
Sys.time()

### setup
#test <- array(0, c(3,3,3,3))
#test[1,2,3,2] <- 1
#idx <- c(1,2,3,2)
### things that work 
#test[1,2,3,2]
#test[t(idx)]
#test[rbind(idx)]
#test[matrix(idx, nrow=1)]
### timing
#system.time(for (i in 1:1000000) test[1,2,3,2] )
#system.time(for (i in 1:1000000) test[rbind(idx)] )
#system.time(for (i in 1:1000000) test[matrix(idx, nrow=1)] )
#system.time(for (i in 1:1000000) test[t(idx)] )
### two vectors
#idx1 <- c(1,2)
#idx2 <- c(3,2)
### things that work
#system.time(for (i in 1:1000000) test[1,2,3,2] )
#system.time(for (i in 1:1000000) test[rbind(c(idx1, idx2))] )
#system.time(for (i in 1:1000000) test[matrix(c(idx1, idx2), nrow=1)] )
#system.time(for (i in 1:1000000) test[t(c(idx1, idx2))] )


### populating distribution testa with observations test_idxs
#test_idxs <- matrix(sample(c(1,2,3), 300000, repl=T), ncol=3)
#testa_for_looped <- array(0, c(3,3,3))
#testa_vectorized <- array(0, c(3,3,3))
#system.time( for (i in 1:nrow(test_idxs)) { testa_for_looped[rbind(test_idxs[i,])] <- testa_for_looped[rbind(test_idxs[i,])] + 1 } )  ## slower
#system.time( testa_vectorized[test_idxs] <- testa_vectorized[test_idxs] + 1  ) ### faster
#sum(testa_for_looped) ### right
#sum(testa_vectorized) ### wrong




#for (e in 1:nrow(entropies)) {
  #dist_2p_ff <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  #dist_2p_sf <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  #dist_2p_ss <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  #dist_1p_b <- array(0, dim=c(2,3,3), dimnames=c("skill", "extr", "intr"))
  #if (1 %in% scales) {
    #idxs1 <- dist_l[,c("skil1", "extr1", "intr1")]+1
    #idxs2 <- dist_l[,c("skil2", "extr2", "intr2")]+1
    #dist_1p_b[idxs1] <- dist_1p_b[idxs1] + 1 
    #dist_1p_b[idxs2] <- dist_1p_b[idxs2] + 1 
  #}
  ##entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]/sum(dist_1p_b[1,,])), entropy_adist(dist_1p_b[2,,]/sum(dist_1p_b[2,,])))
  #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]), entropy_adist(dist_1p_b[2,,]), entropy_adist(dist_2p_ff), entropy_adist(dist_2p_sf), entropy_adist(dist_2p_ss))
#}
