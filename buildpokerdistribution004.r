#!/usr/bin/env Rscript
source("~/projecto/research_projects/poker_information/info_decomp_fns002.r")




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

reps <- 1
increment_me <- "017"

specific <- 1
print("ABS 1000"); Sys.time()
statistics_2p_ABS_1000 <- bootstrap_statistics_2p(read.csv("~/projecto/research_projects/poker_information/distrABS100.csv"), reps)
print("ABS 0600"); Sys.time()
statistics_2p_ABS_0600 <- bootstrap_statistics_2p(read.csv("~/projecto/research_projects/poker_information/distrABS060.csv"), reps)
print("ABS 0400"); Sys.time()
statistics_2p_ABS_0400 <- bootstrap_statistics_2p(read.csv("~/projecto/research_projects/poker_information/distrABS040.csv"), reps)
print("ABS 0200"); Sys.time()
statistics_2p_ABS_0200 <- bootstrap_statistics_2p(read.csv("~/projecto/research_projects/poker_information/distrABS020.csv"), reps)
print("ABS 0100"); Sys.time()
statistics_2p_ABS_0100 <- bootstrap_statistics_2p(read.csv("~/projecto/research_projects/poker_information/distrABS010.csv"), reps)
print("ABS 0050"); Sys.time()
statistics_2p_ABS_0050 <- bootstrap_statistics_2p(read.csv("~/projecto/research_projects/poker_information/distrABS005.csv"), reps)
#print("PS 1000"); Sys.time()
#statistics_2p_PS_1000  <- bootstrap_statistics_2p(read.csv("~/projecto_staid/poker_information/distrPS1000.csv"), reps)
#print("PS 0600"); Sys.time()
#statistics_2p_PS_0600  <- bootstrap_statistics_2p(read.csv("~/projecto_staid/poker_information/distrPS0600.csv"), reps)
#print("PS 0400"); Sys.time()
#statistics_2p_PS_0400  <- bootstrap_statistics_2p(read.csv("~/projecto_staid/poker_information/distrPS0400.csv"), reps)
#print("PS 0200"); Sys.time()
#statistics_2p_PS_0200  <- bootstrap_statistics_2p(read.csv("~/projecto_staid/poker_information/distrPS0200.csv"), reps)
#print("PS 0100"); Sys.time()
#statistics_2p_PS_0100  <- bootstrap_statistics_2p(read.csv("~/projecto_staid/poker_information/distrPS0100.csv"), reps)
#print("PS 0050"); Sys.time()
#statistics_2p_PS_0050  <- bootstrap_statistics_2p(read.csv("~/projecto_staid/poker_information/distrPS0050.csv"), reps)

print("long form"); Sys.time()
stat_long_100_A <- build_stat_long_2p(statistics_2p_ABS_1000 )
stat_long_060_A <- build_stat_long_2p(statistics_2p_ABS_0600 )
stat_long_040_A <- build_stat_long_2p(statistics_2p_ABS_0400 )
stat_long_020_A <- build_stat_long_2p(statistics_2p_ABS_0200 )
stat_long_010_A <- build_stat_long_2p(statistics_2p_ABS_0100 )
stat_long_005_A <- build_stat_long_2p(statistics_2p_ABS_0050 )
#stat_long_100_P <- build_stat_long_2p(statistics_2p_PS_1000 )
#stat_long_060_P <- build_stat_long_2p(statistics_2p_PS_0600 )
#stat_long_040_P <- build_stat_long_2p(statistics_2p_PS_0400 )
#stat_long_020_P <- build_stat_long_2p(statistics_2p_PS_0200 )
#stat_long_010_P <- build_stat_long_2p(statistics_2p_PS_0100 )
#stat_long_005_P <- build_stat_long_2p(statistics_2p_PS_0050 )
print("long form done"); Sys.time()
stat_long <- rbind( cbind(stat_long_100_A, blind=10.00, ublind="ABS10.00", site="ABS") ,
                    cbind(stat_long_060_A, blind=6.00, ublind="ABS6.00", site="ABS") ,
                    cbind(stat_long_040_A, blind=4.00, ublind="ABS4.00", site="ABS") ,
                    cbind(stat_long_020_A, blind=2.00, ublind="ABS2.00", site="ABS") ,
                    cbind(stat_long_010_A, blind=1.00, ublind="ABS1.00", site="ABS") ,
                    cbind(stat_long_005_A, blind=0.50, ublind="ABS0.50", site="ABS") #,
                    #cbind(stat_long_100_P, blind=10.00, ublind="PS10.00", site="PS") ,
                    #cbind(stat_long_060_P, blind=6.00, ublind="PS6.00", site="PS") ,
                    #cbind(stat_long_040_P, blind=4.00, ublind="PS4.00", site="PS") ,
                    #cbind(stat_long_020_P, blind=2.00, ublind="PS2.00", site="PS") ,
                    #cbind(stat_long_010_P, blind=1.00, ublind="PS1.00", site="PS") ,
                    #cbind(stat_long_005_P, blind=0.50, ublind="PS0.50", site="PS") 
                    )

Sys.time()
save(stat_long , file=paste("~/projecto/research_projects/poker_information/poker_graph_ready_data_", increment_me, ".Rdata", sep=''))
Sys.time()

### print("specific info")
print("specific info")

specific <- 1
### filter
print("ABS 1000"); Sys.time()
dist_l_ABS_1000 <- dist_l_filtering_2p( read.csv("~/projecto/research_projects/poker_information/distrABS100.csv") ) 
dist_2p_ABS_1000 <- build_poker_distribution_2p(dist_l_ABS_1000, 1:nrow(dist_2p_ABS_1000))
statistics_2p_ABS_1000_spec1 <- get_statistics_2p(dist_2p_ABS_1000$ss, dist_2p_ABS_1000$sf, dist_2p_ABS_1000$ff, specific=1 )
stat_long_1000_ABS_spec1 <- cbind(build_stat_long_2p(statistics_2p_ABS_1000_spec1 ), blind=10.00, ublind="ABS10.00", site="ABS", specific=1) 
statistics_2p_ABS_1000_spec2 <- get_statistics_2p(dist_2p_ABS_1000$ss, dist_2p_ABS_1000$sf, dist_2p_ABS_1000$ff, specific=2 )
stat_long_1000_ABS_spec2 <- cbind(build_stat_long_2p(statistics_2p_ABS_1000_spec2 ), blind=10.00, ublind="ABS10.00", site="ABS", specific=2) 
statistics_2p_ABS_1000_spec3 <- get_statistics_2p(dist_2p_ABS_1000$ss, dist_2p_ABS_1000$sf, dist_2p_ABS_1000$ff, specific=3 )
stat_long_1000_ABS_spec3 <- cbind(build_stat_long_2p(statistics_2p_ABS_1000_spec3 ), blind=10.00, ublind="ABS10.00", site="ABS", specific=3) 
stat_long_1000_ABS_spec <- rbind(stat_long_1000_ABS_spec1,  stat_long_1000_ABS_spec2,  stat_long_1000_ABS_spec3)
print("ABS 0050"); Sys.time()
dist_l_ABS_0050 <- dist_l_filtering_2p( read.csv("~/projecto/research_projects/poker_information/distrABS005.csv") ) 
dist_2p_ABS_0050 <- build_poker_distribution_2p(dist_l_ABS_0050, 1:nrow(dist_2p_ABS_0050))
statistics_2p_ABS_0050_spec1 <- get_statistics_2p(dist_2p_ABS_0050$ss, dist_2p_ABS_0050$sf, dist_2p_ABS_0050$ff, specific=1 )
stat_long_0050_ABS_spec1 <- cbind(build_stat_long_2p(statistics_2p_ABS_0050_spec1 ), blind=0.50, ublind="ABS0.50", site="ABS", specific=1) 
statistics_2p_ABS_0050_spec2 <- get_statistics_2p(dist_2p_ABS_0050$ss, dist_2p_ABS_0050$sf, dist_2p_ABS_0050$ff, specific=2 )
stat_long_0050_ABS_spec2 <- cbind(build_stat_long_2p(statistics_2p_ABS_0050_spec2 ), blind=0.50, ublind="ABS0.50", site="ABS", specific=2) 
statistics_2p_ABS_0050_spec3 <- get_statistics_2p(dist_2p_ABS_0050$ss, dist_2p_ABS_0050$sf, dist_2p_ABS_0050$ff, specific=3 )
stat_long_0050_ABS_spec3 <- cbind(build_stat_long_2p(statistics_2p_ABS_0050_spec3 ), blind=0.50, ublind="ABS0.50", site="ABS", specific=3) 
stat_long_0050_ABS_spec <- rbind(stat_long_0050_ABS_spec1,  stat_long_0050_ABS_spec2,  stat_long_0050_ABS_spec3)
stat_long_ABS_spec <- rbind(stat_long_1000_ABS_spec , stat_long_0050_ABS_spec )
