#!/usr/bin/env Rscript

source("~/projecto/research_projects/poker_information/info_decomp_fns.r")

reps <- 60
increment_me <- "015"
hand_path <- "~/projecto/research_projects/poker_information/"

#statistics_3p_ABS_1000 <- build_poker_distribution_3p(read.csv("~/projecto/research_projects/poker_information/distrABS100.csv"), reps)
#save(statistics_3p_ABS_1000 , file=paste("~/projecto/research_projects/poker_information/3pdiststatistics_", increment_me, ".Rdata", sep=''))
#stat_long_100_A <- build_stat_long_3p(statistics_3p_ABS_1000 )
#save(stat_long_100_A , file=paste("~/projecto/research_projects/poker_information/poker_graph_3p_ready_data_", increment_me, ".Rdata", sep=''))


print("ABS 1000"); Sys.time()
#statistics_3p_ABS_1000 <- build_poker_distribution_3p(read.csv(paste(hand_path, "distrABS100.csv", sep='')), reps, merge_middle=T)
print("ABS 0600"); Sys.time()
#statistics_3p_ABS_0600 <- build_poker_distribution_3p(read.csv(paste(hand_path, "distrABS060.csv", sep='')), reps, merge_middle=T)
print("ABS 0400"); Sys.time()
#statistics_3p_ABS_0400 <- build_poker_distribution_3p(read.csv(paste(hand_path, "distrABS040.csv", sep='')), reps, merge_middle=T)
print("ABS 0200"); Sys.time()
#statistics_3p_ABS_0200 <- build_poker_distribution_3p(read.csv(paste(hand_path, "distrABS020.csv", sep='')), reps, merge_middle=T)
print("ABS 0100"); Sys.time()
#statistics_3p_ABS_0100 <- build_poker_distribution_3p(read.csv(paste(hand_path, "distrABS010.csv", sep='')), reps, merge_middle=T)
print("ABS 0050"); Sys.time()
#statistics_3p_ABS_0050 <- build_poker_distribution_3p(read.csv(paste(hand_path, "distrABS005.csv", sep='')), reps, merge_middle=T)
print("PS 1000"); Sys.time()
statistics_3p_PS_1000  <- build_poker_distribution_3p(read.csv("~/projecto_staid/poker_information/distrPS1000.csv"), reps, merge_middle=T)
print("PS 0600"); Sys.time()
statistics_3p_PS_0600  <- build_poker_distribution_3p(read.csv("~/projecto_staid/poker_information/distrPS0600.csv"), reps, merge_middle=T)
print("PS 0400"); Sys.time()
statistics_3p_PS_0400  <- build_poker_distribution_3p(read.csv("~/projecto_staid/poker_information/distrPS0400.csv"), reps, merge_middle=T)
print("PS 0200"); Sys.time()
statistics_3p_PS_0200  <- build_poker_distribution_3p(read.csv("~/projecto_staid/poker_information/distrPS0200.csv"), reps, merge_middle=T)
print("PS 0100"); Sys.time()
statistics_3p_PS_0100  <- build_poker_distribution_3p(read.csv("~/projecto_staid/poker_information/distrPS0100.csv"), reps, merge_middle=T)
print("PS 0050"); Sys.time()
statistics_3p_PS_0050  <- build_poker_distribution_3p(read.csv("~/projecto_staid/poker_information/distrPS0050.csv"), reps, merge_middle=T)

save(statistics_3p_ABS_1000 , file=paste(hand_path, "3pdiststatistics_ABS1000_", increment_me, ".Rdata", sep=''))
save(statistics_3p_ABS_0600 , file=paste(hand_path, "3pdiststatistics_ABS0600_", increment_me, ".Rdata", sep=''))
save(statistics_3p_ABS_0400 , file=paste(hand_path, "3pdiststatistics_ABS0400_", increment_me, ".Rdata", sep=''))
save(statistics_3p_ABS_0200 , file=paste(hand_path, "3pdiststatistics_ABS0200_", increment_me, ".Rdata", sep=''))
save(statistics_3p_ABS_0100 , file=paste(hand_path, "3pdiststatistics_ABS0100_", increment_me, ".Rdata", sep=''))
save(statistics_3p_ABS_0050 , file=paste(hand_path, "3pdiststatistics_ABS0050_", increment_me, ".Rdata", sep=''))
save(statistics_3p_PS_1000 , file=paste(hand_path, "3pdiststatistics_PS1000_", increment_me, ".Rdata", sep=''))
save(statistics_3p_PS_0600 , file=paste(hand_path, "3pdiststatistics_PS0600_", increment_me, ".Rdata", sep=''))
save(statistics_3p_PS_0400 , file=paste(hand_path, "3pdiststatistics_PS0400_", increment_me, ".Rdata", sep=''))
save(statistics_3p_PS_0200 , file=paste(hand_path, "3pdiststatistics_PS0200_", increment_me, ".Rdata", sep=''))
save(statistics_3p_PS_0100 , file=paste(hand_path, "3pdiststatistics_PS0100_", increment_me, ".Rdata", sep=''))
save(statistics_3p_PS_0050 , file=paste(hand_path, "3pdiststatistics_PS0050_", increment_me, ".Rdata", sep=''))

print("long form"); Sys.time()
stat_long_100_A <- build_stat_long_3p(statistics_3p_ABS_1000 )
stat_long_060_A <- build_stat_long_3p(statistics_3p_ABS_0600 )
stat_long_040_A <- build_stat_long_3p(statistics_3p_ABS_0400 )
stat_long_020_A <- build_stat_long_3p(statistics_3p_ABS_0200 )
stat_long_010_A <- build_stat_long_3p(statistics_3p_ABS_0100 )
stat_long_005_A <- build_stat_long_3p(statistics_3p_ABS_0050 )
stat_long_100_P <- build_stat_long_3p(statistics_3p_PS_1000 )
stat_long_060_P <- build_stat_long_3p(statistics_3p_PS_0600 )
stat_long_040_P <- build_stat_long_3p(statistics_3p_PS_0400 )
stat_long_020_P <- build_stat_long_3p(statistics_3p_PS_0200 )
stat_long_010_P <- build_stat_long_3p(statistics_3p_PS_0100 )
stat_long_005_P <- build_stat_long_3p(statistics_3p_PS_0050 )
print("long form done"); Sys.time()
stat_3p_long <- rbind( cbind(stat_long_100_A, blind=10.00, ublind="ABS10.00", site="ABS") ,
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
save(stat_3p_long , file=paste(hand_path, "poker_graph_3p_ready_data_", increment_me, ".Rdata", sep=''))
Sys.time()


