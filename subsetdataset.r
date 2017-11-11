
barelyknower <- read.csv("mount_brutus/poker_information/poker_data/ABShands.csv", nrow=1700000)
names(barelyknower) <- c("game", "blnd", "hand", "seats", "st", "IDs", "chps", "act", "bet", "haul", "rank2", "rank1" )
sample_size <- 50000
j <- 0
current_hand <- 0
for (i in 1:nrow(barelyknower)) {
  if (current_hand != barelyknower[i,"hand"]) { 
    j = j + 1 
    current_hand <- barelyknower[i,"hand"]
  }
  if (j == sample_size) { break }
}
barelyknower <- barelyknower[1:(i-1),]
todays.date <- "31102013_01"
save(barelyknower, file=paste("mount_brutus/poker_information/poker_data/", "HHsubset_", todays.date, ".Rdata", sep=""))
