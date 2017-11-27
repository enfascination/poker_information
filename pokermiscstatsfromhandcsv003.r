#!/usr/bin/env Rscript
source("local_settings.r")
source(paste0(pathLocal, "info_decomp_fns006.r"))

### LIBS
library(tidyr)
library(ggplot2)
library(gridExtra)
library(ggthemes)
`%ni%` = Negate(`%in%`) ### help functions
options(gsubfn.engine = "R")
library(sqldf)

### PATHS
#increment_me <- "021"
#increment_me <- "040"
increment_me <- "63"

path_poker_actions_data <- paste0(pathLocal, "hash_actions", "_", increment_me, "/")
#path_poker_wagers_data <- paste0(pathLocal, "hash_wagers", "_", increment_me, "/")
path_poker_unordered_data <- paste0(pathLocal, "wagers_unordered", "_", increment_me, "/")
path_poker_shownonly_data <- paste0(pathLocal, "wagers_shownhands", "_", increment_me, "/")
path_poker_images <- paste0(pathLocal, "images/", "hash_actions", "_", increment_me, "/")

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


### now start prepping data for plot of this distributional information
load(file=file_PS_large)
d <- 2 #dist 1:4 is W1, P1, W2, P2
facet <- "sa" #sa, fa, and some others
dist_n <- length(distributions)
dist_temp <- rep(0,3)
for (i in 1:dist_n) {
#print(names(dist4list[[i]]))
#print(facet)
#print(d)
##print(dist4list[[i]][facet])
#print(apply(dist4list[[i]][[facet]],d,sum))
dist_temp <- get_single_subdist(distributions[[i]][[facet]], d)
}
#print(sum(dist_temp))
#print((dist_temp))
mean <- (round(signif(dist_temp/sum(dist_temp),3),3))
dtsampw <- data.table(do.call(rbind, lapply(distributions,  function(osample) {
			rbind(
	c(player=2, c(apply(osample$sa,1,sum)/sum( apply(osample$sa,1,sum) ), apply(osample$sa,2,sum)/sum( apply(osample$sa,2,sum) ), apply(osample$sa,3,sum)/sum( apply(osample$sa,3,sum) ))),
	c(player=0, c(apply(osample$fa,1,sum)/sum( apply(osample$fa,1,sum) ), apply(osample$fa,2,sum)/sum( apply(osample$fa,2,sum) ), apply(osample$fa,3,sum)/sum( apply(osample$fa,3,sum) )))
	)
})))
setnames(dtsampw, paste0("V",2:10), c(paste0("A1_", c("weak", "medium", "strong")), paste0("P1_", c("weak", "medium", "strong")), paste0("A2_", c("weak", "medium", "strong"))))
dtsamp <- melt(dtsampw, id="player") %>% setnames("value", "percentage")  %>% separate(variable, c("variable", "state"))
dtsamp[,player:=ifelse(player==0, "Amateur", "Expert")]
dtsamp[,state:=factor(state, levels=c("weak", "medium", "strong"))]
dtsamp[,variable:=factor(variable, levels=c("A1", "A2", "P1"), labels=c("Output: P1 action", "Input: P2 action", "Input: P1 cards"))]

fig <- ggplot(dtsamp, aes(x=state, y=percentage, group=player, fill=player)) + geom_bar(position = "dodge", stat = "summary", fun.y = "mean")  + stat_summary(fun.data = mean_se, geom = "errorbar", position="dodge") + facet_wrap(~variable, ncol=1) + theme_few() + theme(aspect.ratio=0.6, legend.position="bottom", panel.spacing.y=unit(0.5, "cm")) 
fig <- ggplot(dtsamp, aes(x=state, y=percentage, group=player, fill=player)) + geom_bar(position = "dodge", stat = "summary", fun.y = "mean")  + stat_summary(fun.data=mean_ci, geom="errorbar", position="dodge") + facet_wrap(~variable, ncol=1) + theme_few() + theme(aspect.ratio=0.6, legend.position="bottom", panel.spacing.y=unit(0.5, "cm")) 
ggsave( fig , filename=paste(path_poker_images, "dist_descriptive", increment_me, sub_increment, ".pdf", sep=''), height=175, width=60, units="mm", scale=1.3)
