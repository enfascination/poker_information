#!/usr/bin/env Rscript
pathLocal <- "~/projecto/research_projects/poker_information/"
source(paste0(pathLocal, "local_settings.r"))

library(ggplot2)
library(gridExtra)
library(ggthemes)
`%ni%` = Negate(`%in%`) ### help functions
options(gsubfn.engine = "R")
library(sqldf)


### process argsif run as script, otherwise this will runfine in console on test settings
library(optparse)
### flag parsing from command line
option_list <- list(
                        make_option(c("-v", "--version"), 
                                            help="version number of temp files [default %default]")
                        )
opt <- parse_args(OptionParser(option_list=option_list))
if( is.null(opt$version) ) {
    #increment_me <- "040_unordered"
    #increment_me <- "040_showdown_ordered_preflop"
    increment_me <- "0"
    increment_me <- "61"
} else {
    increment_me <- opt$version
}

#these were all  generated by buildpokerdistributions006.r, despotet eh existence of 007, which tried to get order in which is still buggy
#increment_me <- "021extr2"  
# the paper uses either 021 or 021extr2
#sub_increment <- "04"
#increment_me <- "030excludeunobserved"
#increment_me <- "030reproduce21"
#increment_me <- "031_100repsfull0025data"
#increment_me <- "032_100_noante"
### increment_me <- '039order' #is a test after the merge code that did a few things: added order, all updated, added the other category, got rid of ante and got rid of post win betting rounds.
#increment_me <- "040_preflop"  preflop only suffix
#increment_me <- '042' #is real data after the merge code that : added working order, the 'other' category, with no ante or post-win betting rounds.
#sub_increment <- "05showdown"  showown only suffix
sub_increment <- "00"
path_poker <- pathLocal
path_poker_wagers_data <- paste0(pathLocal, "hash_wagers", "_", increment_me, "/")
path_poker_actions_data <- paste0(pathLocal, "hash_actions", "_", increment_me, "/")
path_poker_unordered_data <- paste0(pathLocal, "wagers_unordered", "_", increment_me, "/")
path_poker_shownonly_data <- paste0(pathLocal, "wagers_shownhands", "_", increment_me, "/")
path_poker_images <- paste0(pathLocal, "images/", "hash_wagers", "_", increment_me, "/")
#path_poker_images <- paste0(pathLocal, "images/", "hash_actions", "_", increment_me, "/")
dir.create(path_poker_images)

loadPokerData <- function(fromData, whichData, cols=c("ui1", "ue2", "rdn", "syn"), colLabels=c("Unq(W1;P1)", "Unq(W1;W2)", "Rdn(W1;P1,W2)", "Syn(W1;P1,W2)") ) {
    ### this load creates stat_long_2p, stat_long_2p_spec, and stat_long_2p_st
	load(file=paste0(fromData, "poker_graph_ready_data", ".Rdata", sep=''))
	suitToData <- function(stat_long, cols=cols, colLabels=colLabels) {
        #stat_long$setting_bak <- stat_long$setting
		stat_long$setting      <- factor(stat_long$setting, levels=c("Fish v Fish", "Fish v Other", "Shark v Fish", "Shark v Other"), labels=c("Fish v Fish", "Fish", "Shark v Fish", "Shark"))
		stat_long <- subset(stat_long , q %in% cols & setting %ni% c("Shark v Shark", "Shark v Fish", "Fish v Fish"))
		stat_long$q <- factor( as.character(stat_long$q), levels=cols, labels=colLabels )
        #print(table(stat_long$setting_bak))
        #print(table(stat_long$setting ))
		return(stat_long)
	}
	if(whichData == "main") {
		stat_long_2p <- suitToData(stat_long_2p, cols=cols, colLabels=colLabels)
		return(stat_long_2p)
	} else if(whichData == "spec") {
		stat_long_2p_spec <- suitToData(stat_long_2p_spec, cols=cols, colLabels=colLabels)
		stat_long_2p_spec$specific <- factor(as.character(stat_long_2p_spec$specific), levels=c(1,2,3), labels=c("No wager", "Small wager", "Large wager"))
		return(stat_long_2p_spec)
	} else if(whichData == "st") {
		stat_long_2p_st <- suitToData(stat_long_2p_st, cols=cols, colLabels=colLabels)
		return(stat_long_2p_st)
	}
}

### all objects to be graphed
stat_long_2p <- loadPokerData(fromData=path_poker_wagers_data,whichData="main")
stat_long_2p_basics <- loadPokerData(fromData=path_poker_wagers_data,whichData="main", cols=c("h1", "ti", "ti_h1"), colLabels=c("Entropy of W1", "Total information about W1", "I(W1;P1,W2)/H(W1)"))
stat_long_2p_spec <- loadPokerData(fromData=path_poker_wagers_data,whichData="spec")
stat_long_2p_st <- loadPokerData(fromData=path_poker_wagers_data,whichData="st")
stat_long_2p_actions <- loadPokerData(fromData=path_poker_actions_data,whichData="main")
stat_long_2p_actions_basics <- loadPokerData(fromData=path_poker_actions_data,whichData="main", cols=c("h1", "ti", "ti_h1"), colLabels=c("Entropy of W1", "Total information about W1", "I(W1;P1,W2)/H(W1)"))
stat_long_2p_spec_actions <- loadPokerData(fromData=path_poker_actions_data,whichData="spec")
stat_long_2p_st_actions <- loadPokerData(fromData=path_poker_actions_data,whichData="st")
stat_long_2p_showdown <- loadPokerData(fromData=path_poker_shownonly_data,whichData="main")
stat_long_2p_showdown_spec <- loadPokerData(fromData=path_poker_shownonly_data,,whichData="spec")

### FIGURE ONE WITH WAGERS
### figure one
max_fig1_a <- 1.55
min_fig1_c <- 0.18
max_fig1_c <- 0.35
limits1a <- c(0,max_fig1_a)
limits1c <- c(0,max_fig1_a)
limits1a <- NULL
limits1c <- NULL
stat_long_pub <- stat_long_2p_basics
p <- ggplot(subset(stat_long_pub,  q %in% c("Entropy of W1", "Total information about W1") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=4) + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits", limits=limits1a)+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position="none") #; p
q <- ggplot(subset(stat_long_pub,  q %in% c("I(W1;P1,W2)/H(W1)") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + scale_x_discrete("Betting level ($)")+ scale_y_continuous("%", limits=limits1c)+  scale_color_brewer("Focal player", type="qual", palette=2) + labs(title="Total information, normalized") + theme_few() + theme( aspect.ratio=0.6, legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm"), plot.margin=unit(c(1,0,0.75,0), "cm"), plot.title=element_text(size=10)) +  guides(colour = guide_legend(override.aes = list(size=2)))#; q
ggsave( arrangeGrob(p+ guides(position="none"), q, ncol=2) , filename=paste(path_poker_images, "entropy_by_blind", increment_me, sub_increment, ".pdf", sep=''), height=43.2, width=163, units="mm", scale=1.4)

### figure two
#stat_long_pub <- subset(stat_long_2p,  q %in% c("ui1n", "ue2n", "rdnn", "synn") & setting %ni% c("Shark v Shark"))
#stat_long_pub$q <- factor(as.character(stat_long_pub$q), levels=c("ui1n", "ue2n", "rdnn", "synn"), labels=c("Unq(W1;P1)", "Unq(W1;W2)", "Rdn(W1;P1,W2)", "Syn(W1;P1,W2)"))
max_fig2_a <- 0.4
limits2a <- c(0,max_fig2_a)
stat_long_pub <- stat_long_2p
limits2a <- NULL
p <- ggplot(stat_long_pub, aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=4, scale="free_y") + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits", limits=limits2a)+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.93,.25), legend.background = element_rect(colour = 'gray',  size = 0.1), legend.key.size = unit(0.2, "cm"), legend.title=element_text(size=3)) +  guides(colour = guide_legend(override.aes = list(size=1))); p
### save figure two
ggsave( p , filename=paste(path_poker_images, "info_by_blind", increment_me, sub_increment, ".pdf", sep=''), height=43.2, width=175, units="mm", scale=1.3)
#, height=1.70, width=7.5, units="in", scale=1.7)

### FIGURES ONE ANd TWO WITH ACTIONS
### figure one
max_fig1_a <- 1.55
min_fig1_c <- 0.18
max_fig1_c <- 0.35
stat_long_pub_actions <- stat_long_2p_actions_basics
p <- ggplot(subset(stat_long_pub_actions,  q %in% c("Entropy of W1", "Total information about W1") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=4) + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits", limits=c(0,max_fig1_a))+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position="none") #; p
q <- ggplot(subset(stat_long_pub_actions,  q %in% c("I(W1;P1,W2)/H(W1)") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + scale_x_discrete("Betting level ($)")+ scale_y_continuous("%", limits=c(0,max_fig1_c ))+  scale_color_brewer("Focal player", type="qual", palette=2) + labs(title="Total information, normalized") + theme_few() + theme( aspect.ratio=0.6, legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm"), plot.margin=unit(c(1,0,0.75,0), "cm"), plot.title=element_text(size=10)) +  guides(colour = guide_legend(override.aes = list(size=2)))#; q
ggsave( arrangeGrob(p+ guides(position="none"), q, ncol=2) , filename=paste(path_poker_images, "entropy_by_blind_on_actions", increment_me, sub_increment, ".pdf", sep=''), height=43.2, width=163, units="mm", scale=1.4)
### figure two
max_fig2_a <- 0.4
limits2a <- c(0,max_fig2_a)
limits2a <- NULL
stat_long_pub_actions <- stat_long_2p_actions
p <- ggplot(stat_long_pub_actions, aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=4, scale="free_y") + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits", limits=limits2a)+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.93,.75), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); p
### save figure two
ggsave( p , filename=paste(path_poker_images, "info_by_blind_on_actions", increment_me, sub_increment, ".pdf", sep=''), height=43.2, width=175, units="mm", scale=1.3)
#, height=1.70, width=7.5, units="in", scale=1.7)
stat_long_spec_pub <- stat_long_2p_spec_actions
ps <- ggplot(stat_long_spec_pub , aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.25, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5, size=0.25) + facet_grid( q~specific    , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.91,.90), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
ggsave( ps , filename=paste(path_poker_images, "spec_info_by_blind_on_actions", increment_me, sub_increment, ".pdf", sep=''), width=175, height=93.3, units="mm", scale=1.3)

### figure Three
stat_long_spec_pub <- stat_long_2p_spec
ps <- ggplot(stat_long_spec_pub , aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.25, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5, size=0.25) + facet_grid( q~specific    , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.91,.90), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
ggsave( ps , filename=paste(path_poker_images, "spec_info_by_blind_on_actions", increment_me, sub_increment, ".pdf", sep=''), width=175, height=93.3, units="mm", scale=1.3)

### figure four (robustness check with showdown-only data
stat_long_2p_full <- stat_long_2p
stat_long_2p_spec_full <- stat_long_2p_spec
### change labels, again, to get rid of this meaningless "other"
stat_long_2p_fig4 <- rbind(cbind(stat_long_2p_full, Dataset="Full"), cbind(stat_long_2p_showdown, Dataset="Hands w/ showdown")) 
ps <- ggplot(stat_long_2p_fig4, aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.5, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_grid( q ~ Dataset  , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.68,.92), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
ggsave( ps , filename=paste(path_poker_images, "full_v_showdown_robustness", increment_me, sub_increment, ".pdf", sep=''), width=90, height=100, units="mm", scale=1.3)

### figure Three, with shown-only data
stat_long_spec2_pub <- stat_long_2p_showdown_spec 
ps <- ggplot(stat_long_spec2_pub , aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.25, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5, size=0.25) + facet_grid( specific ~ q  , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.10,.90), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
ggsave( ps , filename=paste(path_poker_images, "spec_info_by_blind_showdown", increment_me, sub_increment, ".pdf", sep=''), width=175, height=93.3, units="mm", scale=1.3)


### DON'T INCLUDE, not for now.  figure 5, re-repesenting figure four in terms of idfferences
stat_long_2p_fig5 <- reshape(stat_long_2p_fig4, idvar=c("rep", "q", "blind", "site", "Dataset"), v.names="val", timevar="setting", direction='wide')
ps <- ggplot(subset(stat_long_2p_fig5, (Dataset == "Full") ), aes(factor(q), (`val.Shark`-`val.Fish`)) ) + geom_point(size=0.5, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_grid( Dataset ~ .) + scale_x_discrete("Blind")+ scale_y_continuous("Bits")+  scale_color_brewer("Match", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.68,.92), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
### DON'T INCLUDE, not for now.  , re-repesenting any figure in terms of idfferences
stat_long_2p_fig5 <- reshape(stat_long_2p, idvar=c("rep", "q", "blind", "site"), v.names="val", timevar="setting", direction='wide')
ps <- ggplot(subset(stat_long_2p_fig5 ), aes(factor(q), (`val.Shark`-`val.Fish`)) ) + geom_point(size=0.5, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5)  + scale_x_discrete("Blind")+ scale_y_continuous("Bits")+  scale_color_brewer("Match", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.68,.92), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps

### BY STREET
### figure Three
stat_long_st_pub <- stat_long_2p_st_actions
stat_long_st_pub$street <- ordered(as.character(stat_long_st_pub$street), levels=c(0,1,2,3,4), labels=c("Preflop","Flop","Turn","River","Showdown"))
ps <- ggplot(stat_long_st_pub , aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.25, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5, size=0.25) + facet_grid( q ~ street   , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.91,.90), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
ggsave( ps , filename=paste(path_poker_images, "st_info_by_blind_on_actions", increment_me, sub_increment, ".pdf", sep=''), width=175, height=93.3, units="mm", scale=1.3)
### BY STREET
### figure Three
stat_long_st_pub <- stat_long_2p_st
stat_long_st_pub$street <- ordered(as.character(stat_long_st_pub$street), levels=c(0,1,2,3,4), labels=c("Preflop","Flop","Turn","River","Showdown"))
ps <- ggplot(stat_long_st_pub , aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.25, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5, size=0.25) + facet_grid( q ~ street   , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.91,.90), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
ggsave( ps , filename=paste(path_poker_images, "st_info_by_blind", increment_me, sub_increment, ".pdf", sep=''), width=175, height=93.3, units="mm", scale=1.3)

#library(sqldf)
#sqldf("SELECT ti, SUM(val) AS ti3 FROM stat_long WHERE ((q = q1) OR (q = q2) OR (q=q4) OR (q = q5)) GROUP BY rep")

ggplot(subset(stat_long_2p, site == "PS" & q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Bits")
ggplot(subset(stat_long_2p, site == "PS" & q %in% c("ui1n", "ue2n", "rdnn", "synn") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")

ggplot(subset(stat_long_2p, site == "PS" & q %in% c("mi12", "ti1", "ti2", "ti") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")
ggplot(subset(stat_long_2p, site == "PS" & q %in% c("mi12", "mi34", "mi13", "ti") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")

### specific information
ggplot(subset(stat_long_2p_spec, site == "PS" & q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point() + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_grid( specific ~ q ) + scale_x_discrete("Blind")+ scale_y_continuous("Bits")
ggplot(subset(stat_long_2p_spec, site == "PS" & q %in% c("ui1n", "ue2n", "rdnn", "synn") & setting %ni% c("Shark v Shark") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point() + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_grid( specific ~ q ) + scale_x_discrete("Blind")+ scale_y_continuous("Percentage of total information")
ggplot(subset(stat_long_2p_spec, site == "PS" & q %in% c("mi12", "mi34", "mi13", "ti") & setting %ni% c("Shark v Shark") ), aes(factor(blind), val, group=setting, color=setting) ) + geom_point() + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_grid( specific ~ q ) + scale_x_discrete("Blind")+ scale_y_continuous("Bits")
ddd
#ggplot(subset(stat_long, q=="q5"), aes(val, fill=setting) ) + geom_histogram(binwidth=0.0001) +
#geom_vline(quantile(stat_long[3400:3600,2], c(0, 0.025, 0.975, 1)))

#ggplot(subset(stat_long, q=="q5"), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)

ggplot(subset(stat_long), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")

ggplot(subset(stat_long, q %in% c("q1", "q2", "q4", "q5")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")
ggplot(subset(stat_long, q %in% c("q1", "q2", "q4", "q5")), aes(setting, val, group=factor(ublind), color=factor(ublind)) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")
ggplot(subset(stat_long, q %in% c("ti", "ti2", "ti3")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")
ggplot(subset(stat_long, q %in% c("q3i1n", "q3i2n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")
ggplot(subset(stat_long, q %in% c("q1n", "q2n", "q4n", "q5n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("")+ scale_y_continuous("Percentage of total information")


gg <- cbind(stat_long[stat_long$q == "ti",c("val", "rep", "setting")][order(stat_long[stat_long$q == "ti",c("val", "rep", "setting")]),], stat_long[stat_long$q == "ti3",c("val", "rep", "setting")][order(stat_long[stat_long$q == "ti3",c("val", "rep", "setting")]),], stat_long[stat_long$q == "q1","val"], stat_long[stat_long$q == "tin","val"])
plot(cumsum(sort((gg[seq(from=3, by=3, to=60),1] - gg[seq(from=3, by=3, to=60),2]))))
plot(sort(stat_long[stat_long$q == "q5","val"][]))
plot(sort(stat_long[stat_long$q == "q5","val"][]))
plot(sort(stat_long[stat_long$q == "q5","val"][]))
plot(sort(stat_long[stat_long$q == "ti","val"][]))
plot(sort(stat_long[stat_long$q == "ti3","val"][]))



### u1
ggplot(subset(stat_long, q %in% c("q1n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,1))

### figure one, h1
ggplot(subset(stat_long, q %in% c("q2n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,1))

### figure two, h2a
ymax <- max(subset(stat_long, q=="ti", select=val))
ggplot(subset(stat_long, q %in% c("ti")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))

### synergy
ggplot(subset(stat_long, q %in% c("q4n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,1))

### redundancy
ggplot(subset(stat_long, q %in% c("q5n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,1))

### mutual information between behaviors of p1 and p2
ymax <- max(subset(stat_long, q=="mi13", select=val))
ggplot(subset(stat_long, q %in% c("mi13")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))
ymax <- max(subset(stat_long, q=="h12", select=val))
ggplot(subset(stat_long, q %in% c("h12")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))
ymax <- max(subset(stat_long, q=="h34", select=val))
ggplot(subset(stat_long, q %in% c("h34")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))

### q3, h3
stat_long_q3 <- subset(stat_long, q %in% c( "q3i2n"))
stat_long_q3$setting <- with(stat_long_q3, ifelse((setting== "Shark v Fish") & (q=="q3i1n"), "SHARK v Fish", 
                                                  ifelse((setting== "Shark v Fish") & (q=="q3i2n"), "Shark v FISH", as.character(setting)  )))
stat_long_q3$setting <- factor(stat_long_q3$setting)
stat_long_q3$setting <- factor(stat_long_q3$setting, levels=levels(stat_long_q3$setting)[c(1,3,2,4)])
stat_long_q3$q <- "q3"
ymax <- max(stat_long_q3$val)
ggplot(stat_long_q3, aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total entropy", lim=c(0,ymax))

### q3, h3, unnormalized
stat_long_q3 <- subset(stat_long, q %in% c( "q3i2"))
stat_long_q3$setting <- with(stat_long_q3, ifelse((setting== "Shark v Fish") & (q=="q3i1"), "SHARK v Fish", 
                                                  ifelse((setting== "Shark v Fish") & (q=="q3i2"), "Shark v FISH", as.character(setting)  )))
stat_long_q3$setting <- factor(stat_long_q3$setting)
stat_long_q3$setting <- factor(stat_long_q3$setting, levels=levels(stat_long_q3$setting)[c(1,3,2,4)])
stat_long_q3$q <- "q3"
ymax <- max(stat_long_q3$val)
ggplot(stat_long_q3, aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))
