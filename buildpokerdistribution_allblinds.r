#!/usr/bin/env Rscript
source("local_settings.r")
source(paste0(pathLocal, "info_decomp_fns006.r"))

library(foreach)
library(doParallel)
cl<-makeCluster(nCores)
registerDoParallel(cl)


### process argsif run as script, otherwise this will runfine in console on test settings
library(optparse)
### flag parsing from command line
option_list <- list(
                        make_option(c("-t", "--tag"), 
                                            help="analysis tag fr type of analysis [default %default]"),
                        make_option(c("-v", "--version"), 
                                            help="version number of temp files [default %default]"),
                        make_option(c("-q", "--quiet"), action="store_true", 
                                            help="reduce output [default %default]"),
                        make_option(c("-r", "--reps"), 
                                            help="number of bootstrap samples [default %default]")
                        )
opt <- parse_args(OptionParser(option_list=option_list))
if( is.null(opt$tag) ) {
    analysis_tag <- c("hash_wagers", "hash_actions", "wagers_flop", "wagers_unordered", "wagers_shownhands", "test")[1]
    print(paste0("the variant of this analysis is: ", analysis_tag))
} else {
    analysis_tag <- opt$tag
    print(paste0("the variant of this analysis has been specified by the calling script: ", analysis_tag))
}
if( is.null(opt$reps) ) {
    reps <- 2
} else {
    reps <- opt$reps
}
if( is.null(opt$version) ) {
    #increment_me <- "040_unordered"
    #increment_me <- "040_showdown_ordered_preflop"
    increment_me <- analysis_increment 
} else {
    increment_me <- opt$version
}


### tune variables to enable each of the following variants on the main analysis:
tagopts <- list()
tagopts$extr <- "wagers"
tagopts$ordered <- TRUE
tagopts$onestreet <- FALSE
tagopts$quiet <- opt$quiet
tagopts$showdowned <- FALSE
tagopts$riskmemorycrash <- TRUE
if (analysis_tag == "hash_wagers") {
} else if (analysis_tag == "hash_actions") {
    tagopts$extr <- "actions"
} else if (analysis_tag == "wagers_showdown") {
    tagopts$onestreet <- 4
} else if (analysis_tag == "wagers_flop") {
    tagopts$onestreet <- 1
} else if (analysis_tag == "wagers_unordered") {
    tagopts$ordered <- FALSE
} else if (analysis_tag == "wagers_shownhands") {
    tagopts$showdowned <- TRUE
} else if (analysis_tag == "actions_unordered") {
    tagopts$extr <- "actions"
    tagopts$ordered <- FALSE
} else if (analysis_tag == "actions_shownhands") {
    tagopts$extr <- "actions"
    tagopts$showdowned <- TRUE
} else if (TRUE | analysis_tag == "test") { ### default
    tagopts$quiet <- TRUE
    tagopts$riskmemorycrash <- TRUE
}

path_poker <- pathLocal
path_poker_data_in <- pathData
path_poker_data_out <- paste0(pathData, analysis_tag, "_", increment_me, "/")
dir.create(path_poker_data_out)


### START SAMPLING
blinds_fine <- c("1000","0600","0400","0200", "0100", "0050", "0025")
blinds_coarse <- c("all")
n_streets <- 4
dist_l_all <- list()

### DISTRIBUTIONS FOR TH MAIN FULL DECOMPOSITION
###    WRITE ALL SAMPLINGS OF ALL DISTS
### create Proficient and Nonnproficient categories from >25cent and 25 cent blinds.
print("decomposition: all blinds")
dist_l  <- data.table()
for(ib in 1:(length(blinds_fine))) {
    print(blinds_fine[ib])
    in_file <- paste0(path_poker_data_in, "distrPS", blinds_fine[ib], ".csv")
	tmp <- fread(in_file)
	tmp[,hand:=as.integer64(hand)]
    dist_l <- rbind(dist_l, tmp)
    print(paste0("number of hands: ", tmp[,length(unique(hand))]))
    print(paste0("rows: ", nrow(tmp)))
	rm(tmp)
    if (tagopts$riskmemorycrash) {
        dist_l_all[[ib]] <- dist_l
    }
    if( tagopts$onestreet != FALSE & is.numeric(tagopts$onestreet) ) {
        dist_l <- dist_l[st==tagopts$onestreet]
    }
    print(paste0("total hands: ", dist_l[,length(unique(hand))]))
    print(paste0("total rows: ", nrow(dist_l)))
}
out_file <- paste0(path_poker_data_out, "poker_distributions_2p_PS_all.Rdata")
bootstrap_distributions_to_file(data.matrix(dist_l), output_file=out_file, reps, ordered=tagopts$ordered, extrAsWagerOrAction=tagopts$extr)
for(st in 1:n_streets) {
    out_file <- paste0(path_poker_data_out, "poker_distributions_2p_PS_", "all", "_st", st, ".Rdata")
    bootstrap_distributions_to_file(data.matrix(dist_l[street==st]), output_file=out_file, reps, ordered=tagopts$ordered, extrAsWagerOrAction=tagopts$extr)
}

###    RELOAD THE SAMPLES THEN CONSOLIDATE
stat_long_2p_list <- rep(list(0),length(blinds_coarse))
for(ib in 1:length(blinds_coarse)) {
    in_file <- paste0(path_poker_data_out, "poker_distributions_2p_PS_", blinds_coarse[ib], ".Rdata")
        ppsite <- "PS"; ppblind <- blinds_coarse[ib];
        stat_long_2p_list[[ib]] <- cbind(
                                            build_stat_long_2p(get_statistics_2p_from_file(in_file,specific=NA, quiet=tagopts$quiet, onlyObsHands=tagopts$showdowned ))
                                          , blind=ppblind, ublind=paste0(ppsite, as.character(ppblind))
                                          , site=ppsite, specific=NA
                                          )
}
stat_long_2p <- rbind(stat_long_2p_list[[1]])
for (i in 2:length(stat_long_2p_list)) {
    stat_long_2p <- rbind(stat_long_2p, stat_long_2p_list[[i]])
}

stat_long_2p_new <- stat_long_2p


### NOW SPECIFIC INFORMATION (JUST MAKE STATS: CAN REUSE PREVIOUS OUTPUTS FROM MAIN ANALYSIS)
print("specific information decomposition")
n_channels <- 3
stat_long_2p_spec_list <- rep(list(0),length(blinds_coarse) * n_channels)
il <- 1
for(ib in 1:length(blinds_coarse)) {
    print(blinds_coarse[ib])
    for(channel in 1:n_channels) {
        in_file <- paste0(path_poker_data_out, "poker_distributions_2p_PS_", blinds_coarse[ib], ".Rdata")
        ppsite <- "PS"; ppblind <- blinds_coarse[ib];
        stat_long_2p_spec_list[[il]] <- cbind(
                                            build_stat_long_2p(get_statistics_2p_from_file(in_file,specific=channel, quiet=tagopts$quiet, onlyObsHands=tagopts$showdowned))
                                          , blind=ppblind, ublind=paste0(ppsite, as.character(ppblind))
                                          , site=ppsite, specific=channel
                                          )
        il <- il +1
    }
}
stat_long_2p_spec <- rbind(stat_long_2p_spec_list[[1]])
for (i in 2:length(stat_long_2p_spec_list)) {
    stat_long_2p_spec <- rbind(stat_long_2p_spec, stat_long_2p_spec_list[[i]])
}
stat_long_2p_spec <- stat_long_spec_normalized_patch(stat_long_2p_spec, skip=TRUE)  ### I no longer need to normalize ever beause I'm not plotting perentages but raw information in bits. duh.
stat_long_2p_spec_new <- stat_long_2p_spec 

### NOW PLOT BY STREET
###    WRITE ALL SAMPLINGS OF ALL DISTS
if( tagopts$onestreet == FALSE  ) { ### (skip if this is a one-street robustness analysis)
    print("information decomposition by street")
    if (FALSE) { ### this step is now refactored into above
    n_streets <- 4
    ### bigger blinds
    dist_l  <- data.table()
    for(ib in 1:(length(blinds_fine))) {
        print(blinds_fine[ib])
        in_file <- paste0(path_poker_data_in, "distrPS", blinds_fine[ib], ".csv")
        tmp <- fread(in_file)
        tmp[,hand:=as.integer64(hand)]
        dist_l <- rbind(dist_l, tmp)
    }
    for(st in 1:n_streets) {
        out_file <- paste0(path_poker_data_out, "poker_distributions_2p_PS_", "all", "_st", st, ".Rdata")
        bootstrap_distributions_to_file(data.matrix(dist_l[street==st]), output_file=out_file, reps, ordered=tagopts$ordered, extrAsWagerOrAction=tagopts$extr)
    }
    }
    ###    RELOAD THE BY STREET SAMPLES THEN CONSOLIDATE
    stat_long_2p_st_list <- rep(list(0),length(blinds_coarse) * n_streets)
    il <- 1
    for(ib in 1:length(blinds_coarse)) {
        for(st in 1:n_streets) {
            in_file <- paste0(path_poker_data_out, "poker_distributions_2p_PS_", blinds_coarse[ib], "_st", st, ".Rdata")
            ppsite <- "PS"; ppblind <- blinds_coarse[ib];
            stat_long_2p_st_list[[il]] <- cbind(
                                                build_stat_long_2p(get_statistics_2p_from_file(in_file,specific=NA, quiet=tagopts$quiet, onlyObsHands=tagopts$showdowned))
                                                , blind=ppblind, ublind=paste0(ppsite, as.character(ppblind))
                                                , site=ppsite, street=st, specific=NA
                                                )
            il <- il +1
        }
    }
    stat_long_2p_st <- rbind(stat_long_2p_st_list[[1]])
    for (i in 2:length(stat_long_2p_st_list)) {
        stat_long_2p_st <- rbind(stat_long_2p_st, stat_long_2p_st_list[[i]])
    }
    #objsToSave <- c("stat_long_2p_st")
    objsToSave <- c("stat_long_2p", "stat_long_2p_spec", "stat_long_2p_st")
} else {
    objsToSave <- c("stat_long_2p", "stat_long_2p_spec")
}

stopCluster(cl)

### WRITE IT ALL, ALL THREE stat_long OBJECTS TO AN RDATA FILE
#increment_me <- "040_showdown"
#increment_me <- "040_preflop"
#increment_me <- "040_unordered"
#increment_me <- "040"
save(list=objsToSave, file=paste0(path_poker_data_out, "poker_graph_ready_data", ".Rdata"))


if (0 == "i want to calculate by street, but this hasn't been updated") {
    street_stat_long <- subset(stat_long_2p_st,  q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark"))
    street_stat_long$q <- factor(as.character(street_stat_long$q), levels=c("ui1", "ue2", "rdn", "syn"), labels=c("Unq(E1;I1)", "Unq(E1;E2)", "Rdn(E1;I1,E2)", "Syn(E1;I1,E2)"))
    ggplot(street_stat_long, aes(factor(street), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q , ncol=4) + scale_x_discrete("Street")+ scale_y_continuous("Bits") + scale_color_brewer("Match", type="qual", palette=2)
    ggplot(subset(stat_long_2p_st_spec, site == "PS" & q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark") ), aes(factor(street), val, group=setting, color=setting) ) + geom_point() + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_grid( specific ~ q ) + scale_x_discrete("Blind")+ scale_y_continuous("Bits")

    ggplot(street_stat_long, aes(factor(street), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q , ncol=4) + scale_x_discrete("Street")+ scale_y_continuous("Bits") + scale_color_brewer("Match", type="qual", palette=2) + theme_few() + theme(aspect.ratio=1)
    p <- ggplot(subset(street_stat_long, street != 0), aes(factor(street), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q , ncol=4) + scale_x_discrete("Street")+ scale_y_continuous("Bits") + scale_color_brewer("Match", type="qual", palette=2) + theme_few() + theme(aspect.ratio=1)
    ggsave( p , filename=paste(path_poker, "info_by_street", increment_me, ".pdf", sep=''), width=8.5, height=2.5, units="in", scale=1.0)
}
