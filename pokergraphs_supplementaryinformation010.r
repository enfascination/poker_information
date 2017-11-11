
library(ggplot2)
library(gridExtra)
library(ggthemes)
`%ni%` = Negate(`%in%`) ### help functions
options(gsubfn.engine = "R")
library(sqldf)

### figure two
#stat_long_pub <- subset(stat_long_2p,  q %in% c("ui1n", "ue2n", "rdnn", "synn") & setting %ni% c("Shark v Shark"))
#stat_long_pub$q <- factor(as.character(stat_long_pub$q), levels=c("ui1n", "ue2n", "rdnn", "synn"), labels=c("Unq(W1;P1)", "Unq(W1;W2)", "Rdn(W1;P1,W2)", "Syn(W1;P1,W2)"))
produce_supp_figures <- function(path_poker, input_string, output_string, sub_increment, max_fig2_a, fig3_legend_position=c(.91,.90)) {
  increment_me <- output_string
  load(file=paste("~/projecto/research_projects/poker_information/poker_graph_ready_data_", increment_me, ".Rdata", sep=''))
  stat_long_2p$setting <- factor(stat_long_2p$setting, labels=c("Fish v Fish", "Fish", "Shark v Fish", "Shark"))

  ### figure two
  stat_long_pub <- subset(stat_long_2p,  q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark", "Shark v Fish", "Fish v Fish"))
  stat_long_pub$q <- factor(as.character(stat_long_pub$q), levels=c("ui1", "ue2", "rdn", "syn"), labels=c("Unq(W1;P1)", "Unq(W1;W2)", "Rdn(W1;P1,W2)", "Syn(W1;P1,W2)"))
  p <- ggplot(stat_long_pub, aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25), size=0.5) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=4) + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits", limits=c(0,max_fig2_a))+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = c(.93,.75), legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); p
  ### save figure two
  ggsave( p , filename=paste(path_poker, "info_by_blind", increment_me, sub_increment, ".pdf", sep=''), height=43.2, width=175, units="mm", scale=1.3)
  #, height=1.70, width=7.5, units="in", scale=1.7)

  ### figure Three
  stat_long_2p_spec$setting <- factor(stat_long_2p_spec$setting, labels=c("Fish v Fish", "Fish", "Shark v Fish", "Shark")) ### shuffle labels a bit
  stat_long_spec_pub <- subset(stat_long_2p_spec, q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark", "Shark v Fish", "Fish v Fish"))
  stat_long_spec_pub$q <- factor(as.character(stat_long_spec_pub$q), levels=c("ui1", "ue2", "rdn", "syn"), labels=c("Unq(W1;P1)", "Unq(W1;W2)", "Rdn(W1;P1,W2)", "Syn(W1;P1,W2)"))
  stat_long_spec_pub$specific <- factor(as.character(stat_long_spec_pub$specific), levels=c(1,2,3), labels=c("No wager", "Small wager", "Large wager"))
  ps <- ggplot(stat_long_spec_pub , aes(factor(blind), val, group=setting, color=setting) ) + geom_point(size=0.25, position = position_jitter(width = 0.25, height=0)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5, size=0.25) + facet_grid( specific ~ q  , scales='free_y') + scale_x_discrete("Betting level ($)")+ scale_y_continuous("Bits")+  scale_color_brewer("Focal player", type="qual", palette=2) + theme_few() + theme(aspect.ratio=0.6, legend.position = fig3_legend_position, legend.background = element_rect(colour = 'gray',  size = 0.2), legend.key.size = unit(0.3, "cm")) +  guides(colour = guide_legend(override.aes = list(size=2))); ps
  ggsave( ps , filename=paste(path_poker, "spec_info_by_blind", increment_me, sub_increment, ".pdf", sep=''), width=175, height=93.3, units="mm", scale=1.3)
}
path_poker <- "~/projecto/research_projects/poker_information/"


increment_me <- '040' #is real data after the merge code that : added working order, the 'other' category, with no ante or post-win betting rounds.
sub_increment <- "09"

#poker_graph_ready_data_040.Rdata
#poker_graph_ready_data_040_preflop.Rdata
#poker_graph_ready_data_040_showdown.Rdata
#poker_graph_ready_data_040_unordered.Rdata
#poker_graph_ready_data_040_unordered_specnormalized.Rdata

produce_supp_figures(path_poker, "poker_graph_ready_data_040.Rdata", "040", sub_increment, max_fig2_a=0.40)
produce_supp_figures(path_poker, "poker_graph_ready_data_040_preflop.Rdata", "040_preflop", sub_increment, max_fig2_a=0.10)
produce_supp_figures(path_poker, "poker_graph_ready_data_040_showdown.Rdata", "040_showdown", sub_increment, max_fig2_a=0.80, fig3_legend_position=c(.10,.90))
produce_supp_figures(path_poker, "poker_graph_ready_data_040_unordered.Rdata", "040_unordered", sub_increment, max_fig2_a=0.40)
#produce_supp_figures(path_poker, "poker_graph_ready_data_040_unordered_specnormalized.Rdata", "040_unordered_specnormalized", sub_increment, max_fig2_a=0.40)
#sub_increment <- "08"
#sub_increment <- "09"
#produce_supp_figures(path_poker, "poker_graph_ready_data_041_unordered.Rdata", "040_unordered", sub_increment, max_fig2_a=0.40)
produce_supp_figures(path_poker, "poker_graph_ready_data_040showdown_ordered_preflop.Rdata", "040showdown_ordered_preflop", sub_increment, max_fig2_a=0.40)

