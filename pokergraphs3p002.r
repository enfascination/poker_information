library(ggplot2)

#file_input <- "~/projecto/research_projects/poker_information/diststatistics003.csv"
increment_me <- "015"
load(file=paste("~/projecto/research_projects/poker_information/poker_graph_3p_ready_data_", increment_me, ".Rdata", sep=''))
if (!any("statistics" %in% ls())) {
  #file_input <- "~/projecto/research_projects/poker_information/3pdiststatistics008.csv"
  #statistics <- read.csv(file_input)
  #statistics_2d <- statistics
  #statistics_3d <- array(0, c(120, 11, 3), dimnames=list(NULL,c("q1", "q2", "q3_i1", "q3_i2","q4","q5", "ti", "mi13", "ti2", "h12", "h34"),c("ss","sf","ff")) )
  #statistics_3d[,,1] <- as.matrix(statistics_2d[,2:12])
  #statistics_3d[,,2] <- as.matrix(statistics_2d[,13:23])
  #statistics_3d[,,3] <- as.matrix(statistics_2d[,24:34])
  #statistics_3d <- array(0,c(200,9,3), dimnames=list(NULL,c("h1f", "h2p", "h3s", "hfps", "u2", "u3", "rdn", "syn","ti"),c("sss", "fss","ffs")))
  #statistics_3d[,,1] <- as.matrix(statistics_2d[,2:10])
  #statistics_3d[,,2] <- as.matrix(statistics_2d[,11:19])
  #statistics_3d[,,3] <- as.matrix(statistics_2d[,20:28])
  #statistics <- statistics_3d
}

#library(sqldf)
#sqldf("SELECT ti, SUM(val) AS ti3 FROM stat_3p_long_3p WHERE ((q = q1) OR (q = q2) OR (q=q4) OR (q = q5)) GROUP BY rep")

#ggplot(subset(stat_3p_long, q=="q5"), aes(val, fill=setting) ) + geom_histogram(binwidth=0.0001) +
#geom_vline(quantile(stat_3p_long[3400:3600,2], c(0, 0.025, 0.975, 1)))

#ggplot(subset(stat_3p_long, q=="q5"), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)

ggplot(subset(stat_3p_long), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")

ggplot(subset(stat_3p_long, q %in% c("h1f", "h2p", "h3s", "hfps", "u2", "u3", "rdn", "syn","ti")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")
ggplot(subset(stat_3p_long, q %in% c("h1f", "h2p", "h3s", "hfps")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")
ggplot(subset(stat_3p_long, q %in% c("h1f", "h2p", "h3s")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")
ggplot(subset(stat_3p_long, q %in% c( "u2", "u3", "rdn", "syn","ti")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")
ggplot(subset(stat_3p_long, q %in% c( "u2n", "u3n", "rdnn", "synn")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")
ggplot(subset(stat_3p_long, q %in% c("h1f", "h2p", "h3s", "hfps", "u2", "u3", "rdn", "syn","ti")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Match")+ scale_y_continuous("Bits")



### figure one, h1
ymax <- max(subset(stat_3p_long, q=="q2n", select=val))
ggplot(subset(stat_3p_long, q %in% c("q2n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,ymax))

### figure two, h2a
ymax <- max(subset(stat_3p_long, q=="ti", select=val))
ggplot(subset(stat_3p_long, q %in% c("ti")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))

### figure two, h2b
ymax <- max(subset(stat_3p_long, q=="q4n", select=val))
ggplot(subset(stat_3p_long, q %in% c("q4n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,ymax))

### redundancy
ymax <- max(subset(stat_3p_long, q=="q5n", select=val))
ggplot(subset(stat_3p_long, q %in% c("q5n")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,ymax))

### q3, h3
stat_3p_long_q3 <- subset(stat_3p_long, q %in% c("q3i1n", "q3i2n"))
stat_3p_long_q3$setting <- with(stat_3p_long_q3, ifelse((setting== "Shark v Fish") & (q=="q3i1n"), "SHARK v Fish", 
                                                  ifelse((setting== "Shark v Fish") & (q=="q3i2n"), "Shark v FISH", as.character(setting)  )))
stat_3p_long_q3$setting <- factor(stat_3p_long_q3$setting )
stat_3p_long_q3$q <- "q3"
ymax <- max(stat_3p_long_q3$val)
ggplot(stat_3p_long_q3, aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total information", lim=c(0,ymax))
