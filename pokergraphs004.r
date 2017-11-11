library(ggplot2)

#increment_me <- "013"
#load(file=paste("~/projecto/research_projects/poker_information/poker_graph_ready_data_", increment_me, ".Rdata", sep=''))
#file_input <- "~/projecto/research_projects/poker_information/diststatisticsABS0600_010.Rdata"
#load(file=file_input)
#file_input <- "~/projecto/research_projects/poker_information/diststatisticsABS1000_010.Rdata"
#load(file=file_input)

#library(sqldf)
#sqldf("SELECT ti, SUM(val) AS ti3 FROM stat_long WHERE ((q = q1) OR (q = q2) OR (q=q4) OR (q = q5)) GROUP BY rep")

ggplot(subset(stat_long, site == "PS" & q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")
ggplot(subset(stat_long, site == "PS" & q %in% c("ui1n", "ue2n", "rdnn", "synn") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")
ggplot(subset(stat_long, site == "ABS" & q %in% c("ui1", "ue2", "rdn", "syn") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")
ggplot(subset(stat_long, site == "ABS" & q %in% c("ui1n", "ue2n", "rdnn", "synn") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")

ggplot(subset(stat_long, site == "PS" & q %in% c("mi12", "mi34", "mi13", "ti") & setting %ni% c("Shark v Shark")), aes(factor(blind), val, group=setting, color=setting) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Measure")+ scale_y_continuous("Percentage of total information")

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
