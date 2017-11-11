library(ggplot2)

file_input <- "~/projecto/research_projects/poker_information/diststatistics003.csv"
if (!any("statistics" %in% ls())) {
  statistics <- read.csv(file_input)
  statistics_2d <- statistics
  statistics_3d <- array(0, c(120, 11, 3), dimnames=list(NULL,c("q1", "q2", "q3_i1", "q3_i2","q4","q5", "ti", "mi13", "ti2", "h12", "h34"),c("ss","sf","ff")) )
  statistics_3d[,,1] <- as.matrix(statistics_2d[,2:12])
  statistics_3d[,,2] <- as.matrix(statistics_2d[,13:23])
  statistics_3d[,,3] <- as.matrix(statistics_2d[,24:34])
  statistics <- statistics_3d
}
stat_long <- data.frame(rep=as.numeric(rep(0,length(statistics))), val=0, q=0, setting=0)
#print(length(rep(0,length(statistics))))
for (j in 1:dim(statistics)[2]) {
  for (k in 1:dim(statistics)[3]) {
    idx <- (1:dim(statistics)[1])+((j-1)*(dim(statistics)[1]))+((k-1)*(prod(dim(statistics)[1:2])))
    stat_long[idx,1:2] <- cbind(1:dim(statistics)[1], statistics[,j,k])
    stat_long[idx,3] <- attr( statistics, "dimnames")[[2]][j]
    stat_long[idx,4] <- attr( statistics, "dimnames")[[3]][k]
    #print((dim(statistics)[1])+((j-1)*(dim(statistics)[1]))+((k-1)*(prod(dim(statistics)[1:2]))))
}}
#stat_long$ti <- 0
#stat_long$ti[stat_long$setting=="ff"] <-  stat_long[(stat_long$setting=="ff")&(stat_long$q=="ti"),"val"]  
#stat_long$ti[stat_long$setting=="ss"] <-  stat_long[(stat_long$setting=="ss")&(stat_long$q=="ti"),"val"]  
#stat_long$ti[stat_long$setting=="sf"] <-  stat_long[(stat_long$setting=="sf")&(stat_long$q=="ti"),"val"]  
for (i in 1:max(stat_long$rep)) {
  for (j in c("ss", "sf", "ff")) {
    ti <- sum(stat_long[(stat_long$rep==i) & (stat_long$setting==j) & (stat_long$q %in% c("q1", "q2", "q4", "q5")),"val"])
    h12 <- sum(stat_long[(stat_long$rep==i) & (stat_long$setting==j) & (stat_long$q == "h12"), "val" ])
    h34 <- sum(stat_long[(stat_long$rep==i) & (stat_long$setting==j) & (stat_long$q == "h34"), "val" ])
    stat_long <- rbind(stat_long, data.frame(rep=i, val=ti, q="ti3", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q1"),"val"]/ti, q="q1n", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q2"),"val"]/ti, q="q2n", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q4"),"val"]/ti, q="q4n", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q5"),"val"]/ti, q="q5n", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "ti"),"val"]/ti, q="tin", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q3_i1"),"val"], q="q3i1", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q3_i2"),"val"], q="q3i2", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q3_i1"),"val"]/h12, q="q3i1n", setting=j))
    stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "q3_i2"),"val"]/h34, q="q3i2n", setting=j))
  }
}
stat_long$q <- factor(stat_long$q)
stat_long$setting <- with(stat_long, ifelse(setting=="ss", "Shark v Shark", ifelse(setting=="ff", "Fish v Fish", "Shark v Fish")))
stat_long$setting <- factor(stat_long$setting)
#library(sqldf)
#sqldf("SELECT ti, SUM(val) AS ti3 FROM stat_long WHERE ((q = q1) OR (q = q2) OR (q=q4) OR (q = q5)) GROUP BY rep")


#ggplot(subset(stat_long, q=="q5"), aes(val, fill=setting) ) + geom_histogram(binwidth=0.0001) +
#geom_vline(quantile(stat_long[3400:3600,2], c(0, 0.025, 0.975, 1)))

#ggplot(subset(stat_long, q=="q5"), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)

ggplot(subset(stat_long), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")

ggplot(subset(stat_long, q %in% c("q1", "q2", "q4", "q5", "q1", "ti")), aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5) + facet_wrap( ~ q, ncol=2) + scale_x_discrete("Pairing")+ scale_y_continuous("Bits")
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

### q3, h3
stat_long_q3 <- subset(stat_long, q %in% c("q3i1n", "q3i2n"))
stat_long_q3$setting <- with(stat_long_q3, ifelse((setting== "Shark v Fish") & (q=="q3i1n"), "SHARK v Fish", 
                                                  ifelse((setting== "Shark v Fish") & (q=="q3i2n"), "Shark v FISH", as.character(setting)  )))
stat_long_q3$setting <- factor(stat_long_q3$setting)
stat_long_q3$setting <- factor(stat_long_q3$setting, levels=levels(stat_long_q3$setting)[c(1,3,2,4)])
stat_long_q3$q <- "q3"
ymax <- max(stat_long_q3$val)
ggplot(stat_long_q3, aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Percentage of total entropy", lim=c(0,ymax))

### q3, h3, unnormalized
stat_long_q3 <- subset(stat_long, q %in% c("q3i1", "q3i2"))
stat_long_q3$setting <- with(stat_long_q3, ifelse((setting== "Shark v Fish") & (q=="q3i1"), "SHARK v Fish", 
                                                  ifelse((setting== "Shark v Fish") & (q=="q3i2"), "Shark v FISH", as.character(setting)  )))
stat_long_q3$setting <- factor(stat_long_q3$setting)
stat_long_q3$setting <- factor(stat_long_q3$setting, levels=levels(stat_long_q3$setting)[c(1,3,2,4)])
stat_long_q3$q <- "q3"
ymax <- max(stat_long_q3$val)
ggplot(stat_long_q3, aes(setting, val) ) + geom_point(position = position_jitter(width = .25)) + stat_summary(fun.y=mean, fun.ymin=function(y) quantile(y, c(0.025)), fun.ymax=function(y) quantile(y, c(0.975)), color="red", geom="errorbar", width= 0.5)  + scale_x_discrete("")+ scale_y_continuous("Bits", lim=c(0,ymax))
