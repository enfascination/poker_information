options(gsubfn.engine = "R")

entropy_adist <- function (x) {sum(c(x / sum(x )) * -log2( x / sum(x )), na.rm=T) }
mutinfo_2ddist <- function (x) {
  mutinfo_2d <- rep(0, length(x))
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) { mutinfo_2d[(i-1)*ncol(x)+j] <- x[i,j]*log2((x[i,j])/(sum(x[,j])*sum(x[i,]))) }
  }
  return(sum(mutinfo_2d, na.rm=T))
}
mutinfo_2ddist_alt <- function (x) {
  mutinfo_2d <- rep(0, nrow(x))
  for (i in 1:nrow(x)) {
    mutinfo_2d[i] <- sum(x[i,])*specinfo_2ddist_alt(x,i)
  }
  return(sum(mutinfo_2d))
}
multivarinformation_3ddist <- function (x) {
  ### I(X;Y;Z) = H(X) + H(Y) + H(Z) - H(X,Y) - H(X,Z) - H(Y,Z) + H(X,Y,Z)
  return(entropy_adist(apply(x,1,sum)) + entropy_adist( apply(x,2,sum) ) + entropy_adist( apply(x,3,sum) ) - entropy_adist(apply(x,c(1,2),sum) ) - entropy_adist(apply(x,c(1,3),sum) ) - entropy_adist(apply(x,c(2,3),sum) ) + entropy_adist(x))
}
interactioninformation_3ddist <- function (x) {
  return(condmutinf_3ddist_alt(x) - mutinfo_2ddist(apply(x,1:2,sum)))
}
condmutinf_3ddist <- function (x) {  ### rows and columns condition on 3rd index
  condent_3d <- x
  condent_3d[,,] <- 0
  for (k in 1:dim(x)[3]) {
    for (j in 1:dim(x)[2]) { 
      for (i in 1:dim(x)[1]) {
        condent_3d[i,j,k] <- x[i,j,k] * log2((apply(x,3,sum)[k]*x[i,j,k])/(apply(x,c(1,3),sum)[i,k]*apply(x,c(2,3),sum)[j,k]))
      }
    }
  }
  return(sum(condent_3d, na.rm=T))
}
condmutinf_3ddist_alt <- function (x) {  ### rows and columns condition on 3rd index
  return( entropy_adist(apply(x,c(1,3),sum) ) + entropy_adist(apply(x,c(2,3),sum) ) - entropy_adist(x) - entropy_adist( apply(x,3,sum) ))
}
condent_2ddist <- function (x) {  ### rows conditions on columns
  condent_2d <- rep(0, length(x))
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) { condent_2d[(i-1)*ncol(x)+j] <- x[i,j]*log2((sum(x[,j]))/(x[i,j])) }
  }
  return(sum(condent_2d, na.rm=T))
}
mininfo_adist <- function(x, focus_var=1) {  ### first dimension is the dimension of interest by default
  mininfo_2d <- rep(0, dim(x)[focus_var])
  xs <- apply(x, focus_var, sum)
  for (i in 1:length(xs)) {
    SIdims <- ((1:length(dim(x)))[-focus_var])
    SIs <- rep(0, length(dim(x))-1)
    for (j in 1:length(SIdims)) {
      SIs[j] <- specinfo_2ddist(apply(x, c(focus_var,SIdims[j]), sum),i)
    }
    mininfo_2d[i] <- xs[i]*min(SIs)
  }
  return(sum(mininfo_2d, na.rm=T))
}
### two implementations suit debugging purposes.  If I got the same answer writing the same equation in two different ways, then each is probably right andI can trust my output.  
mininfo_adist_alt <- function(x, focus_var=1) {  ### first dimension is the dimension of interest by default
  mininfo_2d <- rep(0, dim(x)[focus_var])
  for (i in 1:dim(x)[focus_var]) {
    mininfo_2d[i] <- apply(x, focus_var, sum)[i]*min( 
                              sapply((1:length(dim(x)))[-focus_var], function(d) specinfo_2ddist(apply(x,c(focus_var,d),sum), i) )
                                                    )
  }
  return(sum(mininfo_2d, na.rm=T))
}
maxinfo_adist <- function(x, focus_var=1) {  ### first dimension is the dimension of interest by default
  maxinfo_2d <- rep(0, dim(x)[focus_var])
  for (i in 1:dim(x)[focus_var]) {
    maxinfo_2d[i] <- apply(x, focus_var, sum)[i]*max( 
                              sapply((1:length(dim(x)))[-focus_var], function(d) specinfo_2ddist(apply(x,c(focus_var,d),sum), i) )
                                                    )
  }
  return(sum(maxinfo_2d, na.rm=T))
}
specinfo_2ddist <- function(x, i) {   ###  x is a matrix, and the focal ndex is the first --- rowsh.  i is the value of X, the row, to restrict over.
  x_i_d <- x[i,]/sum(x[i,])
  specinfo <- rep(0, length(x_i_d))
  for (j in 1:ncol(x)) {
    x_j_d <- x[,j]/sum(x[,j])
    specinfo[j] <- x_i_d[j]*(log2(1/sum(x[i,])) - log2(1/x_j_d[i]))
  }
  return(sum(specinfo, na.rm=T))
}
### first variable is focal variable
totalinfo_3ddist_alt2 <- function(x) {
  return(interactioninformation_3ddist(x) + mutinfo_2ddist(apply(x,c(1,2),sum)) + mutinfo_2ddist(apply(x,c(1,3),sum)))
}
#totalinfo_3ddist_alt <- function(x) {
  #return( mutinfo_2ddist(apply(x,c(1,2),sum)) + mutinfo_2ddist(apply(x,c(1,3),sum)) - mininfo_adist_alt(x,1)  ) 
#}
totalinfo_3ddist <- function(x) {
  return( mutinfo_2ddist(apply(x,c(1,3),sum)) +  condmutinf_3ddist_alt(x) ) 
}
totalinfo_3ddist_alt3 <- function(x) {
  return( -interactioninformation_3ddist(x) + condmutinf_3ddist_alt(x) + condmutinf_3ddist_alt(aperm(x, c(1,3,2))) ) 
}
### returns unique info that indexed dimension contains about first dimension;  I can only be 2 or 3
uniqueinfo_3ddist <- function(x, i) {
  mutinfo_2ddist(apply(x,c(1,i),sum)) - mininfo_adist(x, 1)
}
uniqueinfo_3ddist_alt <- function(x, i) {
  rotate <- 1:3
  if (i==3) {rotate <- c(1,3,2)}
  condmutinf_3ddist_alt(aperm(x, rotate)) - synergy_3ddist(x)
}
### returns redundancy that second two dimensions contain about focal first dimension
redundancy_3ddist <- function(x) { mininfo_adist(x, 1) }
### returns synergy that second two dimensions contain about focal first dimension
synergy_3ddist <- function(x) {
  totalinfo_3ddist(x) - uniqueinfo_3ddist(x, 2) - mutinfo_2ddist(apply(x,c(1,3),sum))
}
synergy_3ddist_alt <- function(x) {
  totalinfo_3ddist(x) - uniqueinfo_3ddist(x, 2) - uniqueinfo_3ddist(x, 3)  - redundancy_3ddist(x)
}
#mutinfo_2ddist(sampd)
#mutinfo_2ddist(t(sampd))
#entropy_adist(apply(sampd, 1, sum)) - condent_2ddist(sampd)
#entropy_adist(apply(sampd, 2, sum)) - condent_2ddist(t(sampd))
#mutinfo_2ddist_alt(sampd)
#mutinfo_2ddist_alt(t(sampd))
#interactioninformation_3ddist(apply(dist_2p_ff_d, c(1,2,3), sum))
#interactioninformation_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,2,3)))
#interactioninformation_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,3,2)))
#interactioninformation_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,2,1)))
#-multivarinformation_3ddist(apply(dist_2p_ff_d, c(1,2,3), sum))
#condmutinf_3ddist(apply(dist_2p_ff_d, c(1,2,3), sum))
#condmutinf_3ddist_alt(apply(dist_2p_ff_d, c(1,2,3), sum))
#condmutinf_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,2,3)))
#condmutinf_3ddist_alt(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,2,3)))
#condmutinf_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,3,2)))
#condmutinf_3ddist_alt(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,3,2)))
#condmutinf_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,2,1)))
#condmutinf_3ddist_alt(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,2,1)))
#totalinfo_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,2,1)))
#totalinfo_3ddist_alt2(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,2,1)))
#totalinfo_3ddist_alt3(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,2,1)))
#totalinfo_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,1,2)))
#totalinfo_3ddist_alt2(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,1,2)))
#totalinfo_3ddist_alt3(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(3,1,2)))
#totalinfo_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(2,3,1)))
#totalinfo_3ddist_alt2(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(2,3,1)))
#totalinfo_3ddist_alt3(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(2,3,1)))
#totalinfo_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(2,1,3)))
#totalinfo_3ddist_alt2(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(2,1,3)))
#totalinfo_3ddist_alt3(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(2,1,3)))
#totalinfo_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,3,2)))
#totalinfo_3ddist_alt2(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,3,2)))
#totalinfo_3ddist_alt3(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,3,2)))
#totalinfo_3ddist(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,2,3)))
#totalinfo_3ddist_alt2(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,2,3)))
#totalinfo_3ddist_alt3(aperm(apply(dist_2p_ff_d, c(1,2,3), sum), c(1,2,3)))
#x <- apply(dist_2p_ff_d, c(1,2,3), sum)
#maxinfo_adist(x,1) - mutinfo_2ddist(apply(x,c(1,2),sum)) - mutinfo_2ddist(apply(x,c(1,3),sum)) + mininfo_adist(x,1)
#uniqueinfo_3ddist(x,2) + uniqueinfo_3ddist(x,3) + redundancy_3ddist(x) + synergy_3ddist(x)
#uniqueinfo_3ddist(x,2) + uniqueinfo_3ddist(x,3) + redundancy_3ddist(x) + synergy_3ddist_alt(x)

ff <- array(0,c(2,2,2))
ff[1,1,1] <- 1
  ff[2,1,2] <- 1
 ff[2,2,1] <- 1
 ff[1,2,2] <- 1
ff[2,2,] <- 1
ff <- ff/sum(ff)
mutinfo_2ddist(apply(ff, c(3,1), sum))
 mutinfo_2ddist(apply(ff, c(2,1), sum))
 multivarinformation_3ddist(ff)
 interactioninformation_3ddist(ff)
 mininfo_adist(ff)
 totalinfo_3ddist(ff)
 totalinfo_3ddist_alt2(ff)
 totalinfo_3ddist_alt3(ff)
 redundancy_3ddist(ff)
 synergy_3ddist(ff)
 uniqueinfo_3ddist(ff,2)
 uniqueinfo_3ddist(ff,3)
### end helpers

### upload data
dist_l  <- read.csv("~/projecto/research_projects/poker_information/distrABS005.csv")
dist_l2 <- read.csv("~/projecto/research_projects/poker_information/distrABS010.csv")
dist_l <- rbind(dist_l, dist_l2)
rm(dist_l2)
#dist_l  <- read.csv("~/projecto/research_projects/poker_information/distrABS100.csv")
#dist_l2 <- read.csv("~/projecto/research_projects/poker_information/distrABS060.csv")
#dist_l3 <- read.csv("~/projecto/research_projects/poker_information/distrABS040.csv")
#dist_l <- rbind(dist_l, dist_l2, dist_l3)
#rm(dist_l2)
#rm(dist_l3)
### subset to tables of two people
dist_l <- as.matrix(dist_l[dist_l[,"seats"] ==2,c("street", "skil1", "extr11", "intr1", "skil2", "extr12", "intr2")])
colnames(dist_l) <- c("street", "skil1", "extr1", "intr1", "skil2", "extr2", "intr2")
### get rid of middle players
dist_l <- dist_l[dist_l[,"skil1"] != 1,]
dist_l <- dist_l[dist_l[,"skil2"] != 1,]
dist_l[,"skil1"] <- ifelse(dist_l[,"skil1"] == 2, 1, dist_l[,"skil1"])
dist_l[,"skil2"] <- ifelse(dist_l[,"skil2"] == 2, 1, dist_l[,"skil2"])

### changing this, say by sampling indices with replacement, will let me bootstrap distributiosn and measures over them
indices <- 1:nrow(dist_l)
scales <- c(2,4)
#Rprof(tmp <- tempfile())
#indices <- 1:50000

### changing this, say by sampling indices with replacement, will let me bootstrap distributiosn and measures over them
  #Rprof(tmp <- tempfile())#bootstrap cli
  scales <- c(2,4)
  reps <- 5
  entropies <- matrix(0,ncol=5, nrow=reps)
  statistics <- array(0,c(reps,11,3), dimnames=list(NULL,c("q1", "q2", "q3_i1", "q3_i2","q4","q5", "ti", "mi13", "ti2", "h12","h34"),c("ss","sf","ff")))
  scales <- c(2,4)
  dist_1p_b <- array(0, dim=c(2,3,3), dimnames=c("skill", "extr", "intr"))
  dist_2p_ff <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  dist_2p_sf <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  dist_2p_ss <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  for (e in 1:reps) {
    dist_1p_b[,,] <- 0
    dist_2p_ff[,,,] <- 0
    dist_2p_sf[,,,] <- 0
    dist_2p_ss[,,,] <- 0
    indices <- sample(1:nrow(dist_l), length(indices), replace=T)
    for (i in indices) {
      #print( dist_l[i,] )
      idxs1 <- rbind(dist_l[i,c("skil1", "extr1", "intr1")]+1)
      idxs2 <- rbind(dist_l[i,c("skil2", "extr2", "intr2")]+1)
      if (2 %in% scales) {
        dist_1p_b[idxs1] <- dist_1p_b[idxs1] + 1 
        dist_1p_b[idxs2] <- dist_1p_b[idxs2] + 1 
      }
      if (4 %in% scales) {
        idxs12 <- rbind(dist_l[i,c("extr1", "intr1", "extr2", "intr2")]+1)
        idxs21 <- rbind(dist_l[i,c("extr2", "intr2", "extr1", "intr1")]+1)
        if ((dist_l[i,"skil1"] == 0) & (dist_l[i,"skil2"] == 0)) {
          dist_2p_ff[idxs12] <- dist_2p_ff[idxs12] + 1
          dist_2p_ff[idxs21] <- dist_2p_ff[idxs21] + 1
        }
        else if ((dist_l[i,"skil1"] == 1) & (dist_l[i,"skil2"] == 1)) {
          dist_2p_ss[idxs12] <- dist_2p_ss[idxs12] + 1
          dist_2p_ss[idxs21] <- dist_2p_ss[idxs21] + 1
        }
        else if ((dist_l[i,"skil1"] == 1) & (dist_l[i,"skil2"] == 0)) {
          dist_2p_sf[idxs12] <- dist_2p_sf[idxs12] + 1
        }
        else if ((dist_l[i,"skil1"] == 0) & (dist_l[i,"skil2"] == 1)) {
          dist_2p_sf[idxs21] <- dist_2p_sf[idxs21] + 1
        }
      }
    }
    #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]/sum(dist_1p_b[1,,])), entropy_adist(dist_1p_b[2,,]/sum(dist_1p_b[2,,])))
    entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]), entropy_adist(dist_1p_b[2,,]), entropy_adist(dist_2p_ff), entropy_adist(dist_2p_sf), entropy_adist(dist_2p_ss))
    dist_2p_ss_d <- dist_2p_ss/sum(dist_2p_ss)
    dist_2p_sf_d <- dist_2p_sf/sum(dist_2p_sf)
    dist_2p_ff_d <- dist_2p_ff/sum(dist_2p_ff)
    ss_3vdist <- apply(dist_2p_ss_d, c(1,2,3), sum)
    sf_3vdist <- apply(dist_2p_sf_d, c(1,2,3), sum)
    ff_3vdist <- apply(dist_2p_ff_d, c(1,2,3), sum)
    statistics[e,c("q5"),1] <- redundancy_3ddist( ss_3vdist )
    statistics[e,c("q5"),2] <- redundancy_3ddist( sf_3vdist )
    statistics[e,c("q5"),3] <- redundancy_3ddist( ff_3vdist )
    statistics[e,c("q1","q2"),1] <- c(uniqueinfo_3ddist(ss_3vdist, 2) , uniqueinfo_3ddist(ss_3vdist, 3) )
    statistics[e,c("q1","q2"),2] <- c(uniqueinfo_3ddist(sf_3vdist, 2) , uniqueinfo_3ddist(sf_3vdist, 3)  )
    statistics[e,c("q1","q2"),3] <- c(uniqueinfo_3ddist(ff_3vdist, 2) , uniqueinfo_3ddist(ff_3vdist, 3) )
    statistics[e,c("q4"),1] <- synergy_3ddist( ss_3vdist )
    statistics[e,c("q4"),2] <- synergy_3ddist( sf_3vdist )
    statistics[e,c("q4"),3] <- synergy_3ddist( ff_3vdist )
    statistics[e,c("q3_i1"),1] <- mutinfo_2ddist( apply(dist_2p_ss_d, c(1,2), sum) )
    statistics[e,c("q3_i1"),2] <- mutinfo_2ddist( apply(dist_2p_sf_d, c(1,2), sum) )
    statistics[e,c("q3_i1"),3] <- mutinfo_2ddist( apply(dist_2p_ff_d, c(1,2), sum) )
    statistics[e,c("q3_i2"),1] <- mutinfo_2ddist( apply(dist_2p_ss_d, c(3,4), sum) )
    statistics[e,c("q3_i2"),2] <- mutinfo_2ddist( apply(dist_2p_sf_d, c(3,4), sum) )
    statistics[e,c("q3_i2"),3] <- mutinfo_2ddist( apply(dist_2p_ff_d, c(3,4), sum) )
    statistics[e,c("ti"),1] <- totalinfo_3ddist( ss_3vdist )
    statistics[e,c("ti"),2] <- totalinfo_3ddist( sf_3vdist )
    statistics[e,c("ti"),3] <- totalinfo_3ddist( ff_3vdist )
    statistics[e,c("mi13"),1] <- mutinfo_2ddist( apply(dist_2p_ss_d, c(1,3), sum) )
    statistics[e,c("mi13"),2] <- mutinfo_2ddist( apply(dist_2p_sf_d, c(1,3), sum) )
    statistics[e,c("mi13"),3] <- mutinfo_2ddist( apply(dist_2p_ff_d, c(1,3), sum) )
    statistics[e,c("ti2"),1] <- statistics[e,c("q1"),1] + statistics[e,c("q2"),1] + statistics[e,c("q4"),1] + statistics[e,c("q5"),1]
    statistics[e,c("ti2"),2] <- statistics[e,c("q1"),2] + statistics[e,c("q2"),2] + statistics[e,c("q4"),2] + statistics[e,c("q5"),2]
    statistics[e,c("ti2"),3] <- statistics[e,c("q1"),3] + statistics[e,c("q2"),3] + statistics[e,c("q4"),3] + statistics[e,c("q5"),3]
    statistics[e,c("h12","h34"),1] <- c(entropy_adist(apply(dist_2p_ss_d, c(1,2),sum)), entropy_adist(apply(dist_2p_ss_d, c(3,4),sum)))
    statistics[e,c("h12","h34"),2] <- c(entropy_adist(apply(dist_2p_sf_d, c(1,2),sum)), entropy_adist(apply(dist_2p_sf_d, c(3,4),sum)))
    statistics[e,c("h12","h34"),3] <- c(entropy_adist(apply(dist_2p_ff_d, c(1,2),sum)), entropy_adist(apply(dist_2p_ff_d, c(3,4),sum) ))
    print(statistics[e,,])
  }
  quantile(entropies[,1], c(0.025, 0.5, 0.975))
  quantile(entropies[,2], c(0.025, 0.5, 0.975))
  quantile(entropies[,3], c(0.025, 0.5, 0.975))
  quantile(entropies[,4], c(0.025, 0.5, 0.975))
  quantile(entropies[,5], c(0.025, 0.5, 0.975))
  #Rprof()#bootstrap cli
  #summaryRprof(tmp)


statistics_2p <- statistics
file_output <- "~/projecto/research_projects/poker_information/diststatistics010.csv"
write.csv(statistics, file=file_output)


### setup
#test <- array(0, c(3,3,3,3))
#test[1,2,3,2] <- 1
#idx <- c(1,2,3,2)
### things that work 
#test[1,2,3,2]
#test[t(idx)]
#test[rbind(idx)]
#test[matrix(idx, nrow=1)]
### timing
#system.time(for (i in 1:1000000) test[1,2,3,2] )
#system.time(for (i in 1:1000000) test[rbind(idx)] )
#system.time(for (i in 1:1000000) test[matrix(idx, nrow=1)] )
#system.time(for (i in 1:1000000) test[t(idx)] )
### two vectors
#idx1 <- c(1,2)
#idx2 <- c(3,2)
### things that work
#system.time(for (i in 1:1000000) test[1,2,3,2] )
#system.time(for (i in 1:1000000) test[rbind(c(idx1, idx2))] )
#system.time(for (i in 1:1000000) test[matrix(c(idx1, idx2), nrow=1)] )
#system.time(for (i in 1:1000000) test[t(c(idx1, idx2))] )


### populating distribution testa with observations test_idxs
#test_idxs <- matrix(sample(c(1,2,3), 300000, repl=T), ncol=3)
#testa_for_looped <- array(0, c(3,3,3))
#testa_vectorized <- array(0, c(3,3,3))
#system.time( for (i in 1:nrow(test_idxs)) { testa_for_looped[rbind(test_idxs[i,])] <- testa_for_looped[rbind(test_idxs[i,])] + 1 } )  ## slower
#system.time( testa_vectorized[test_idxs] <- testa_vectorized[test_idxs] + 1  ) ### faster
#sum(testa_for_looped) ### right
#sum(testa_vectorized) ### wrong




#for (e in 1:nrow(entropies)) {
  #dist_2p_ff <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  #dist_2p_sf <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  #dist_2p_ss <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
  #dist_1p_b <- array(0, dim=c(2,3,3), dimnames=c("skill", "extr", "intr"))
  #if (1 %in% scales) {
    #idxs1 <- dist_l[,c("skil1", "extr1", "intr1")]+1
    #idxs2 <- dist_l[,c("skil2", "extr2", "intr2")]+1
    #dist_1p_b[idxs1] <- dist_1p_b[idxs1] + 1 
    #dist_1p_b[idxs2] <- dist_1p_b[idxs2] + 1 
  #}
  ##entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]/sum(dist_1p_b[1,,])), entropy_adist(dist_1p_b[2,,]/sum(dist_1p_b[2,,])))
  #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]), entropy_adist(dist_1p_b[2,,]), entropy_adist(dist_2p_ff), entropy_adist(dist_2p_sf), entropy_adist(dist_2p_ss))
#}
