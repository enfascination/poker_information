
options(gsubfn.engine = "R")
library(abind)
library(ggthemes)
library(data.table)
require(bit64)
library(foreach)
library(doParallel)

### help functions
`%ni%` = Negate(`%in%`) 

### data handing functions
dist_l_filtering_2p <- function(dist_l, extrAsWagerOrAction="wagers", keepID=FALSE) {
  ### subset to tables of two people
	if(!keepID) {
		dist_l <- as.matrix(dist_l[dist_l[,"seats"] ==2,c("street", "skil1", "extr11", "extr21", "intr1", "skil2", "extr12", "extr22", "intr2")])
		colnames(dist_l) <- c("street", "skil1", "extr1", "act1", "intr1", "skil2", "extr2", "act2", "intr2")
	} else {
		dist_l <- as.matrix(dist_l[dist_l[,"seats"] ==2,c("hand", "seats", "blind", "street", "skil1", "extr11", "extr21", "intr1", "skil2", "extr12", "extr22", "intr2")])
		colnames(dist_l) <- c("hand", "seats", "blind", "street", "skil1", "extr1", "act1", "intr1", "skil2", "extr2", "act2", "intr2")
	}
  #print(summary(dist_l))
  #print(str(dist_l))
  #### get rid of middle players.   NO WAIT, don't.  now I need them for accurate estimates on the 'other' category.  just keep in mind that skil was used as an index in counting for some distributions, like the deprecated 2-d distribution for calculating entropy badly.  
  #dist_l <- dist_l[dist_l[,"skil1"] != 1,]
  #dist_l <- dist_l[dist_l[,"skil2"] != 1,]
  #dist_l[,"skil1"] <- ifelse(dist_l[,"skil1"] == 0, 0, dist_l[,"skil1"])
  #dist_l[,"skil2"] <- ifelse(dist_l[,"skil2"] == 0, 0, dist_l[,"skil2"])
  #dist_l[,"skil1"] <- ifelse(dist_l[,"skil1"] == 2, 1, dist_l[,"skil1"])
  #dist_l[,"skil2"] <- ifelse(dist_l[,"skil2"] == 2, 1, dist_l[,"skil2"])

  ### temp robustness check to make abs_020 data work:
  #dist_l[,"skil1"] <- dist_l[,"skil1"] - 1 
  #dist_l[,"skil2"] <- dist_l[,"skil2"] - 1 
  ### to minimize code change's I'll rescale the ratings
  dist_l[,"skil1"] <- (dist_l[,"skil1"] ) / 2
  dist_l[,"skil2"] <- (dist_l[,"skil2"] ) / 2

  ### get rid of the ante street.  there is no strategy in it
  dist_l <- dist_l[dist_l[,"street"] != 0,]
  ### temp robustness check for dependence between streets .  restriction of dataset to preflop play
  #dist_l <- dist_l[dist_l[,"street"] == 1,]
  ### get rid of empty streets, where everyone folded, definitely no strategy there
  dist_l <- dist_l[!((dist_l[,"act1"] == 0) & (dist_l[,"act2"] == 0)),]
  ### then shift action codes down by one
  ### act : currently 0 means out, 1 means fold, 2 is weak and 3 is aggressive
  dist_l[,"act1"] <- dist_l[,"act1"] - 1
  dist_l[,"act2"] <- dist_l[,"act2"] - 1


  if( extrAsWagerOrAction == "actions" ) {
	### bin on actions instead of on wagers
	colnames(dist_l)[which(colnames(dist_l) %in% c("extr1", "act1", "extr2", "act2"))] <- c("extr1_real", "extr1", "extr2_real", "extr2" )
  }

  if (!keepID) {
	return(dist_l[,c("street", "skil1", "extr1", "intr1", "skil2", "extr2", "intr2")])
  } else {
	return(dist_l[,c("hand", "seats", "blind", "street", "skil1", "extr1", "intr1", "skil2", "extr2", "intr2")])
  }
}

### end 2 player chugging fns
build_poker_distribution_2p <- function(dist_l, indices=NA, ordered=F) {
  #entropies <- matrix(0,ncol=5, nrow=reps)
  scales <- c(2,4)
  scales <- c(4)
  #dist_1p_b <- array(0, dim=c(2,3,3), dimnames=list("skill", "extr", "intr"))
  dist_2p_ff <- array(0, dim=c(3,3,3,3), dimnames=list("extr1"=NULL, "intr1"=NULL, "extr2"=NULL, "intr2"=NULL))
  dist_2p_sf <- array(0, dim=c(3,3,3,3), dimnames=list("extr1"=NULL, "intr1"=NULL, "extr2"=NULL, "intr2"=NULL))
  dist_2p_ss <- array(0, dim=c(3,3,3,3), dimnames=list("extr1"=NULL, "intr1"=NULL, "extr2"=NULL, "intr2"=NULL))
  dist_2p_fa <- array(0, dim=c(3,3,3,3), dimnames=list("extr1"=NULL, "intr1"=NULL, "extr2"=NULL, "intr2"=NULL))
  dist_2p_sa <- array(0, dim=c(3,3,3,3), dimnames=list("extr1"=NULL, "intr1"=NULL, "extr2"=NULL, "intr2"=NULL))
  dist_2p_aa <- array(0, dim=c(3,3,3,3), dimnames=list("extr1"=NULL, "intr1"=NULL, "extr2"=NULL, "intr2"=NULL))
    #dist_1p_b[,,] <- 0
    dist_2p_ff[,,,] <- 0
    dist_2p_sf[,,,] <- 0
    dist_2p_ss[,,,] <- 0
    if (is.na(indices)) {
      indices <- sample(1:nrow(dist_l), nrow(dist_l), replace=T)
    }
    for (i in indices) {
      #print( dist_l[i,] )
      #print( i)
      if(all(is.na(dist_l[i,]))) {
        print("problem in data with NAs or someting")
        print( dist_l[i,] )
        print( i)
        next
      }
      #if (2 %in% scales) {
        #idxs1 <- rbind(dist_l[i,c("skil1", "extr1", "intr1")]+1)
        #idxs2 <- rbind(dist_l[i,c("skil2", "extr2", "intr2")]+1)
        #dist_1p_b[idxs1] <- dist_1p_b[idxs1] + 1 
        #dist_1p_b[idxs2] <- dist_1p_b[idxs2] + 1 
      #}
      if (4 %in% scales) {
        idxs12 <- rbind(dist_l[i,c("extr1", "intr1", "extr2", "intr2")]+1)
        idxs21 <- rbind(dist_l[i,c("extr2", "intr2", "extr1", "intr1")]+1)
        #print(dist_l[i,])
        #print(idxs21)
        if(!ordered) {picked_player <- sample(c(0,1), 1)}  ### there is tricky statistical dependence, and tricky sampling bias, in using both player in a hand
        #print(c(ordered, picked_player))
        if ((dist_l[i,"skil1"] == 0) && (dist_l[i,"skil2"] == 0)) {
          if(!ordered) { 
            if (picked_player == 0 ) {
              dist_2p_ff[idxs12] <- dist_2p_ff[idxs12] + 1 
              dist_2p_fa[idxs12] <- dist_2p_fa[idxs12] + 1
              dist_2p_aa[idxs12] <- dist_2p_aa[idxs12] + 1
            } else {
              dist_2p_ff[idxs21] <- dist_2p_ff[idxs21] + 1
              dist_2p_fa[idxs21] <- dist_2p_fa[idxs21] + 1
              dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
            }
          } else {
            dist_2p_ff[idxs21] <- dist_2p_ff[idxs21] + 1
            dist_2p_fa[idxs21] <- dist_2p_fa[idxs21] + 1
            dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
          }
        }
        else if ((dist_l[i,"skil1"] == 1) && (dist_l[i,"skil2"] == 1)) {
          if(!ordered) { 
            if (picked_player == 0 ) {
              dist_2p_ss[idxs12] <- dist_2p_ss[idxs12] + 1 
              dist_2p_sa[idxs12] <- dist_2p_sa[idxs12] + 1
              dist_2p_aa[idxs12] <- dist_2p_aa[idxs12] + 1
            } else { 
              dist_2p_ss[idxs21] <- dist_2p_ss[idxs21] + 1
              dist_2p_sa[idxs21] <- dist_2p_sa[idxs21] + 1
              dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
            }
          } else {
            dist_2p_ss[idxs21] <- dist_2p_ss[idxs21] + 1
            dist_2p_sa[idxs21] <- dist_2p_sa[idxs21] + 1
            dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
          }
        }
        else if ((dist_l[i,"skil1"] == 1) && (dist_l[i,"skil2"] == 0)) {
          if(!ordered) { 
            if (picked_player == 0 ) {
              dist_2p_sf[idxs12] <- dist_2p_sf[idxs12] + 1 
              dist_2p_sa[idxs12] <- dist_2p_sa[idxs12] + 1
              dist_2p_aa[idxs12] <- dist_2p_aa[idxs12] + 1
            } else { 
              dist_2p_fa[idxs21] <- dist_2p_fa[idxs21] + 1
              dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
            }
          } else {
            dist_2p_fa[idxs21] <- dist_2p_fa[idxs21] + 1
            dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
          }
        }
        else if ((dist_l[i,"skil1"] == 0) && (dist_l[i,"skil2"] == 1)) {
          if(!ordered) { 
            if (picked_player == 0 ) {
              dist_2p_fa[idxs12] <- dist_2p_fa[idxs12] + 1
              dist_2p_aa[idxs12] <- dist_2p_aa[idxs12] + 1
            } else { 
              dist_2p_sf[idxs21] <- dist_2p_sf[idxs21] + 1
              dist_2p_sa[idxs21] <- dist_2p_sa[idxs21] + 1
              dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
            }
          } else {
            dist_2p_sf[idxs21] <- dist_2p_sf[idxs21] + 1
            dist_2p_sa[idxs21] <- dist_2p_sa[idxs21] + 1
            dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
          }
        }
        else if ((dist_l[i,"skil1"] == 0.5) && (dist_l[i,"skil2"] == 0)) {
          if(ordered || (picked_player != 0 )) {
            dist_2p_fa[idxs21] <- dist_2p_fa[idxs21] + 1
            dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
          }
        }
        else if ((dist_l[i,"skil1"] == 0.5) && (dist_l[i,"skil2"] == 1)) {
          if(ordered || (picked_player != 0 )) {
            dist_2p_sa[idxs21] <- dist_2p_sa[idxs21] + 1
            dist_2p_aa[idxs21] <- dist_2p_aa[idxs21] + 1
          }
        } 
        else if ((dist_l[i,"skil1"] == 0) && (dist_l[i,"skil2"] == 0.5)) {
          if(!ordered && (picked_player == 0 )) {
            dist_2p_fa[idxs12] <- dist_2p_fa[idxs12] + 1
            dist_2p_aa[idxs12] <- dist_2p_aa[idxs12] + 1
          }
        }
        else if ((dist_l[i,"skil1"] == 1) && (dist_l[i,"skil2"] == 0.5)) {
          if(!ordered && (picked_player == 0 )) {
            dist_2p_sa[idxs12] <- dist_2p_sa[idxs12] + 1
            dist_2p_aa[idxs12] <- dist_2p_aa[idxs12] + 1
          }
        } 
        else if (((dist_l[i,"skil1"] == 0.5) && (dist_l[i,"skil2"] == 0.5))
               ) {
          next
        }
        else {print (paste("BIG ERROR MISSING HITS ASFDASFASASGSFGFGGGGG", dist_l[i,c("skil1")],dist_l[i,c( "skil2")]))}
      }
    }
    dist_2p <- list()
    dist_2p$ss <- dist_2p_ss
    dist_2p$sf <- dist_2p_sf
    dist_2p$ff <- dist_2p_ff
    dist_2p$fa <- dist_2p_fa
    dist_2p$sa <- dist_2p_sa
    dist_2p$aa <- dist_2p_aa
    return(dist_2p)
  }

get_statistics_2p <- function(dist_2p, specific=NA, quiet=FALSE) {
  statistics <- array(0,c(16), dimnames=list(c("ui1", "ue2", "syn","rdn", "mi12", "mi34", "mi13", "ti", "ti2", "h1","h2","h3","h12","h34","h13","h123")))
  dist_2p_d <- dist_2p/sum(dist_2p)
  dist_2p_3d <- apply(dist_2p_d, c(1,2,3), sum)
  if (is.na(specific)) {
    statistics[c("rdn")] <- redundancy_3ddist( dist_2p_3d )
    statistics[c("ui1","ue2")] <- c(uniqueinfo_3ddist(dist_2p_3d, 2) , uniqueinfo_3ddist(dist_2p_3d, 3) )
    statistics[c("syn")] <- synergy_3ddist( dist_2p_3d )
    statistics[c("mi12")] <- mutinfo_2ddist( apply(dist_2p_d, c(1,2), sum) )
    statistics[c("mi34")] <- mutinfo_2ddist( apply(dist_2p_d, c(3,4), sum) )
    statistics[c("mi13")] <- mutinfo_2ddist( apply(dist_2p_d, c(1,3), sum) )
    statistics[c("ti")] <- totalinfo_3ddist( dist_2p_3d )
    statistics[c("ti2")] <- statistics[c("ui1")] + statistics[c("ue2")] + statistics[c("syn")] + statistics[c("rdn")]
    statistics[c("h1","h2","h3")] <- c(entropy_adist(apply(dist_2p_d, c(1),sum)), entropy_adist(apply(dist_2p_d, c(2),sum)), entropy_adist(apply(dist_2p_d, c(3),sum)))
    statistics[c("h12","h34","h13")] <- c(entropy_adist(apply(dist_2p_d, c(1,2),sum)), entropy_adist(apply(dist_2p_d, c(3,4),sum)), entropy_adist(apply(dist_2p_d, c(1,3),sum)))
    statistics[c("h123")] <- entropy_adist(dist_2p_3d )
  } else {
    statistics[c("rdn")] <- specific_redundancy_3ddist( dist_2p_3d , specific)
    statistics[c("ui1","ue2")] <- c(specific_uniqueinfo_3ddist(dist_2p_3d, specific, 2) , specific_uniqueinfo_3ddist(dist_2p_3d, specific, 3) )
    statistics[c("syn")] <- specific_synergy_3ddist( dist_2p_3d , specific)
    statistics[c("mi12")] <- specinfo_2ddist( apply(dist_2p_d, c(1,2), sum) , specific)
    statistics[c("mi34")] <- specinfo_2ddist( apply(dist_2p_d, c(3,4), sum) , specific)
    statistics[c("mi13")] <- specinfo_2ddist( apply(dist_2p_d, c(1,3), sum) , specific)
    statistics[c("ti")] <- specinfo_3ddist( dist_2p_3d , specific)
    statistics[c("ti2")] <- statistics[c("ui1")] + statistics[c("ue2")] + statistics[c("syn")] + statistics[c("rdn")]
    statistics[c("h1","h2","h3")] <- c(entropy_adist(apply(dist_2p_d, c(1),sum)[specific]/sum(apply(dist_2p_d, c(1),sum)[specific])), entropy_adist(apply(dist_2p_d, c(2),sum)[specific]/sum(apply(dist_2p_d, c(2),sum)[specific])), entropy_adist(apply(dist_2p_d, c(3),sum)[specific]/sum(apply(dist_2p_d, c(3),sum)[specific])))
    statistics[c("h12","h34","h13")] <- c(entropy_adist(apply(dist_2p_d, c(1,2),sum)[specific,]/sum(apply(dist_2p_d, c(1,2),sum)[specific,])), entropy_adist(apply(dist_2p_d, c(3,4),sum)[specific,]/sum(apply(dist_2p_d, c(3,4),sum)[specific,])), entropy_adist(apply(dist_2p_d, c(1,3),sum)[specific,]/sum(apply(dist_2p_d, c(1,3),sum)[specific,])))
    statistics[c("h123")] <- entropy_adist(dist_2p_3d[specific,,] )
    }
  if (! quiet) { print(statistics[]) }
  return(statistics)
}
get_statistics_2p_deprecated <- function(dist_2p_ss, dist_2p_sf, dist_2p_ff) {
  statistics <- array(0,c(1,12,3), dimnames=list(NULL,c("ui1", "ue2", "syn","rdn", "mi12", "mi34", "mi13", "ti", "ti2", "h12","h34","h13"),c("ss","sf","ff")))
  e <- 1
    #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]/sum(dist_1p_b[1,,])), entropy_adist(dist_1p_b[2,,]/sum(dist_1p_b[2,,])))
    #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]), entropy_adist(dist_1p_b[2,,]), entropy_adist(dist_2p_ff), entropy_adist(dist_2p_sf), entropy_adist(dist_2p_ss))
    dist_2p_ss_d <- dist_2p_ss/sum(dist_2p_ss)
    dist_2p_sf_d <- dist_2p_sf/sum(dist_2p_sf)
    dist_2p_ff_d <- dist_2p_ff/sum(dist_2p_ff)
    ss_3vdist <- apply(dist_2p_ss_d, c(1,2,3), sum)
    sf_3vdist <- apply(dist_2p_sf_d, c(1,2,3), sum)
    ff_3vdist <- apply(dist_2p_ff_d, c(1,2,3), sum)
    if (is.na(specific)) {
      statistics[e,c("rdn"),1] <- redundancy_3ddist( ss_3vdist )
      statistics[e,c("rdn"),2] <- redundancy_3ddist( sf_3vdist )
      statistics[e,c("rdn"),3] <- redundancy_3ddist( ff_3vdist )
      statistics[e,c("ui1","ue2"),1] <- c(uniqueinfo_3ddist(ss_3vdist, 2) , uniqueinfo_3ddist(ss_3vdist, 3) )
      statistics[e,c("ui1","ue2"),2] <- c(uniqueinfo_3ddist(sf_3vdist, 2) , uniqueinfo_3ddist(sf_3vdist, 3)  )
      statistics[e,c("ui1","ue2"),3] <- c(uniqueinfo_3ddist(ff_3vdist, 2) , uniqueinfo_3ddist(ff_3vdist, 3) )
      statistics[e,c("syn"),1] <- synergy_3ddist( ss_3vdist )
      statistics[e,c("syn"),2] <- synergy_3ddist( sf_3vdist )
      statistics[e,c("syn"),3] <- synergy_3ddist( ff_3vdist )
      statistics[e,c("mi12"),1] <- mutinfo_2ddist( apply(dist_2p_ss_d, c(1,2), sum) )
      statistics[e,c("mi12"),2] <- mutinfo_2ddist( apply(dist_2p_sf_d, c(1,2), sum) )
      statistics[e,c("mi12"),3] <- mutinfo_2ddist( apply(dist_2p_ff_d, c(1,2), sum) )
      statistics[e,c("mi34"),1] <- mutinfo_2ddist( apply(dist_2p_ss_d, c(3,4), sum) )
      statistics[e,c("mi34"),2] <- mutinfo_2ddist( apply(dist_2p_sf_d, c(3,4), sum) )
      statistics[e,c("mi34"),3] <- mutinfo_2ddist( apply(dist_2p_ff_d, c(3,4), sum) )
      statistics[e,c("mi13"),1] <- mutinfo_2ddist( apply(dist_2p_ss_d, c(1,3), sum) )
      statistics[e,c("mi13"),2] <- mutinfo_2ddist( apply(dist_2p_sf_d, c(1,3), sum) )
      statistics[e,c("mi13"),3] <- mutinfo_2ddist( apply(dist_2p_ff_d, c(1,3), sum) )
      statistics[e,c("ti"),1] <- totalinfo_3ddist( ss_3vdist )
      statistics[e,c("ti"),2] <- totalinfo_3ddist( sf_3vdist )
      statistics[e,c("ti"),3] <- totalinfo_3ddist( ff_3vdist )
      statistics[e,c("ti2"),1] <- statistics[e,c("ui1"),1] + statistics[e,c("ue2"),1] + statistics[e,c("syn"),1] + statistics[e,c("rdn"),1]
      statistics[e,c("ti2"),2] <- statistics[e,c("ui1"),2] + statistics[e,c("ue2"),2] + statistics[e,c("syn"),2] + statistics[e,c("rdn"),2]
      statistics[e,c("ti2"),3] <- statistics[e,c("ui1"),3] + statistics[e,c("ue2"),3] + statistics[e,c("syn"),3] + statistics[e,c("rdn"),3]
      statistics[e,c("h12","h34","h13"),1] <- c(entropy_adist(apply(dist_2p_ss_d, c(1,2),sum)), entropy_adist(apply(dist_2p_ss_d, c(3,4),sum)), entropy_adist(apply(dist_2p_ss_d, c(1,3),sum)))
      statistics[e,c("h12","h34","h13"),2] <- c(entropy_adist(apply(dist_2p_sf_d, c(1,2),sum)), entropy_adist(apply(dist_2p_sf_d, c(3,4),sum)), entropy_adist(apply(dist_2p_sf_d, c(1,3),sum)))
      statistics[e,c("h12","h34","h13"),3] <- c(entropy_adist(apply(dist_2p_ff_d, c(1,2),sum)), entropy_adist(apply(dist_2p_ff_d, c(3,4),sum) ), entropy_adist(apply(dist_2p_ff_d, c(1,3),sum)))
    } else {
      statistics[e,c("rdn"),1] <- specific_redundancy_3ddist( ss_3vdist , specific)
      statistics[e,c("rdn"),2] <- specific_redundancy_3ddist( sf_3vdist , specific)
      statistics[e,c("rdn"),3] <- specific_redundancy_3ddist( ff_3vdist , specific)
      statistics[e,c("ui1","ue2"),1] <- c(specific_uniqueinfo_3ddist(ss_3vdist, specific, 2) , specific_uniqueinfo_3ddist(ss_3vdist, specific, 3) )
      statistics[e,c("ui1","ue2"),2] <- c(specific_uniqueinfo_3ddist(sf_3vdist, specific, 2) , specific_uniqueinfo_3ddist(sf_3vdist, specific, 3) )
      statistics[e,c("ui1","ue2"),3] <- c(specific_uniqueinfo_3ddist(ff_3vdist, specific, 2) , specific_uniqueinfo_3ddist(ff_3vdist, specific, 3) )
      statistics[e,c("syn"),1] <- specific_synergy_3ddist( ss_3vdist , specific)
      statistics[e,c("syn"),2] <- specific_synergy_3ddist( sf_3vdist , specific)
      statistics[e,c("syn"),3] <- specific_synergy_3ddist( ff_3vdist , specific)
      statistics[e,c("mi12"),1] <- specinfo_2ddist( apply(dist_2p_ss_d, c(1,2), sum) , specific)
      statistics[e,c("mi12"),2] <- specinfo_2ddist( apply(dist_2p_sf_d, c(1,2), sum) , specific)
      statistics[e,c("mi12"),3] <- specinfo_2ddist( apply(dist_2p_ff_d, c(1,2), sum) , specific)
      statistics[e,c("mi34"),1] <- specinfo_2ddist( apply(dist_2p_ss_d, c(3,4), sum) , specific)
      statistics[e,c("mi34"),2] <- specinfo_2ddist( apply(dist_2p_sf_d, c(3,4), sum) , specific)
      statistics[e,c("mi34"),3] <- specinfo_2ddist( apply(dist_2p_ff_d, c(3,4), sum) , specific)
      statistics[e,c("mi13"),1] <- specinfo_2ddist( apply(dist_2p_ss_d, c(1,3), sum) , specific)
      statistics[e,c("mi13"),2] <- specinfo_2ddist( apply(dist_2p_sf_d, c(1,3), sum) , specific)
      statistics[e,c("mi13"),3] <- specinfo_2ddist( apply(dist_2p_ff_d, c(1,3), sum) , specific)
      statistics[e,c("ti"),1] <- specinfo_3ddist( ss_3vdist , specific)
      statistics[e,c("ti"),2] <- specinfo_3ddist( sf_3vdist , specific)
      statistics[e,c("ti"),3] <- specinfo_3ddist( ff_3vdist , specific)
      statistics[e,c("ti2"),1] <- statistics[e,c("ui1"),1] + statistics[e,c("ue2"),1] + statistics[e,c("syn"),1] + statistics[e,c("rdn"),1]
      statistics[e,c("ti2"),2] <- statistics[e,c("ui1"),2] + statistics[e,c("ue2"),2] + statistics[e,c("syn"),2] + statistics[e,c("rdn"),2]
      statistics[e,c("ti2"),3] <- statistics[e,c("ui1"),3] + statistics[e,c("ue2"),3] + statistics[e,c("syn"),3] + statistics[e,c("rdn"),3]
      statistics[e,c("h12","h34","h13"),1] <- c(entropy_adist(apply(dist_2p_ss_d, c(1,2),sum)[specific,]), entropy_adist(apply(dist_2p_ss_d, c(3,4),sum)[specific,]), entropy_adist(apply(dist_2p_ss_d, c(1,3),sum)[specific,]))
      statistics[e,c("h12","h34","h13"),2] <- c(entropy_adist(apply(dist_2p_sf_d, c(1,2),sum)[specific,]), entropy_adist(apply(dist_2p_sf_d, c(3,4),sum)[specific,]), entropy_adist(apply(dist_2p_sf_d, c(1,3),sum)[specific,]))
      statistics[e,c("h12","h34","h13"),3] <- c(entropy_adist(apply(dist_2p_ff_d, c(1,2),sum)[specific,]), entropy_adist(apply(dist_2p_ff_d, c(3,4),sum)[specific,]), entropy_adist(apply(dist_2p_ff_d, c(1,3),sum)[specific,]))
    }
    print(statistics[e,,])
  #quantile(entropies[,1], c(0.025, 0.5, 0.975))
  #quantile(entropies[,2], c(0.025, 0.5, 0.975))
  #quantile(entropies[,3], c(0.025, 0.5, 0.975))
  #quantile(entropies[,4], c(0.025, 0.5, 0.975))
  #quantile(entropies[,5], c(0.025, 0.5, 0.975))
  #Rprof()#bootstrap cli
  #summaryRprof(tmp)
  return(statistics)
}

bootstrap_statistics_2p_deprecated <- function(dist_l, reps) {
  ### filter
  dist_l <- dist_l_filtering_2p( dist_l ) 
  
  statistics <- array(0,c(reps,16,3), dimnames=list(NULL,c("ui1", "ue2", "syn","rdn", "mi12", "mi34", "mi13", "ti", "ti2","h1","h2","h3", "h12","h34","h13","h123"),c("ss","sf","ff")))
  for (e in 1:reps) {
    print(e)
    dist_2p <- build_poker_distribution_2p(dist_l)
    #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]/sum(dist_1p_b[1,,])), entropy_adist(dist_1p_b[2,,]/sum(dist_1p_b[2,,])))
    #entropies[e,] <- c(entropy_adist(dist_1p_b[1,,]), entropy_adist(dist_1p_b[2,,]), entropy_adist(dist_2p_ff), entropy_adist(dist_2p_sf), entropy_adist(dist_2p_ss))
    statistics[e,,] <- get_statistics_2p_deprecated(dist_2p$ss, dist_2p$sf, dist_2p$ff)
    #dist_2p_ss_d <- dist_2p$ss_d
    #dist_2p_sf_d <- dist_2p$sf_d
    #dist_2p_ff_d <- dist_2p$ff_d
  }
  return(statistics)
}

bootstrap_distributions_2p <- function(dist_l, reps, ordered=F, ...) {
  dist_l_filt <- dist_l_filtering_2p( dist_l, ... ) ### filter
  distributions <- list()
  for (e in 1:reps) {
    print(e)
    distributions[[e]] <- build_poker_distribution_2p(dist_l_filt, ordered=ordered)
  }
  return(distributions)
}
bootstrap_distributions_2p_par <- function(dist_l, reps, ordered=F, ...) {
  dist_l_filt <- dist_l_filtering_2p( dist_l, ... ) ### filter
  distributions <- foreach(e=1:reps, .export=c('build_poker_distribution_2p')) %dopar% {
		print(e)
		build_poker_distribution_2p(dist_l_filt, ordered=ordered)
  }
  return(distributions)
}

bootstrap_distributions_to_file <- function(dist_l, output_file, reps, ordered=NA, ...) {
    Sys.time()
    #distributions <- bootstrap_distributions_2p(dist_l, reps, ordered=ordered, ...)
    distributions <- bootstrap_distributions_2p_par(dist_l, reps, ordered=ordered, ...)
    save(distributions, file=output_file)
    Sys.time()
}

censor_dist <- function(distribution, onlyObservedHands=F) {
  #return(distribution[2:3,1:3,2:3,1:3])  ### remove wagers of zero
	if (onlyObservedHands) {
		return(distribution[1:3,2:3,1:3,2:3])  ### remove unobserved hands (hands that didn't go to showdown
	} else {
		return(distribution)
	}
}

get_statistics_2p_from_file <- function(input_file, specific=NA, quiet=FALSE, onlyObsHands=F) {
  load(file=input_file)
  statistics <- abind(
		get_statistics_2p(censor_dist(distributions[[1]]$sa, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[1]]$fa, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[1]]$sf, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[1]]$ff, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[1]]$aa, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		rev.along=0, new.names=list(NULL, c("sa", "fa", "sf", "ff", "aa")))
  if(length(distributions)<=1) {
    ### make it into three  dimensions, adding empty trivial d to the first d
    statistics <- abind(statistics, along=0)
  } else {
    for (j in 2:length(distributions)) {
      if (length(dim(statistics))==2) { along = 0 
      } else { along = 1 }
      statistics <- abind(statistics, abind(
		get_statistics_2p(censor_dist(distributions[[j]]$sa, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[j]]$fa, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[j]]$sf, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[j]]$ff, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		get_statistics_2p(censor_dist(distributions[[j]]$aa, onlyObservedHands=onlyObsHands), specific=specific, quiet=quiet), 
		rev.along=0), along=along)
    }
  }
  print(dim(statistics))
  return(statistics)
}
### the normalized values got calculated off the wrong total nformation and apparently had to be recalculated.  don't know why I didn't just fix the bug instead of writing this new function.  Oh, the total information for the specific information is different than that for the general calculation.  This is for getting normalized specific information
stat_long_spec_normalized_patch <- function(stat_long, skip=FALSE) {
  if (skip) {  ### if I'm not going to fix these values then I should remove them from the data, because they're wrong.
    return(stat_long[stat_long$q %ni% c("ti2", "ui1n", "ue2n", "rdnn", "synn"),])
  }
  #global_rows <- rep(0, nrow(stat_long)/3)
  last_rep <- -1
  stat_long_rep <- stat_long[1:(length(unique(stat_long$q))*length(unique(stat_long$specific))),]
  for (i in 1:nrow(stat_long)) {
    if (last_rep != stat_long[i,"rep"]) {
      stat_long_rep[,]  <- stat_long[((stat_long[i,"rep"] == stat_long[,"rep"])
              & (stat_long[i,"setting"] == stat_long[,"setting"])
              & (stat_long[i,"blind"] == stat_long[,"blind"])
              & !is.na(stat_long[i,"specific"])
              ),]
      ti <- sum(stat_long_rep[("ti" == stat_long_rep[,"q"]),"val"])
      last_rep <- stat_long[i,"rep"]
    }
    if (stat_long[i,"q"] == "ti2")  { stat_long[i,"val"] <- ti}
    ### maybe an improvement on what's to follow:
    if ( FALSE & (stat_long[i,"q"] %in% c("ui1n", "ue2n", "rdnn", "synn"))) {
      stat_long_rep_spec <- stat_long_rep[(stat_long[i,"specific"] == stat_long_rep[,"specific"]),c('q', "val")]
      if (stat_long[i,"q"] == "ui1n") {
        stat_long[i,"val"] <- stat_long_rep_spec[("ui1" == stat_long_rep_spec[,"q"]),"val"]/ti }
      if (stat_long[i,"q"] == "ue2n") {
        stat_long[i,"val"] <- stat_long_rep_spec[("ue2" == stat_long_rep_spec[,"q"]),"val"]/ti }
      if (stat_long[i,"q"] == "rdnn") {
        stat_long[i,"val"] <- stat_long_rep_spec[("rdn" == stat_long_rep_spec[,"q"]),"val"]/ti }
      if (stat_long[i,"q"] == "synn") {
        stat_long[i,"val"] <- stat_long_rep_spec[("syn" == stat_long_rep_spec[,"q"]),"val"]/ti }
    }
    if (stat_long[i,"q"] == "ui1n") {
  stat_long[i,"val"] <- stat_long_rep[(stat_long[i,"specific"] == stat_long_rep[,"specific"]) & ("ui1" == stat_long_rep[,"q"]),"val"]/ti }
    if (stat_long[i,"q"] == "ue2n") {
  stat_long[i,"val"] <- stat_long_rep[(stat_long[i,"specific"] == stat_long_rep[,"specific"]) & ("ue2" == stat_long_rep[,"q"]),"val"]/ti }
    if (stat_long[i,"q"] == "rdnn") {
  stat_long[i,"val"] <- stat_long_rep[(stat_long[i,"specific"] == stat_long_rep[,"specific"]) & ("rdn" == stat_long_rep[,"q"]),"val"]/ti }
    if (stat_long[i,"q"] == "synn") {
  stat_long[i,"val"] <- stat_long_rep[(stat_long[i,"specific"] == stat_long_rep[,"specific"]) & ("syn" == stat_long_rep[,"q"]),"val"]/ti }
  }
  return(stat_long)
}
stat_long_normalized <- function(stat_long, setting) {
  stat_long_t <- stat_long[0,c("rep", "val", "q")]
  for (i in 1:max(stat_long$rep)) {
      ti3 <- sum(stat_long[(stat_long$rep==i) & (stat_long$q %in% c("ui1", "ue2", "syn", "rdn")),"val"])
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=ti3, q="ti3"))
      ti <- sum(stat_long[(stat_long$rep==i) & (stat_long$q %in% c("ti2")),"val"])
      h1 <- sum(stat_long[(stat_long$rep==i) & (stat_long$q == "h1"), "val" ])
      h12 <- sum(stat_long[(stat_long$rep==i) & (stat_long$q == "h12"), "val" ])
      h34 <- sum(stat_long[(stat_long$rep==i) & (stat_long$q == "h34"), "val" ])
      h13 <- sum(stat_long[(stat_long$rep==i) & (stat_long$q == "h13"), "val" ])
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "ui1"),"val"]/ti, q="ui1n"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "ue2"),"val"]/ti, q="ue2n"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "syn"),"val"]/ti, q="synn"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "rdn"),"val"]/ti, q="rdnn"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "ti"),"val"]/ti, q="tin"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "ti"),"val"]/h1, q="ti_h1"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "mi12"),"val"]/h12, q="mi12n"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "mi34"),"val"]/h34, q="mi34n"))
      stat_long_t <- rbind(stat_long_t, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$q == "mi13"),"val"]/h13, q="mi13n"))
    }
  stat_long_t <- cbind(stat_long_t, setting=setting)
  return(stat_long_t)
}

### converts output of build_poker_distribution_2p  into long format for graphing on um ggplot
build_stat_long_2p <- function(statistics) {
  stat_long <- data.frame(rep=as.numeric(rep(0,length(statistics))), val=0, q=0, setting=0)
  #print(length(rep(0,length(statistics))))
  #print(dim(abind(statistics, along=0)))
  for (j in 1:dim(statistics)[2]) {
    for (k in 1:dim(statistics)[3]) {
      idx <- (1:dim(statistics)[1])+((j-1)*(dim(statistics)[1]))+((k-1)*(prod(dim(statistics)[1:2])))
      stat_long[idx,1:2] <- cbind(1:dim(statistics)[1], statistics[,j,k])
      stat_long[idx,3] <- attr( statistics, "dimnames")[[2]][j]
      stat_long[idx,4] <- attr( statistics, "dimnames")[[3]][k]
      #print((dim(statistics)[1])+((j-1)*(dim(statistics)[1]))+((k-1)*(prod(dim(statistics)[1:2]))))
    }
  }
  #print(head(stat_long))
  #stat_long$ti <- 0
  #stat_long$ti[stat_long$setting=="ff"] <-  stat_long[(stat_long$setting=="ff")&(stat_long$q=="ti"),"val"]  
  #stat_long$ti[stat_long$setting=="ss"] <-  stat_long[(stat_long$setting=="ss")&(stat_long$q=="ti"),"val"]  
  #stat_long$ti[stat_long$setting=="sf"] <-  stat_long[(stat_long$setting=="sf")&(stat_long$q=="ti"),"val"]  
  stat_long <- rbind(stat_long, stat_long_normalized(stat_long[stat_long$setting=="ff",], "ff"), stat_long_normalized(stat_long[stat_long$setting=="sf",], "sf"), stat_long_normalized(stat_long[stat_long$setting=="sa",], "sa"), stat_long_normalized(stat_long[stat_long$setting=="fa",], "fa"), stat_long_normalized(stat_long[stat_long$setting=="aa",], "aa"))

  stat_long$q <- factor(stat_long$q)
  stat_long$setting <- with(stat_long, ifelse(setting=="ss", "Shark v Shark", stat_long$setting))
  stat_long$setting <- with(stat_long, ifelse(setting=="ff", "Fish v Fish", stat_long$setting))
  stat_long$setting <- with(stat_long, ifelse(setting=="sf", "Shark v Fish", stat_long$setting))
  stat_long$setting <- with(stat_long, ifelse(setting=="fa", "Fish v Other", stat_long$setting))
  stat_long$setting <- with(stat_long, ifelse(setting=="sa", "Shark v Other", stat_long$setting))
  stat_long$setting <- with(stat_long, ifelse(setting=="aa", "All v All", stat_long$setting))
  stat_long$setting <- factor(stat_long$setting)
  return(stat_long)
}
### end 2 player chugging fns

### start 3 player chugging fns
###  unfortunatey, i cannot assume that this code works, or that it ever did.  lots hasn't been updated.  ordering won't work, compare closely tiwth the 2- code.
build_poker_distribution_3p <- function(dist_l, reps, merge_middle=FALSE) {
  ### subset to tables of two people
  ###  bets
  print(dim(dist_l))
  dist_l <- as.matrix(dist_l[dist_l[,"seats"] ==3,c("street", "skil1", "extr11", "skil2", "extr12", "skil3", "extr13" )])
  ###  actions
  #dist_l <- as.matrix(dist_l[dist_l[,"seats"] ==3,c("street", "skil1", "extr21", "skil2", "extr22", "skil3", "extr23" )])
  colnames(dist_l) <- c("street", "skil1", "extr1", "skil2", "extr2", "skil3", "extr3")
  ### deal with middle players
  if (!merge_middle) {
    ### get rid of middle players
    print(dim(dist_l))
    dist_l <- dist_l[dist_l[,"skil1"] != 2,]
    print(dim(dist_l))
    dist_l <- dist_l[dist_l[,"skil2"] != 2,]
    print(dim(dist_l))
    dist_l <- dist_l[dist_l[,"skil3"] != 2,]
    print(dim(dist_l))
  } else {
    ### merge middle players
    print("1")
    dist_l[,"skil1"] <- ifelse(dist_l[,"skil1"] == 2, 0, dist_l[,"skil1"])
    print("2")
    dist_l[,"skil2"] <- ifelse(dist_l[,"skil2"] == 2, 0, dist_l[,"skil2"])
    print("3")
    dist_l[,"skil3"] <- ifelse(dist_l[,"skil3"] == 2, 0, dist_l[,"skil3"])
    print("4")
  }
  dist_l[,"skil1"] <- ifelse(dist_l[,"skil1"] == 1, 0, dist_l[,"skil1"])
  dist_l[,"skil2"] <- ifelse(dist_l[,"skil2"] == 1, 0, dist_l[,"skil2"])
  dist_l[,"skil3"] <- ifelse(dist_l[,"skil3"] == 1, 0, dist_l[,"skil3"])
  dist_l[,"skil1"] <- ifelse(dist_l[,"skil1"] == 3, 1, dist_l[,"skil1"])
  dist_l[,"skil2"] <- ifelse(dist_l[,"skil2"] == 3, 1, dist_l[,"skil2"])
  dist_l[,"skil3"] <- ifelse(dist_l[,"skil3"] == 3, 1, dist_l[,"skil3"])

  ### changing this, say by sampling indices with replacement, will let me bootstrap distributiosn and measures over them
  indices <- 1:nrow(dist_l)
  testing <-F
  #Rprof(tmp <- tempfile())
  #indices <- 1:50000

  ### changing this, say by sampling indices with replacement, will let me bootstrap distributiosn and measures over them
    #Rprof(tmp <- tempfile())#bootstrap cli
    #reps <- 60
    statistics <- array(0,c(reps,9,4), dimnames=list(NULL,c("h1f", "h2p", "h3s", "hfps", "u2", "u3", "rdn", "syn","ti"),c("sss", "fss","ffs", "fff")))
    dist_3p_sss <- array(0, dim=c(3,3,3), dimnames=c("extr1", "extr2", "extr3"))
    dist_3p_fss <- array(0, dim=c(3,3,3), dimnames=c("extr1", "extr2", "extr3"))
    dist_3p_ffs <- array(0, dim=c(3,3,3), dimnames=c("extr1", "extr2", "extr3"))
    dist_3p_fff <- array(0, dim=c(3,3,3), dimnames=c("extr1", "extr2", "extr3"))
    scales <- c(3)
    #dist_1p_b <- array(0, dim=c(2,3,3), dimnames=c("skill", "extr", "intr"))
    #dist_2p_ff <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
    #dist_2p_sf <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
    #dist_2p_ss <- array(0, dim=c(3,3,3,3), dimnames=c("extr1", "intr1", "extr2", "intr2"))
    for (e in 1:reps) {
      indices <- sample(1:nrow(dist_l), length(indices), replace=T)
      for (i in indices) {
        #print( dist_l[i,] )
        if (3 %in% scales) {
          if ((dist_l[i,"skil1"] == 0) & (dist_l[i,"skil2"] == 0) & (dist_l[i,"skil3"] == 1)) {
            dist_3p_ffs[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] <- dist_3p_ffs[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] + 1
            dist_3p_ffs[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] <- dist_3p_ffs[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 0) & (dist_l[i,"skil2"] == 1) & (dist_l[i,"skil3"] == 0)) {
            dist_3p_ffs[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] <- dist_3p_ffs[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] + 1
            dist_3p_ffs[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] <- dist_3p_ffs[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 1) & (dist_l[i,"skil2"] == 0) & (dist_l[i,"skil3"] == 0)) {
            dist_3p_ffs[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] <- dist_3p_ffs[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] + 1
            dist_3p_ffs[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] <- dist_3p_ffs[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 0) & (dist_l[i,"skil2"] == 1) & (dist_l[i,"skil3"] == 1)) {
            dist_3p_fss[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] <- dist_3p_fss[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] + 1
            dist_3p_fss[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] <- dist_3p_fss[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 1) & (dist_l[i,"skil2"] == 0) & (dist_l[i,"skil3"] == 1)) {
            dist_3p_fss[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] <- dist_3p_fss[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] + 1
            dist_3p_fss[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] <- dist_3p_fss[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 1) & (dist_l[i,"skil2"] == 1) & (dist_l[i,"skil3"] == 0)) {
            dist_3p_fss[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] <- dist_3p_fss[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] + 1
            dist_3p_fss[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] <- dist_3p_fss[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 1) & (dist_l[i,"skil2"] == 1) & (dist_l[i,"skil3"] == 1)) {
            dist_3p_sss[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] <- dist_3p_sss[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] + 1
            dist_3p_sss[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] <- dist_3p_sss[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] + 1
            dist_3p_sss[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] <- dist_3p_sss[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] + 1
            dist_3p_sss[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] <- dist_3p_sss[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] + 1
            dist_3p_sss[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] <- dist_3p_sss[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] + 1
            dist_3p_sss[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] <- dist_3p_sss[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] + 1
          } 
          else if ((dist_l[i,"skil1"] == 0) & (dist_l[i,"skil2"] == 0) & (dist_l[i,"skil3"] == 0)) {
            dist_3p_fff[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] <- dist_3p_fff[rbind(dist_l[i,c("extr3", "extr2", "extr1")]+1)] + 1
            dist_3p_fff[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] <- dist_3p_fff[rbind(dist_l[i,c("extr2", "extr3", "extr1")]+1)] + 1
            dist_3p_fff[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] <- dist_3p_fff[rbind(dist_l[i,c("extr3", "extr1", "extr2")]+1)] + 1
            dist_3p_fff[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] <- dist_3p_fff[rbind(dist_l[i,c("extr1", "extr3", "extr2")]+1)] + 1
            dist_3p_fff[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] <- dist_3p_fff[rbind(dist_l[i,c("extr1", "extr2", "extr3")]+1)] + 1
            dist_3p_fff[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] <- dist_3p_fff[rbind(dist_l[i,c("extr2", "extr1", "extr3")]+1)] + 1
          } 
        }
      }
      dist_3p_sss_d <- dist_3p_sss/sum(dist_3p_sss)
      dist_3p_fss_d <- dist_3p_fss/sum(dist_3p_fss)
      dist_3p_ffs_d <- dist_3p_ffs/sum(dist_3p_ffs) 
      dist_3p_fff_d <- dist_3p_fff/sum(dist_3p_fff) 
      #statistics <- array(0,c(1,8,2), dimnames=list(NULL,c("h1f", "h2p", "h3s", "hfps", "u2", "u3", "rdn", "syn","ti"),c("fss","ffs")))
      statistics[e,c("h1f","h2p"),1] <- c( entropy_adist(apply(dist_3p_sss_d,1,sum)), entropy_adist(apply(dist_3p_sss_d,2,sum))) 
      statistics[e,c("h1f","h2p"),2] <- c( entropy_adist(apply(dist_3p_fss_d,1,sum)), entropy_adist(apply(dist_3p_fss_d,2,sum))) 
      statistics[e,c("h1f","h2p"),3] <- c( entropy_adist(apply(dist_3p_ffs_d,1,sum)), entropy_adist(apply(dist_3p_ffs_d,2,sum))) 
      statistics[e,c("h1f","h2p"),4] <- c( entropy_adist(apply(dist_3p_fff_d,1,sum)), entropy_adist(apply(dist_3p_fff_d,2,sum))) 
      statistics[e,c("h3s","hfps"),1] <- c( entropy_adist(apply(dist_3p_sss_d,3,sum)), entropy_adist(dist_3p_sss_d)) 
      statistics[e,c("h3s","hfps"),2] <- c( entropy_adist(apply(dist_3p_fss_d,3,sum)), entropy_adist(dist_3p_fss_d)) 
      statistics[e,c("h3s","hfps"),3] <- c( entropy_adist(apply(dist_3p_ffs_d,3,sum)), entropy_adist(dist_3p_ffs_d)) 
      statistics[e,c("h3s","hfps"),4] <- c( entropy_adist(apply(dist_3p_fff_d,3,sum)), entropy_adist(dist_3p_fff_d)) 
      statistics[e,c("u2","u3"),1] <- c(uniqueinfo_3ddist(dist_3p_sss_d, 2), uniqueinfo_3ddist(dist_3p_sss_d, 3) )
      statistics[e,c("u2","u3"),2] <- c(uniqueinfo_3ddist(dist_3p_fss_d, 2), uniqueinfo_3ddist(dist_3p_fss_d, 3) )
      statistics[e,c("u2","u3"),3] <- c(uniqueinfo_3ddist(dist_3p_ffs_d, 2), uniqueinfo_3ddist(dist_3p_ffs_d, 3) )
      statistics[e,c("u2","u3"),4] <- c(uniqueinfo_3ddist(dist_3p_fff_d, 2), uniqueinfo_3ddist(dist_3p_fff_d, 3) )
      statistics[e,c("rdn", "syn"),1] <- c(redundancy_3ddist( dist_3p_sss_d ), synergy_3ddist( dist_3p_sss_d ))
      statistics[e,c("rdn", "syn"),2] <- c(redundancy_3ddist( dist_3p_fss_d ), synergy_3ddist( dist_3p_fss_d ))
      statistics[e,c("rdn", "syn"),3] <- c(redundancy_3ddist( dist_3p_ffs_d ), synergy_3ddist( dist_3p_ffs_d ))
      statistics[e,c("rdn", "syn"),4] <- c(redundancy_3ddist( dist_3p_fff_d ), synergy_3ddist( dist_3p_fff_d ))
      statistics[e,c("ti"),1] <- totalinfo_3ddist( dist_3p_sss_d  )
      statistics[e,c("ti"),2] <- totalinfo_3ddist( dist_3p_fss_d  )
      statistics[e,c("ti"),3] <- totalinfo_3ddist( dist_3p_ffs_d  )
      statistics[e,c("ti"),4] <- totalinfo_3ddist( dist_3p_fff_d  )
      print(statistics[e,,])
    }
    #Rprof()#bootstrap cli
    #summaryRprof(tmp)
    return(statistics)
}

### converts output of build_poker_distribution_3p  into long format for graphing on um ggplot
build_stat_long_3p <- function(statistics) {
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
    for (j in c("sss", "fss", "ffs", "fff")) {
      ti <- sum(stat_long[(stat_long$rep==i) & (stat_long$setting==j) & (stat_long$q %in% c("u2", "u3", "rdn", "syn")),"val"])
      stat_long <- rbind(stat_long, data.frame(rep=i, val=ti, q="ti2", setting=j))
      stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "u2"),"val"]/ti, q="u2n", setting=j))
      stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "u3"),"val"]/ti, q="u3n", setting=j))
      stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "rdn"),"val"]/ti, q="rdnn", setting=j))
      stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "syn"),"val"]/ti, q="synn", setting=j))
      stat_long <- rbind(stat_long, data.frame(rep=i, val=stat_long[(stat_long$rep ==i) & (stat_long$setting==j) & (stat_long$q == "ti"),"val"]/ti, q="tin", setting=j))
    }
  }
  stat_long$q <- factor(stat_long$q)
  #stat_long$setting <- with(stat_long, ifelse(setting=="ss", "Shark v Shark", ifelse(setting=="ff", "Fish v Fish", "Shark v Fish")))
  stat_long$setting <- factor(stat_long$setting)
  return(stat_long)
}
### end 2 player chugging fns






### infomation thoery functions

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
    mutinfo_2d[i] <- sum(x[i,])*specinfo_2ddist(x,i)
  }
  return(sum(mutinfo_2d))
}
multivarinformation_3ddist <- function (x) {
  ### I(X;Y;Z) = H(X) + H(Y) + H(Z) - H(X,Y) - H(X,Z) - H(Y,Z) + H(X,Y,Z)
  return(entropy_adist(apply(x,1,sum)) + entropy_adist( apply(x,2,sum) ) + entropy_adist( apply(x,3,sum) ) - entropy_adist(apply(x,c(1,2),sum) ) - entropy_adist(apply(x,c(1,3),sum) ) - entropy_adist(apply(x,c(2,3),sum) ) + entropy_adist(x))
}
totalcorrelation_3ddist <- function (x) {
  ### C(X;Y;Z) = H(X) + H(Y) + H(Z) - H(X,Y,Z)
  return(entropy_adist(apply(x,1,sum)) + entropy_adist( apply(x,2,sum) ) + entropy_adist( apply(x,3,sum) ) - entropy_adist(x))
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
    specinfo[j] <- x_i_d[j]*log2( x_i_d[j]/sum(x[,j]) )
  }
  return(sum(specinfo, na.rm=T))
}
specinfo_2ddist_alt <- function(x, i) {   
  x_i_d <- x[i,]/sum(x[i,])
  specinfo <- rep(0, length(x_i_d))
  for (j in 1:ncol(x)) {
    x_j_d <- x[,j]/sum(x[,j])
    specinfo[j] <- x_i_d[j]*(log2(1/sum(x[i,])) - log2(1/x_j_d[i]))
  }
  return(sum(specinfo, na.rm=T))
}
specinfo_2ddist_alt2 <- function(x, i) {
  answer <- c()
  for (j in 1:(dim(x)[2])) {
    answer <- c(answer, (x[i,j]/sum(x[i,]))*log2((x[i,j]/sum(x[i,]))/sum(x[,j])) )
  }
  return(sum(answer, na.rm=T))
}
### I(S=s; R_1, R_2) = \sum_{r_1} \sum_{r_2} p(r_1, r_2 | s) [\log \frac{1}{p(s)} - \log \frac{1}{p(s|r_1, r_2)}]\\
### I(S=s; R) = \sum_{r} p(r | s) \log \frac{p(r | s) }{p(r)} \\
### I(S=s; R_1, R_2) = \sum_{r_1} \sum_{r_2} p(r_1, r_2 | s) [\log \frac{1}{p(s)} - \log \frac{1}{p(s|r_1, r_2)}]\\
### I(S=s; R_1, R_2) = \sum_{r_1} \sum_{r_2} p(r_1, r_2 | s) \log \frac{p(r_1, r_2 | s) }{p(r_1,r_2)} \\
specinfo_3ddist <- function(x, i) {
  x_i_d <- x[i,,]/sum(x[i,,])
  specinfo <- rep(0, length(x_i_d))
  specinfo <- matrix(0, dim(x)[2], dim(x)[3] )
  for (j in 1:(dim(x)[2])) {
    for (k in 1:(dim(x)[3])) {
      x_jk_d <- x[,j,k]/sum(x[,j,k])
      specinfo[j,k] <- x_i_d[j,k]*(log2(1/sum(x[i,,])) - log2(1/x_jk_d[i]))
    }
  }
  return(sum(specinfo, na.rm=T))
}
specinfo_3ddist_alt <- function(x, i) {
  x_i_d <- x[i,,]/sum(x[i,,])
  specinfo <- matrix(0, dim(x)[2], dim(x)[3] )
  for (j in 1:(dim(x)[2])) {
    for (k in 1:(dim(x)[3])) {
      specinfo[j,k] <- x_i_d[j,k]*(log2(x_i_d[j,k]/sum(x[,j,k])) )
    }
  }
  return(sum(specinfo, na.rm=T))
}
specinfo_3ddist_alt2 <- function(x, i) {
  answer <- c()
  for (j in 1:(dim(x)[2])) {
    for (k in 1:(dim(x)[3])) {
      answer <- c(answer, (x[i,j,k]/sum(x[i,,]))*(log2(1/sum(x[i,,]))-log2(1/(x[i,j,k]/sum(x[,j,k])))) )
    }
  }
  return(sum(answer, na.rm=T))
}
specinfo_3ddist_alt3 <- function(x, i) {
  answer <- c()
  for (j in 1:(dim(x)[2])) {
    for (k in 1:(dim(x)[3])) {
      answer <- c(answer, (x[i,j,k]/sum(x[i,,]))*log2((x[i,j,k]/sum(x[i,,]))/sum(x[,j,k])) )
    }
  }
  return(sum(answer, na.rm=T))
}
specific_redundancy_3ddist <- function(x, i) {
  return(min(specinfo_2ddist(apply(x, c(1,2), sum), i), specinfo_2ddist(apply(x, c(1,3), sum), i) )) 
}
specific_synergy_3ddist <- function(x, i) {
  spec2 <- specinfo_2ddist(apply(x, c(1,2), sum), i) 
  spec3 <- specinfo_2ddist(apply(x, c(1,3), sum), i) 
  return(specinfo_3ddist(x, i) - spec2 - spec3 + min(spec2, spec3)) 
}
specific_uniqueinfo_3ddist <- function(x, i, unq) {
  spec2 <- specinfo_2ddist(apply(x, c(1,2), sum), i) 
  spec3 <- specinfo_2ddist(apply(x, c(1,3), sum), i) 
  if (unq == 2) { return(spec2 - min(spec2, spec3)) }
  if (unq == 3) { return(spec3 - min(spec2, spec3)) }
  return(NULL)
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
uniqueinfo_3ddist <- function(x, unq) {
  mutinfo_2ddist(apply(x,c(1,unq),sum)) - mininfo_adist(x, 1)
}
uniqueinfo_3ddist_alt <- function(x, unq) {
  rotate <- 1:3
  if (unq==3) {rotate <- c(1,3,2)}
  condmutinf_3ddist_alt(aperm(x, rotate)) - synergy_3ddist(x)
}
### returns redundancy that second two dimensions contain about focal first dimension
redundancy_3ddist <- function(x) { mininfo_adist(x, 1) }
### returns synergy that second two dimensions contain about focal first dimension
synergy_3ddist <- function(x) {
  return(totalinfo_3ddist(x) - uniqueinfo_3ddist(x, 2) - mutinfo_2ddist(apply(x,c(1,3),sum)))
}
synergy_3ddist_alt <- function(x) {
  return(totalinfo_3ddist(x) - uniqueinfo_3ddist(x, 2) - uniqueinfo_3ddist(x, 3)  - redundancy_3ddist(x))
}

### attempts to improve redundnacy. obviously not really started, nor done, but not dangerous, because they don't yet pretedn to do anything
renormalization_compression_info <- function(ff, renorm_function, focus_var=1 ) {
    return(ff)
}
test_pairwise_coarsenability <- function(ff, mapping_in, mapping_out) {
    dimf <- len(dim(ff))
    for (i in sample(2:dimf)) {
        for (j in sample(2:dimf)) {
            if (i == j) {next}
            apply(ff, c(1,i,j), sum)
        }
    }
    return(ff)
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
#ff[2,2,] <- 1
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


#load(file=input_file)
#dist_E1_E2_I1(distributions)
dist_E1_E2_I1 <- function(dist_list) {
  distsf <- matrix(0, nrow=length(dist_list), ncol=12)
  distff <- matrix(0, nrow=length(dist_list), ncol=12)
  for (i in 1:length(dist_list)) {
    distff[i,1:3] <- apply(dist_list[[i]]$ff, c(1), sum)/sum(apply(dist_list[[i]]$ff, c(1), sum))
    distff[i,4:6] <- apply(dist_list[[i]]$ff, c(3), sum)/sum(apply(dist_list[[i]]$ff, c(3), sum))
    distff[i,7:9] <- apply(dist_list[[i]]$ff, c(2), sum)/sum(apply(dist_list[[i]]$ff, c(2), sum))
    distff[i,10:12] <- apply(dist_list[[i]]$sf, c(1), sum)/sum(apply(dist_list[[i]]$sf, c(1), sum)) - apply(dist_list[[i]]$ff, c(1), sum)/sum(apply(dist_list[[i]]$ff, c(1), sum))
    distsf[i,1:3] <- apply(dist_list[[i]]$sf, c(1), sum)/sum(apply(dist_list[[i]]$sf, c(1), sum))
    distsf[i,4:6] <- apply(dist_list[[i]]$sf, c(3), sum)/sum(apply(dist_list[[i]]$sf, c(3), sum))
    distsf[i,7:9] <- apply(dist_list[[i]]$sf, c(2), sum)/sum(apply(dist_list[[i]]$sf, c(2), sum))
    distsf[i,10:12] <- apply(dist_list[[i]]$sf, c(1), sum)/sum(apply(dist_list[[i]]$sf, c(1), sum)) - apply(dist_list[[i]]$sf, c(3), sum)/sum(apply(dist_list[[i]]$sf, c(3), sum))
  }
  return(rbind(colMeans(distff), colMeans(distsf), apply(distff, 2, sd(nrow(distff))^0.5), apply(distsf, 2, sd(nrow(distsf))^0.5) ))
}
