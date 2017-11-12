#!/usr/bin/env Rscript
packageLoad<-function(libName){
      # try to load the package
      if (!require(libName,character.only = TRUE)){ 
              # if package is not available, install it
              install.packages(libName,dep=TRUE, repos="http://cran.r-project.org")
      # try again
      if(!require(libName,character.only = TRUE)) 
                stop(paste("Package ",libName," not found and its installation failed."))
                                            }
}
packagesLoad<-function(libs){
	for (l in libs){
		packageLoad(l)
	}
}
packagesLoad(c("abind", "ggplot2", "ggthemes", "optparse", "plyr", "infotheo", "testit", "stringi", "data.table", "bit64", "abind", "gridExtra", "sqldf"))
