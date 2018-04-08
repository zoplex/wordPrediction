library(dplyr)
library(data.table)
#
#       finalNgramPrep.R
#
if ( .Platform$OS.type=="unix") {
        setwd("/home/zoplex/R/capstoneProject/data/final/en_US")
} else {
        setwd("C:\\_Zdata\\MachineLearning\\DataScienceCapstoneProject\\data\\final\\en_US\\")
}

Sys.time()
ld1             <- load(file="n1gramGT1.tm", verbose=T);        n1gram      <- eval(as.symbol(ld1))         #   1.4 MB
ld2             <- load(file="n2gramGT1.tm", verbose=T);        n2gram      <- eval(as.symbol(ld2))         #  24 MB
ld3             <- load(file="n3gramGT1.tm", verbose=T);        n3gram      <- eval(as.symbol(ld3))         #  20 MB
Sys.time()
nrow(n1gram); nrow(n2gram); nrow(n3gram)
setkey(n1gram, col1)
setkey(n2gram, col1)
setkey(n3gram, col1)














