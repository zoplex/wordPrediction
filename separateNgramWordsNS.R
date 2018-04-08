library(dplyr)
library(tidyr)

Sys.time()
if ( .Platform$OS.type=="unix") {
        setwd("/home/zoplex/R/capstoneProject/data/final/en_US")
} else {
        setwd("C:\\_Zdata\\MachineLearning\\DataScienceCapstoneProject\\data\\final\\en_US\\")
}

ld2             <- load(file="n2gramGT1NS.tm", verbose=T);        n2gram      <<- eval(as.symbol(ld2))         #  24 MB
ld3             <- load(file="n3gramGT1NS.tm", verbose=T);        n3gram      <<- eval(as.symbol(ld3))         #  20 MB
n2              <- n2gram %>% separate(col1, c("w1", "w2"), "_")
n3              <- n3gram %>% separate(col1, c("w1", "w2", "w3"), "_")
save(n2, file="n2gramGT1splitNS.tm")
save(n3, file="n3gramGT1splitNS.tm")

