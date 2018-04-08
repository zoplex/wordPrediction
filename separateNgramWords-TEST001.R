library(dplyr)
library(tidyr)

setwd('/home/zoplex/R/capstoneProject/finalCode/')
ld2             <- load(file="n2gramGT1.tm", verbose=T);        n2gram      <<- eval(as.symbol(ld2))         #  24 MB
ld3             <- load(file="n3gramGT1.tm", verbose=T);        n3gram      <<- eval(as.symbol(ld3))         #  20 MB
n2              <- n2gram %>% separate(col1, c("w1", "w2"), "_")
n3              <- n3gram %>% separate(col1, c("w1", "w2", "w3"), "_")
# save(n2, file="n2gramGT1split.tm")
# save(n3, file="n3gramGT1split.tm")
browser()
setkeyv( n2, c("w1"))
setkeyv( n3, c("w1","w2"))

textInClean   <- "word1 aspire leader program continu"
allwords        <- unlist((strsplit(gsub("\\s+"," ",trimws(textInClean)), " ", fixed=T))[[1]])
last2w          <- allwords[max(1,(length(allwords)-1)):length(allwords)]
df3res          <- n3[w1==last2w[1] & w2==last2w[2]]
s3              <- df3res[ w3 ]
df2res          <- n2[w1==last2w[2]]
s2              <- df2res[ w2 ]
df3res
df2res