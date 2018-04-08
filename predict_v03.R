library(tm)
library(RWeka)
library(wordcloud)
library(qdap)
library(ggplot2)
library(reshape)
library(scales)
library(dplyr)
library(tidytext)
library(tidyr)






cleanInput      <- function( instr ) {
        
        fclean                  <- function(x) {gsub("[^\x20-\x7E]", "", x)}
        cleantext               <- unlist(lapply(instr, FUN=fclean))
        textCorpus      <- Corpus(VectorSource(cleantext)) 
        textCorpus      <- tm_map(textCorpus, content_transformer(tolower))
        textCorpus      <- tm_map(textCorpus, content_transformer(removePunctuation))
        textCorpus      <- tm_map(textCorpus, content_transformer(removeNumbers))
        textCorpus      <- tm_map(textCorpus, removeWords, stopwords("english"))
        textCorpus      <- tm_map(textCorpus, removeWords, profws )
        alnewtext       <- unlist(lapply(textCorpus, function(x) x ))
        return ( alnewtext )
}
dfsearch        <- function ( zword ) {
        # res1    <- dftopN1[ grepl(zword, dftopN1$n1grams, ignore.case=TRUE ),]
        res2    <- dftopN2[ grepl(zword, dftopN2$n2grams, ignore.case=TRUE ),]
        res3    <- dftopN3[ grepl(zword, dftopN3$n3grams, ignore.case=TRUE ),]
        res4    <- dftopN4[ grepl(zword, dftopN4$n4grams, ignore.case=TRUE ),]
        res5    <- dftopN5[ grepl(zword, dftopN5$n5grams, ignore.case=TRUE ),]   
        return ( bind_rows( res2, res3, res4, res5 ) )
}

Sys.time()
if ( .Platform$OS.type=="unix") {
          setwd("/home/zoplex/R/capstoneProject/data/final/en_US")
} else {
          setwd("C:\\_Zdata\\MachineLearning\\DataScienceCapstoneProject\\data\\final\\en_US\\")
}
profws          <- readLines("https://www.cs.cmu.edu/~biglou/resources/bad-words.txt")

ld4             <- load("tb4final50pct_predict.tm", verbose=T)
df4final        <- eval(as.symbol(ld4))
ld3             <- load("tb3final50pct_predict.tm", verbose=T)
df3final        <- eval(as.symbol(ld3))
ld2             <- load("tb2final50pct_predict.tm", verbose=T)
df2final        <- eval(as.symbol(ld2))

ld5h1           <- load("tb5final50pct_predict_h1.tm", verbose=T)
df5final_h1     <- eval(as.symbol(ld5h1))
ld5h2           <- load("tb5final50pct_predict_h2.tm", verbose=T)
df5final_h2     <- eval(as.symbol(ld5h2))

Sys.time()
browser()

instr           <- "pound of bacon, a bouquet, and a case of"
results         <- c("pretzels", "soda", "beer", "cheese")
cleanstr        <- cleanInput( instr ); cleanstr <- gsub(' +', ' ', cleanstr)
allwords        <- unlist((strsplit(cleanstr, " ", fixed=T))[[1]])

if ( length( allwords) >= 4 ) {
        last4w          <- allwords[(length(allwords)-3):length(allwords)]
        
        df5res_h1       <- df5final_h1[ w1==last4w[1] & w2==last4w[2] & w3==last4w[3] & w4==last4w[4],]
        s5_h1           <- df5res_h1[ w5 %in% results, ]
        
        df5res_h2       <- df5final_h2[ w1==last4w[1] & w2==last4w[2] & w3==last4w[3] & w4==last4w[4],]
        s5_h2           <- df5res_h2[ w5 %in% results, ]
        
        df4res          <- df4final[ w1==last4w[2] & w2==last4w[3] & w3==last4w[4],]
        s4              <- df4res[ w4 %in% results, ]
        df3res          <- df3final[ w1==last4w[3] & w2==last4w[4],]
        s3              <- df3res[ w3 %in% results, ]
        df2res          <- df2final[ w1==last4w[4],]
        s2              <- df2res[ w2 %in% results, ]
        s5_h1
        s5_h2
        s4
        s3
        s2
}




