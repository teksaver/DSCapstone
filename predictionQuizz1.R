library(quanteda)
set.seed(1979)

# Full File loading


#blogsCorpusFile <- textfile("./final/en_US/en_US.blogs.txt")
#fullBlogsCorpus <- corpus(blogsCorpusFile)

# File loading with sampling

blogsConn <- file("./final/en_US/en_US.blogs.txt","r")
newsConn <- file("./final/en_US/en_US.news.txt","r")
#twittConn <- file("./final/en_US/en_US.twitter.txt","r")
blogsLines <- readLines(blogsConn,n=-1,skipNul = TRUE)
newsLines <- readLines(newsConn,n=-1,skipNul = TRUE)
#twittLines <- readLines(twittConn,n=-1,skipNul = TRUE)
close(blogsConn)
close(newsConn)
#close(twittConn)
blogsSubset <- blogsLines[sample(1:length(blogsLines), length(blogsLines)/500, replace=FALSE)]
newsSubset <- blogsLines[sample(1:length(newsLines), length(newsLines)/500, replace=FALSE)]

#twittSubset <- blogsLines[sample(1:length(twittLines), length(twittLines)/500, replace=FALSE)]

#sample corpus loading
myCorpus <- corpus(blogsSubset[!is.na(blogsSubset)]) +
    corpus(newsSubset[!is.na(newsSubset)])
metadoc(myCorpus, "language") <- "english"
summary(myCorpus, n = 5)

#prepare profanity
profanity <- scan("naughty.txt", what="", sep="\n")

#create unigrams
unigrams.dfm <- dfm(myCorpus, ngrams = 1, ignoredFeatures = c(profanity),
                    removePunct = TRUE, removeNumbers = TRUE,
                    removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)
topfeatures(unigrams.dfm)

#create bigrams
bigrams.dfm <- dfm(myCorpus, ngrams = 2, ignoredFeatures = c(profanity),
                   removePunct = TRUE,
                   removeNumbers = TRUE,
                   removeTwitter = TRUE,
                   removeSeparators = TRUE,
                   removeHyphens = TRUE,
                   stem = TRUE)
topfeatures(bigrams.dfm)

#create trigrams
trigrams.dfm <- dfm(myCorpus, ngrams = 3, ignoredFeatures = c(profanity),
                   removePunct = TRUE, removeNumbers = TRUE,
                   removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)
topfeatures(trigrams.dfm)

#tests tidyr

#library(dplyr)
#library(tidyr)
#long.df <- as.data.frame(trigrams.dfm) %>%
#    gather("trigram","occurences") %>%
#    filter(occurences!=0)
#long.df <- data.frame(known = rep(long.df$trigram, long.df$occurences))
#long.df$y <- gsub("^.*\\_","", long.df$known )
#long.df$known <- gsub("\\_[^_]*$","",long.df$known)


testQuizz1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

testQuizz2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"

testCorpus <- corpus(testQuizz1)
testCorpusBigrams.dfm <- dfm(testCorpus, ngrams = 2, ignoredFeatures = c(profanity),
                   removePunct = TRUE, removeNumbers = TRUE,
                   removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)

topfeatures(testCorpusBigrams.dfm)
as.data.frame(testCorpusBigrams.dfm)

tofind <- which(grepl("case_of_",colnames(trigrams.dfm)))
candidatesVector <-apply(trigrams.dfm[,tofind],2,sum)
candidatesDf <- data.frame(pred=names(candidatesVector),n=candidatesVector)
candidatesDf$pred <- gsub("^.*\\_","", candidatesDf$pred )
#candidatesDf$x <- gsub("\\_[^_]*$","",candidatesDf$pred)
candidatesDf[order(-candidatesDf$n),]$pred[1]
