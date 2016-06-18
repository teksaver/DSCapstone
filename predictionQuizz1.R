library(quanteda)
set.seed(1979)

# Full File loading


#blogsCorpusFile <- textfile("./final/en_US/en_US.blogs.txt")
#fullBlogsCorpus <- corpus(blogsCorpusFile)

# File loading with sampling

blogsConn <- file("./final/en_US/en_US.blogs.txt","r")
newsConn <- file("./final/en_US/en_US.news.txt","r")
twittConn <- file("./final/en_US/en_US.twitter.txt","r")
blogsLines <- readLines(blogsConn,n=-1,skipNul = TRUE)
newsLines <- readLines(newsConn,n=-1,skipNul = TRUE)
twittLines <- readLines(twittConn,n=-1,skipNul = TRUE)
close(blogsConn)
close(newsConn)
close(twittConn)
blogsSubset <- blogsLines[sample(1:length(blogsLines), length(blogsLines)/5, replace=FALSE)]
newsSubset <- blogsLines[sample(1:length(newsLines), length(newsLines)/5, replace=FALSE)]
twittSubset <- blogsLines[sample(1:length(twittLines), length(twittLines)/5, replace=FALSE)]

rm(blogsLines)
rm(newsLines)
rm(twittLines)
#sample corpus loading
myCorpus <- corpus(blogsSubset[!is.na(blogsSubset)]) +
    corpus(newsSubset[!is.na(newsSubset)]) +
    corpus(twittSubset[!is.na(twittSubset)])

metadoc(myCorpus, "language") <- "english"
summary(myCorpus, n = 5)

#prepare profanity
profanity <- scan("naughty.txt", what="", sep="\n")
#
# #create unigrams
# unigrams.dfm <- dfm(myCorpus, ngrams = 1, ignoredFeatures = c(profanity),
#                     removePunct = TRUE, removeNumbers = TRUE,
#                     removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)
# topfeatures(unigrams.dfm)
#
# #create bigrams
# bigrams.dfm <- dfm(myCorpus, ngrams = 2, ignoredFeatures = c(profanity),
#                    removePunct = TRUE,
#                    removeNumbers = TRUE,
#                    removeTwitter = TRUE,
#                    removeSeparators = TRUE,
#                    removeHyphens = TRUE,
#                    stem = FALSE)
# topfeatures(bigrams.dfm)

#create trigrams
trigrams.dfm <- dfm(myCorpus,
                ngrams = 3,
                stem = FALSE,
                ignoredFeatures = c(profanity),
                removePunct = TRUE,
                removeNumbers = TRUE,
                removeTwitter = TRUE,
                removeSeparators = TRUE,
                removeHyphens = TRUE)
topfeatures(trigrams.dfm)

#create quadrigrams
quadrigrams.dfm <- dfm(myCorpus,
                    ngrams = 4,
                    stem = FALSE,
                    ignoredFeatures = c(profanity),
                    removePunct = TRUE,
                    removeNumbers = TRUE,
                    removeTwitter = TRUE,
                    removeSeparators = TRUE,
                    removeHyphens = TRUE)
topfeatures(quadrigrams.dfm)



testQuizz1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

testQuizz2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"


#1 tofind <- which(grepl("^a_case_of_",colnames(quadrigrams.dfm))) #beer 6 OK
#2 tofind <- which(grepl("^make_me_the_",colnames(quadrigrams.dfm))) #best 0 KO
#3 tofind <- which(grepl("^make_me_the_",colnames(quadrigrams.dfm))) #happiest 3 OK
#4 tofind <- which(grepl("^struggling_but_the_",colnames(quadrigrams.dfm))) #players 0 KO
#5 tofind <- which(grepl("^date_at_the_",colnames(quadrigrams.dfm))) #movies 0 KO
#6 tofind <- which(grepl("^be_on_my_",colnames(quadrigrams.dfm))) #way 7 OK
#7 tofind <- which(grepl("^in_quite_some_",colnames(quadrigrams.dfm))) #time 18 OK
#8 tofind <- which(grepl("^with_his_little_",colnames(quadrigrams.dfm))) #finger 2 OK
#9 tofind <- which(grepl("^faith_during_the_",colnames(quadrigrams.dfm))) # bad 0 OK
#10 tofind <- which(grepl("^you_must_be_",colnames(quadrigrams.dfm))) #asleep 0 KO

candidatesVector <-apply(quadrigrams.dfm[,tofind],2,sum)
candidatesDf <- data.frame(pred=names(candidatesVector),n=candidatesVector)
candidatesDf$pred <- gsub("^.*\\_","", candidatesDf$pred )
#candidatesDf$x <- gsub("\\_[^_]*$","",candidatesDf$pred)
head(candidatesDf[order(-candidatesDf$n),],10)
