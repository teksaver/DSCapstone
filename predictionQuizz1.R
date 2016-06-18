library(quanteda)

# Full File loading

#blogsCorpusFile <- textfile("./final/en_US/en_US.blogs.txt")
#fullBlogsCorpus <- corpus(blogsCorpusFile)

# File loading with sampling

blogsConn <- file("./final/en_US/en_US.blogs.txt","r")
#newsConn <- file("./final/en_US/en_US.news.txt","r")
#twittConn <- file("./final/en_US/en_US.twitter.txt","r")
blogsLines <- readLines(blogsConn,n=-1,skipNul = TRUE)
#newsLines <- readLines(newsConn,n=-1,skipNul = TRUE)
#twittLines <- readLines(twittConn,n=-1,skipNul = TRUE)
close(blogsConn)
#close(newsConn)
#close(twittConn)
blogsSubset <- blogsLines[sample(1:length(blogsLines), length(blogsLines)/500, replace=FALSE)]
#newsSubset <- blogsLines[sample(1:length(newsLines), length(newsLines)/500, replace=FALSE)]
#twittSubset <- blogsLines[sample(1:length(twittLines), length(twittLines)/500, replace=FALSE)]

#sample corpus loading
myCorpus <- corpus(blogsSubset)
metadoc(myCorpus, "language") <- "english"
summary(myCorpus, n = 5)

#prepare profanity
profanity <- scan("naughty.txt", what="", sep="\n")

#create unigrams
unigrams.dfm <- dfm(myCorpus, ngrams = 1, ignoredFeatures = c(profanity, stopwords("english")),
                    removePunct = TRUE, removeNumbers = TRUE,
                    removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)
topfeatures(unigrams.dfm)

#create bigrams
bigrams.dfm <- dfm(myCorpus, ngrams = 2, ignoredFeatures = c(profanity, stopwords("english")),
                    removePunct = TRUE, removeNumbers = TRUE,
                    removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)
topfeatures(bigrams.dfm)

#create trigrams
trigrams.dfm <- dfm(myCorpus, ngrams = 1:3, ignoredFeatures = c(profanity, stopwords("english")),
                   removePunct = TRUE, removeNumbers = TRUE,
                   removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, stem = TRUE)
topfeatures(trigrams.dfm)
