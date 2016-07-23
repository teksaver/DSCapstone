library(data.table)
library(dplyr)
library(tidyr)

trigrams <- fread("demi_trigrams.csv")

create_env_ngrams <- function(df){
    occur <- new.env()
    for(i in 1:nrow(df)) {
        occur[[df[i]$ngram]]=df[i]$predict
    }
    occur
}


trigrams.df <- trigrams %>%
    filter(Frequency>3) %>%
    filter(grepl("^[[:alnum:]]*_[[:alnum:]]*_[[:alnum:]]*$",Content)) %>%
    arrange(-Frequency) %>%
    separate(Content,into=c("w1","w2","predict"),sep="_") %>%
    unite(ngram,w1,w2)%>%
    group_by(ngram) %>%
    top_n(n = 5, wt = Frequency) %>%
    summarise( predict = toString(unique(predict)))

trigrams.env <- create_env_ngrams(trigrams.df)

saveRDS(trigrams.env, "ShinyApp/predictNext/data/trigrams.env.rds")


bigrams <- fread('ShinyApp/predictNext/bigrams_full.csv', header = T, sep = ',')
bigrams.df <- bigrams %>%
    filter(grepl("^[[:alnum:]]*_[[:alnum:]]*$",Content)) %>%
    separate(Content,into=c("ngram","predict"),sep="_") %>%
    arrange(-Frequency) %>%
    group_by(ngram) %>%
    top_n(n = 5, wt = Frequency) %>%
    summarise( predict = toString(unique(predict)))



bigrams.env <- create_env_ngrams(bigrams.df)

get_n_max_from_ngram <-function(n,ngram,ngrams.env){
    ngrams.env[[ngram]]
}

want <- get_n_max_from_ngram(5,"please",bigrams.env)

bigrams.env <- readRDS("bigrams.env.RDS")



get_n_max_from_ngram(3,"hqsdello",bigrams.env)

trigrams.df <- fread('trigrams.df.csv', header = T, sep = ',')
trigrams.env <- create_env_ngrams(trigrams.df)
trigrams.multiple.df <- trigrams.df %>% filter(grepl(",",predict))


# prediction function

bigrams <- readRDS("ShinyApp/predictNext/data/bigrams.env.rds")
trigrams <- readRDS("ShinyApp/predictNext/data/trigrams.env.rds")

get_n_max_from_ngram <-function(n,ngram,ngrams.env){
    liste <- ngrams.env[[tolower(ngram)]]
    if (is.null(liste)){
        return(list(nb=0))
    }
    mots <- strsplit(liste,',')[[1]]
    return(list(mots=mots,nb=length(mots)))
}
library(quanteda)
profanity <- scan("naughty.txt", what="", sep="\n")

create_prediction_table <- function(n, phrase){
    res=data.frame(prediction=character(),rating=integer())
    # get prediction for 2gram
    phrase.dfm <- dfm(phrase,
                  ngrams = 2,
                  stem = FALSE,
                  ignoredFeatures = c(profanity),
                  removePunct = TRUE,
                  removeNumbers = TRUE,
                  removeTwitter = TRUE,
                  removeSeparators = TRUE,
                  removeHyphens = TRUE)

    if (!is.null(phrase.dfm)){# we have a 2gram
        tokens <- data.frame(Content = features(phrase.dfm), row.names = NULL, stringsAsFactors = FALSE)
        ngram <- tokens[nrow(tokens),]
        i=0

        for (mot in get_n_max_from_ngram(n,ngram,trigrams)$mots){
            print (mot)
            res <- rbind(res,data.frame(prediction=mot,rating=3))
            i=i+1
            if(i==n){
                return(res)
            }
        }

        # if not enough words, deal with last word
        word.dfm <- dfm(phrase,
                          ngrams = 1,
                          stem = FALSE,
                          ignoredFeatures = c(profanity),
                          removePunct = TRUE,
                          removeNumbers = TRUE,
                          removeTwitter = TRUE,
                          removeSeparators = TRUE,
                          removeHyphens = TRUE)

        if (!is.null(word.dfm)){# we have a word list
            tokens <- data.frame(Content = features(word.dfm), row.names = NULL, stringsAsFactors = FALSE)
            ngram <- tokens[nrow(tokens),]
            print(paste("décomposition : ",ngram))
            i=0

            for (mot in get_n_max_from_ngram(n,ngram,bigrams)$mots){
                print (mot)
                res <- rbind(res,data.frame(prediction=mot,rating=2))
                i=i+1
                if(i==n){
                    return(res)
                }
            }
        }

    }
    #if we arrive here, the results are not complete!!
    res <- rbind(res,data.frame(prediction="will",rating=1))
    res

}

create_prediction_table(5,"i want to eat")



