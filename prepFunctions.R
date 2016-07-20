library(data.table)

trigrams <- fread("demi_trigrams.csv")
tritest <- head(trigrams,n=1000)

vals = strsplit(tritest$Content,split="_")


trigrams.df <- trigrams %>%
    filter(grepl("^[[:alnum:]]*_[[:alnum:]]*_[[:alnum:]]*$",Content)) %>%
    separate(Content,into=c("w1","w2","predict"),sep="_") %>%
    unite(ngram,w1,w2)%>%
    arrange(-Frequency) %>%
    group_by(ngram) %>%
    top_n(n = 5, wt = Frequency) %>%
    summarise( predict = toString(unique(predict)))


grep("\\_[^_]*$", tritest[1]$Content,value=TRUE )


candidatesDf$pred <- gsub("^.*\\_","", candidatesDf$pred )


bigrams <- fread('ShinyApp/predictNext/bigrams_full.csv', header = T, sep = ',')
bigrams.df <- bigrams %>%
    filter(grepl("^[[:alnum:]]*_[[:alnum:]]*$",Content)) %>%
    separate(Content,into=c("ngram","predict"),sep="_") %>%
    arrange(-Frequency) %>%
    group_by(ngram) %>%
    top_n(n = 5, wt = Frequency) %>%
    summarise( predict = toString(unique(predict)))

create_env_ngrams <- function(df){
    occur <- new.env()
    for(i in 1:nrow(df)) {
        occur[[df[i]$ngram]]=df[i]$predict
    }
    occur
}

bigrams.env <- create_env_ngrams(bigrams.df)

get_n_max_from_ngram <-function(n,ngram,ngrams.env){
    ngrams.env[[ngram]]
}

want <- get_n_max_from_ngram(5,"please",bigrams.env)

bigrams.env <- readRDS("bigrams.env.RDS")

get_n_max_from_ngram <-function(n,ngram,ngrams.env){
    liste <- ngrams.env[[tolower(ngram)]]
    if (is.null(liste)){
        return(list(nb=0))
    }
    mots <- strsplit(liste,',')[[1]]
    return(list(mots=mots,nb=length(mots)))
}

get_n_max_from_ngram(3,"hqsdello",bigrams.env)

