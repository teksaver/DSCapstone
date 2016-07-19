library(data.table)

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

