#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(quanteda)

bigrams <- readRDS("data/bigrams.env.rds")
trigrams <- readRDS("data/trigrams.env.rds")


get_n_max_from_ngram <-function(n,ngram,ngrams.env){
    liste <- ngrams.env[[tolower(ngram)]]
    if (is.null(liste)){
        return(list(nb=0))
    }
    mots <- strsplit(liste,',')[[1]]
    return(list(mots=mots,nb=length(mots)))
}

profanity <- scan("data/naughty.txt", what="", sep="\n")

create_prediction_table <- function(n, phrase){
    res=data.frame(prediction=character(),rating=character())
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
            res <- rbind(res,data.frame(prediction=mot,rating="3 stars"))
            i=i+1
            if(i==n){
                return(res)
            }
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
            i=0

            for (mot in get_n_max_from_ngram(n,ngram,bigrams)$mots){
                res <- rbind(res,data.frame(prediction=mot,rating="2 stars"))
                i=i+1
                if(i==n){
                    return(res)
                }
            }


    }
    #if we arrive here, the results are not complete!!
    res <- rbind(res,data.frame(prediction="will",rating="1 star"))
    res

}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$predicted <- renderTable(
        create_prediction_table(input$nb_words,input$ngram)
  )
})
