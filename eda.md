# Data Science Specialization report 1: Exploratory Data Analysis
Sylvain Tenier  
June 12, 2016  



# Executive summary

We analyse 3 files containing text in English from blogs, news and Twitter. We first summarize the main characteristics of each file. We then use "vanilla" R tools to detect numbers, profanity and tokenize each line of the corpus. Using the tokenized version, we show that the population respects zipf's law distribution for 1 and 2-grams. We also demonstrate that relatively few words are necessary in 1-grams to account for the majority of occurences in a corpus, but that the proportion increases according to n in n-grams. Finally, we present our plan and tools for a word prediction appication from this corpus.

# Main characteristics

## Number of lines for each data set

We first check whether the dataset can fit in memory. Using the `readLines` function from a `connection` removes any *null characters* warnings and loads all the data without trouble.


```r
blogsConn <- file("./final/en_US/en_US.blogs.txt","r")
newsConn <- file("./final/en_US/en_US.news.txt","r")
twittConn <- file("./final/en_US/en_US.twitter.txt","r")
blogsLines <- readLines(blogsConn,n=-1,skipNul = TRUE)
newsLines <- readLines(newsConn,n=-1,skipNul = TRUE)
twittLines <- readLines(twittConn,n=-1,skipNul = TRUE)
close(blogsConn)
close(newsConn)
close(twittConn)
```

During the analysis, tests were performed using a subset of the data using the `sample` function such as

```r
blogsLines[sample(1:length(blogsLines), length(blogsLines)/10, replace=FALSE)]
```
However, the results presented here are generated from the full data set. This is reasonnable since the computations can be carried out on a nine-years old laptop (intel i3, 4GB RAM).


```r
library(knitr)
summBlog <- as.character(summary(blogsLines))
summNews <- as.character(summary(newsLines))
summTwitt <- as.character(summary(twittLines))
summDf <- data.frame(source="Blog",lines=summBlog[1],class=summBlog[2])
summDf <-rbind(summDf,data.frame(source="News",lines=summNews[1],class=summNews[2]))
summDf <-rbind(summDf,data.frame(source="Twitter",lines=summTwitt[1],class=summTwitt[2]))
kable(summDf)
```



source    lines     class     
--------  --------  ----------
Blog      899288    character 
News      1010242   character 
Twitter   2360148   character 

```r
rm(summDf)
```

## Distribution of number of characters per line

We plot a histogram for the distribution of number of characters per line for each kind of data. We can see that blog and news data follow a normal distribution, while Twitter's 160 chararacters-per-message limit causes a strong bias towards that limit.


```r
charCountDF <- data.frame(nbChar=integer(),source=factor(levels=c("Blog","News","Twitter")))
charCountDF <- rbind(charCountDF,data.frame(nbChar=sapply(blogsLines,nchar),source="Blog"))
charCountDF <- rbind(charCountDF,data.frame(nbChar=sapply(newsLines,nchar),source="News"))
charCountDF <- rbind(charCountDF,data.frame(nbChar=sapply(twittLines,nchar),source="Twitter"))
```


```r
suppressPackageStartupMessages(library(ggplot2))
ggplot(data=charCountDF,aes(nbChar))+
    facet_grid(. ~ source)+
    geom_histogram(bins=30)+
    scale_x_log10() +
    labs(x="Number of characters",title="Number of characters per line")
```

![](eda_files/figure-html/char distribution histogram-1.png)<!-- -->

```r
rm(charCountDF)
```

# Content analysis

We first need to construct a function to generate a regular expression for profanity detection. We use the textfile provided at https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en.


```r
prof <- scan("naughty.txt", what="", sep="\n")
profExpr <- " ("
for (item in prof){
    profExpr <- paste0(profExpr,item, sep = "|", collapse = NULL)
}
profExpr <- paste0(profExpr,")(s|ing|ed)* ",sep="",collapse = NULL)
```

We now create a `tokenize` function that :

- removes all punctuation,
- put all words in lowercase,
- creates a generic `.number.` token for each number,
- substitutes bad words into a `.profanity.` token,
- removes extra whitespace,
- splits all words and generic tokens by whitespace.


```r
tokenize <- function(line){
    #substitute all punctuation with space
    line <- gsub("[[:punct:]]"," ",line)
    #set all characters in lower case
    line <- tolower(line)
    #generalize numbers with <number> token
    line <- gsub(" [[:digit:]]+ "," .number. ",line)
    #replace profanity with .profanity.
    line <- gsub(profExpr," .profanity. ",line)
    #remove leading and trailing space (R > 3.2)
    line <- trimws(line, which="both")
    #split by space
    tokens <- strsplit(line, split="\\s+")[[1]]
    return(tokens)
}
```

We then create a function to calculate the occurences for each word. The `ngramsOcc function uses a R environment to benefit from an efficient key-value (hash table) data structure.


```r
library("data.table")
ngramsOcc <- function(lines){
    # an environment is the most efficient hash table implementation in R
    occur <- new.env()
    occurTwo <- new.env() #for 2-grams
    nbWords <- 0 #word count
    for (line in lines){
        #get the list of words in a line
        words <- tokenize(line)
        for(i in 1:length(words)){
            if(grepl(" ",words[i])==TRUE){
                print(line)
            }
            nbWords <- nbWords +1
            #if this is the first occurence of the word, add it to the environment
            if(is.null(occur[[words[i]]])){
                occur[[words[i]]] <- 1
                #else increment the number of times the word is seen
            }else{
                occur[[words[i]]] <- occur[[words[i]]] +1
            }
            # same procedure for 2grams
            if(i>1){#do nothing for the first token
                twoGram <- paste(words[i-1],words[i],sep="_")
                if(is.null(occurTwo[[twoGram]])){
                    occurTwo[[twoGram]] <- 1
                    #else increment the number of times the word is seen
                }else{
                    occurTwo[[twoGram]] <- occurTwo[[twoGram]] +1
                }
            }
            
        }
    }
    #create dataframes and return list with 1gram, 2gram and word count
    wordOccDf <- as.data.frame(unlist(as.list(occur)))
    twoGramDf <- as.data.frame(unlist(as.list(occurTwo)))
    setDT(wordOccDf,keep.rownames = TRUE)
    setDT(twoGramDf,keep.rownames = TRUE)
    colnames(wordOccDf)<-c("word","occ")
    colnames(twoGramDf)<-c("2gram","occ")
    wordOccDf <- wordOccDf[order(-occ),]
    twoGramDf <- twoGramDf[order(-occ),]
    return(list(oneGram=wordOccDf,twoGram=twoGramDf, nbWords=nbWords))
}
```

Using the function, we launch the calculation for each dataset. Since full calculations would take around 12 hours, we use a randomly drawn 10\% subset using the sample function, as seen in the inference class.


```r
set.seed(12345)
blogsSubset <- blogsLines[sample(1:length(blogsLines), length(blogsLines)/10, replace=FALSE)]
newsSubset <- blogsLines[sample(1:length(newsLines), length(newsLines)/10, replace=FALSE)]
twittSubset <- blogsLines[sample(1:length(twittLines), length(twittLines)/10, replace=FALSE)]
blogsNGrams <- ngramsOcc(blogsSubset)
newsNGrams <- ngramsOcc(newsSubset)
twittNGrams <- ngramsOcc(twittSubset)
rm(blogsLines)
rm(newsLines)
rm(twittLines)
```

## 1-gram (words) analysis

The 1-gram occurence calculation provides the following results:

- In the *Blogs* dataset, there are 4245568 occurences for 87301 distinct words.
- In the *News* dataset, there are 4271207 occurences for 87446 distinct words.
- In the *Twitter* dataset, there are 4427429 occurences for 87663 distinct words.

We first generate and display and histogram of 1-grams (words) occurences. The following histogram shows that the resulting distribution is consistent with zipf's law (http://nlp.stanford.edu/IR-book/html/htmledition/zipfs-law-modeling-the-distribution-of-terms-1.html) for all datasets, i.e. the distribution of words follows a power law.


```r
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

total1gramOcc <- cbind(blogsNGrams$oneGram,source="Blogs") %>%
rbind(cbind(newsNGrams$oneGram,source="News")) %>%
rbind(cbind(twittNGrams$oneGram,source="Twitter"))

ggplot(total1gramOcc, aes(occ)) +
    geom_histogram(bins = 30, pad=TRUE) +
    scale_y_log10()+
    scale_x_log10()+
    facet_grid(. ~ source)+
    labs(x="Number of occurences",title="Distribution of word occurences")
```

![](eda_files/figure-html/1-gram histogram-1.png)<!-- -->

This can be further demonstrated by the following tables, that show that the top 10 popular words account for around 20\% of the total occurences. 


```r
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
top10 <- cbind(head(blogsNGrams$oneGram,n=10),source = "Blogs",total=blogsNGrams$nbWords)
top10 <- rbind(top10,cbind(head(newsNGrams$oneGram,n=10),source= "News",total=newsNGrams$nbWords))
top10 <- rbind(top10,cbind(head(twittNGrams$oneGram,n=10),source= "Twitter",total=twittNGrams$nbWords))

#Calculate proportion of top10 words per data source
sum10 <- top10 %>%
    group_by(source,total) %>%
    summarise(word="Sum top 10",occ=sum(occ)) %>%
    select(word,occ,source,total) %>%
    mutate(proportion=paste("(",round(occ*100/total,2),"%)")) %>%
    unite("occurences",occ,proportion,sep=" ") %>%
    select(occurences,source) %>%
    spread(source,occurences)

#Display top10 occurences and the sum
top10 <- top10 %>%
    mutate(proportion=paste("(",round(occ*100/total,2),"%)")) %>%
    unite("occurences",occ,proportion,sep=" ") %>%
    select(-total) %>%
    spread(source,occurences)
    
kable(top10,caption="Top10 words per source")
```



Table: Top10 words per source

word   Blogs              News               Twitter          
-----  -----------------  -----------------  -----------------
a      90059 ( 2.12 %)    90230 ( 2.11 %)    90239 ( 2.04 %)  
and    109417 ( 2.58 %)   109615 ( 2.57 %)   109947 ( 2.48 %) 
i      90792 ( 2.14 %)    90875 ( 2.13 %)    91603 ( 2.07 %)  
in     59613 ( 1.4 %)     59698 ( 1.4 %)     60214 ( 1.36 %)  
is     42777 ( 1.01 %)    43201 ( 1.01 %)    NA               
it     48042 ( 1.13 %)    48573 ( 1.14 %)    48733 ( 1.1 %)   
NA     NA                 NA                 146199 ( 3.3 %)  
of     87309 ( 2.06 %)    87895 ( 2.06 %)    88297 ( 1.99 %)  
that   47891 ( 1.13 %)    48596 ( 1.14 %)    48509 ( 1.1 %)   
the    185354 ( 4.37 %)   186419 ( 4.36 %)   187518 ( 4.24 %) 
to     106202 ( 2.5 %)    107246 ( 2.51 %)   108362 ( 2.45 %) 

```r
kable(sum10,caption="Total occurences for top 10 words")
```



Table: Total occurences for top 10 words

Blogs               News                Twitter           
------------------  ------------------  ------------------
867456 ( 20.43 %)   872348 ( 20.42 %)   979621 ( 22.13 %) 


## 2-grams analysis

We now do the same operation for 2-grams. While the distribution still follows a power law, the top 10 2-grams now only account for around 2\% of the occurences. This show that a larger proportion of 2-grams will be necessary to make 2-grams-based predictions useful.




```r
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
top10 <- cbind(head(blogsNGrams$twoGram,n=10),source = "Blogs",total=blogsNGrams$nbWords)
top10 <- rbind(top10,cbind(head(newsNGrams$twoGram,n=10),source= "News",total=newsNGrams$nbWords))
top10 <- rbind(top10,cbind(head(twittNGrams$twoGram,n=10),source= "Twitter",total=twittNGrams$nbWords))

#Calculate proportion of top10 words per data source
sum10 <- top10 %>%
    group_by(source,total) %>%
    summarise(word="Sum top 10",occ=sum(occ)) %>%
    select(word,occ,source,total) %>%
    mutate(proportion=paste("(",round(occ*100/total,2),"%)")) %>%
    unite("occurences",occ,proportion,sep=" ") %>%
    select(occurences,source) %>%
    spread(source,occurences)

#Display top10 occurences and the sum
top10 <- top10 %>%
    mutate(proportion=paste("(",round(occ*100/total,2),"%)")) %>%
    unite("occurences",occ,proportion,sep=" ") %>%
    select(-total) %>%
    spread(source,occurences)
    
kable(top10,caption="Top 10 2-grams per source")
```



Table: Top 10 2-grams per source

2gram            Blogs             News              Twitter         
---------------  ----------------  ----------------  ----------------
and_i            5589 ( 0.13 %)    5632 ( 0.13 %)    5720 ( 0.13 %)  
and_the          5625 ( 0.13 %)    5763 ( 0.13 %)    5910 ( 0.13 %)  
for_the          5744 ( 0.14 %)    5871 ( 0.14 %)    5883 ( 0.13 %)  
i_m              6739 ( 0.16 %)    6613 ( 0.15 %)    6733 ( 0.15 %)  
in_the           15336 ( 0.36 %)   15336 ( 0.36 %)   15358 ( 0.35 %) 
it_.profanity.   13126 ( 0.31 %)   13258 ( 0.31 %)   13478 ( 0.3 %)  
of_the           18590 ( 0.44 %)   18574 ( 0.43 %)   18819 ( 0.43 %) 
on_the           7289 ( 0.17 %)    7595 ( 0.18 %)    7478 ( 0.17 %)  
to_be            6892 ( 0.16 %)    6944 ( 0.16 %)    6928 ( 0.16 %)  
to_the           8524 ( 0.2 %)     8592 ( 0.2 %)     8602 ( 0.19 %)  

```r
kable(sum10,caption="Total occurences for top 10 2-grams")
```



Table: Total occurences for top 10 2-grams

Blogs            News             Twitter         
---------------  ---------------  ----------------
93454 ( 2.2 %)   94178 ( 2.2 %)   94909 ( 2.14 %) 


```r
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

total1gramOcc <- cbind(blogsNGrams$oneGram,source="Blogs") %>%
rbind(cbind(newsNGrams$oneGram,source="News")) %>%
rbind(cbind(twittNGrams$oneGram,source="Twitter"))

ggplot(total1gramOcc, aes(occ)) +
    geom_histogram(bins = 30, pad=TRUE) +
    scale_y_log10()+
    scale_x_log10()+
    facet_grid(. ~ source)+
    labs(x="Number of occurences",title="Distribution of 2-grams occurences")
```

![](eda_files/figure-html/2-gram histogram-1.png)<!-- -->

# Plans for the Shiny prediction app

## Using NLP tools

This first step exploratory analysis gives us a good idea of the specificity of each dataset. We have shown that relatively few words (1-grams) are necessary to account for a large proportion of occurences in each dataset. However, the more we increase n in n-grams, the more examples are necessary. A good backoff strategy will therefore be necessary to enable good prediction with a reasonable, shiny.io compatible prediction model.

For the final application, we will use NLP tools such as the tm package to perform sentence segmentation inside each line. This will allow us to find the most common first words using the full corpus. We will then be able to remove stopwords but perform grammatical segmentation to provide predictions according to the proper construction of a sentence.

## Dealing with limited resources

The data used for this analysis was small enough to fit in memory. If needed for computational reasons, specially given the limited resources at shiny.io, we will use the following strategies:

- Subsetting strategies: as seen in the inference class of the specialization, since the lines follow a normal distribution (as shown in the first part of this analysis) we can take multiple subsamples of the data to get a good approximation of the results on the full dataset
- Reduction strategy: since the word occurence distribution follows a power law, we can use a small subest of the most-occuring words and still have a high coverage. Least common words can be factorized as `.rare.` exactly like all numbers are factorized into `.number.` and bad words as `.profanity.`.
- backoff strategy: the more words are used to generate the prediction, the better the prediction. However since the learning corpus and memory are constrained, we will backoff to less words (up to using only one word) in cases where the particular phrase has not been seen enough to be kept into the model.

# Conclusion

Thie exploratory analysis has enabled us to understand better the corpus and enabled us to setup a strategy for the final app. The next weeks will be used to understand how NLP tools can help us to create the smallest model that can fit in shiny.io's constraint and still provide a great prediction for the user.


