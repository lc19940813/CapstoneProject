# This R file contains all the definition of functions of the main R file
# Chao Liu
# 06/2016

library(tm)
library(dplyr)
library(SnowballC)
library(wordcloud)
library(ngram)
training <- read.csv("train.csv",stringsAsFactors = FALSE)
wordtable <- read.csv("bigram.csv",stringsAsFactors = FALSE)
wordtable3 <- read.csv("trigram.csv",stringsAsFactors = FALSE)
wordtable4 <- read.csv("quadgram.csv",stringsAsFactors = FALSE)
worddict <- list(wordtable,wordtable3,wordtable4)

# This function takes a small part of the original data out as our training dataset
writeTestDocument <- function(twitter_lines, blogs_lines, news_lines){
    twitter <- character()
    con <- file("F://Data Science//Capstone//Coursera-SwiftKey//final//en_US//en_US.twitter.txt","r")
    twitter <- append(twitter, readLines(con, twitter_lines))
    write.table(twitter, file = "F://Data Science//Capstone//Coursera-SwiftKey//test//test_twitter.txt")
    close(con)
    
    blogs <- character()
    con <- file("F://Data Science//Capstone//Coursera-SwiftKey//final//en_US//en_US.blogs.txt","r")
    blogs <- append(blogs, readLines(con, blogs_lines))
    write.table(blogs, file = "F://Data Science//Capstone//Coursera-SwiftKey//test//test_blogs.txt")
    close(con)
    
    news <- character()
    con <- file("F://Data Science//Capstone//Coursera-SwiftKey//final//en_US//en_US.news.txt","r")
    news <- append(news, readLines(con, news_lines))
    write.table(news, file = "F://Data Science//Capstone//Coursera-SwiftKey//test//test_news.txt")
    close(con)
    
    rm(con);
    rm(twitter);rm(blogs);rm(news);
}

# This function will clean the input words for further counting and calculation
clean_words <- function(words){
    words <- removeNumbers(words)
    words <- tolower(words)
    words <- removeWords(words, stopwords("english"))
    words <- removeWords(words, c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits"))
    words <- removePunctuation(words)
    words <- wordStem(words)
    words <- concatenate(strsplit(words," ")[[1]])
    words
}

# The following functions can get the nearest number of the words that are able to constitute a dictionary
# to cover the corpus with the given coverage ratio
nearest_to_target <- function(freq, target){
    which.min(abs(freq-target))
}
cover_ratio <- function(freq,ratio){
    freq <- sort(freq, decreasing = TRUE)
    temp <- numeric()
    for(i in 1: length(freq)){
        temp[i] <- sum(freq[1:i])/sum(freq)
    }
    nearest_to_target(temp,ratio)
}

# This function calculates the dissimilarity according to simple cosine relationship
dissimilarity <- function(x, y){
    sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
}

# This function will remove those uncommon words by steps since the grepl function will be out of the memory 
# if we directly combine all the words together
remove_ngram_uncommon_words_by_step <- function(wordtable,uncommon_words,step = 1000){
    k <- 0
    l <- length(uncommon_words)
    while ((k + 1) * step <= l) {
        start <- 1+k*step
        end <- (k+1)*step
        pattern <- concatenate(uncommon_words[start:end], collapse = "|")
        wordtable <- wordtable[!grepl(pattern = pattern,x = wordtable$ngrams),]
        k <- k + 1
    }
    pattern <- concatenate(uncommon_words[(1+k*step):l], collapse = "|")
    wordtable <- wordtable[!grepl(pattern = pattern,x = wordtable$ngrams),]
    wordtable
}

# This function provides simple ngram model to prediect the next word
# The return value will be the 5 most probable words according to our training dataset
pred_ngram <- function(words){
    words <- clean_words(words)
    if(wordcount(words) == 0) return(" ")
    n <- wordcount(words) + 1
    if(n > 4) stop("Model has too many parameters")
    res <- character()
    index <- grep(paste0("^",words," .+"), worddict[[n-1]]$ngrams)
    if(length(index) == 0){
        return(" ")
    }
    m <- min(length(index),20)
    temp <- worddict[[n-1]][index,]
    #removeWords(temp$ngrams[1:n],paste(word, ""))
    for(i in 1:m){
        res[i] <- strsplit(temp$ngrams[i]," ")[[1]][length(strsplit(temp$ngrams[i]," ")[[1]])]
    }
    res
}

# This function will implement the simple backoff model that predicts the next word according to words
# collected from Corpus without any data smoothing or adjustment
simple_backoff_model <- function(input){
    while(pred_ngram(input)[1] == " ") {
        input <- concatenate(strsplit(input," ")[[1]][-1])
        if(wordcount(input) == 0 ) { 
            return(" ")
        }
    }
    pred_ngram(input)
}

# This function parses the input sentense and uses the simple backoff model to predict
pred_model <- function(input){
    l <- wordcount(input)
    if(l >= 4){
        words <- concatenate(strsplit(input," ")[[1]][(l-2):l])
    }
    else {
        words <- input
    }
    res <- simple_backoff_model(words)
    if(res[1] == " ") res <- training$name[1:20]
    res
}

# This function will count the frequency of given words pair 
count_word_freq <- function(words){
    n <- wordcount(words)
    if(n == 0) return(0)
    if(n > 4) stop("Model has too many parameters")
    if(n == 1){
        index <- grep(paste0("^",words,"$"),training$name)
        if(length(index) == 0) return(0)
        else{
            return(training$freq[index])
        }
    }
    else{
        temp <- paste0("^",words," ")
        index <- grep(temp,worddict[[n - 1]]$ngrams)
        if(length(index) == 0) return(0)
        else{
            return(worddict[[n - 1]]$freq[index])
        }
    }
}

# This function will implement the Stupid Backoff model created by Google
prob_ngrams <- function(input, word){
    word_f <- count_word_freq(word)
    if(word_f == 0) return(0)
    if(wordcount(input) == 0) return(word_f / sum(training$freq))
    input <- clean_words(input) 
    words <- concatenate(input,word)
    l <- count_word_freq(words)
    if(l > 0){
        return(l/count_word_freq(input))
    }
    else{
        input <- concatenate(strsplit(input," ")[[1]][-1])
        return(0.4*prob_ngrams(input,word))
    }
}

# This function will compute scores after smoothing by Stupid backoff model 
prob <- function(input){
    input <- clean_words(input)
    x <- training$name
    sapply(x, function(word){prob_ngrams(input,word)})
}

# This function is the ultimate prediction model according to Stupid backoff model
pred_model_stupid <- function(input){
    l <- wordcount(input)
    if(l >= 4){
        words <- concatenate(strsplit(input," ")[[1]][(l-2):l])
    }
    else {
        words <- input
    }
    x <- prob(words)
    y <- sort(x,decreasing = TRUE)
    res <- character()
    for(i in 1:5){
        res[i] <- training$name[which(prob == y[i])]
    }
    res
}