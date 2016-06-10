# This file mainly contains all the explanatory data analysis
# Chao Liu
# 06/2016

source("Functionality.R")
#writeTestDocument(200000,20000,20000)

#read data
rctol <- list(reader = readPlain, language = "en", load = TRUE)
dat <- VCorpus(x = DirSource("F://Data Science//Capstone//Coursera-SwiftKey//test"), readerControl = rctol)
rm(rctol)

#Profane words according to Seven dirty words from Wikipedia
profane_word <- c("shit","piss","fuck","cunt","cocksucker","motherfucker","tits")
#pattern <- "[Ss]hit|[Pp]iss|[Ff]uck|[Cc]unt|[Cc]ocksucker|[Mm]otherfucker|[Tt]its"

#simple cleaning,stemming, tokenization, profane filtering
dat <- tm_map(dat, removeNumbers)
dat <- tm_map(dat, content_transformer(tolower))
dat <- tm_map(dat, removeWords, stopwords("english"))
dat <- tm_map(dat, stripWhitespace)
dat <- tm_map(dat, removePunctuation)
dat <- tm_map(dat, stemDocument)
dat <- tm_map(dat, removeWords, stopwords("english"))
dat <- tm_map(dat, removeWords, profane_word)
#ctrol <- list(removePunctuation = TRUE, stripWhitespace = TRUE, stopwords = TRUE)

#Calculating TDM
my_TDM <- TermDocumentMatrix(dat)
#rm_dat <- removeSparseTerms(my_TDM, 0.4) #reduce dimension by sparse ratio

#reduce dimension by total freq
test <- as.matrix(my_TDM)
test <- list(test, name  = rownames(test))
test <- tbl_df(as.data.frame(test))
test <- mutate(test, freq = test_blogs.txt + test_news.txt + test_twitter.txt)
test <- arrange(test, desc(freq))
test$name <- as.character(test$name)
uncommon_words <- as.character(test$name[cover_ratio(test$freq,0.9):dim(test)[1]])

#dat <- remove_uncommon_words_by_step(dat, uncommon_words,step = 2000)
test <- test[1:cover_ratio(test$freq,0.9),]
ninety_coverage <- dim(test)[1]/dim(my_TDM)[1] #See the data compression level
rm(my_TDM);rm(uncommon_words)

#explanatory data analysis
head((test[order(test$freq, decreasing = TRUE)[1:20],]),20) # most frequent words
findAssocs(my_TDM, "love", 0.9999) # find associates

#clustering
hclusting <- hclust(dist(test), method = "ward")
plot(hclusting)

#drew word cloud (twitter, blogs, news)
pal <- brewer.pal(5,"Dark2")
wordcloud(words = test$name, freq = test$freq, min.freq = test$freq[30], colors = pal)

wordcloud(words = test$name, freq = test$test_blogs.txt, min.freq = sort(test$test_blogs.txt,decreasing = T)[30], 
          colors = pal)

wordcloud(words = test$name, freq = test$test_news.txt, min.freq = sort(test$test_news.txt,decreasing = T)[30], 
          colors = pal)

wordcloud(words = test$name, freq = test$test_twitter.txt, min.freq = sort(test$test_twitter.txt,decreasing = T)[30], 
          colors = pal)

#cosine relationship
correlation_matrix <- matrix(data = 0, nrow = 3, ncol = 3)
for(i in 1:3){
    for(j in 1:3){
        correlation_matrix[i,j] <- dissimilarity(test[,i],test[,j])
    }
}
rownames(correlation_matrix) <- names(test)[1:3]; colnames(correlation_matrix) <- names(test)[1:3]
correlation_matrix 

#ngrams

#bigram
ng <- ngram(concatenate(content(dat[[1]]),content(dat[[2]]),content(dat[[3]])), n = 2)
#ng <- ngram_asweka(concatenate(content(dat[1])), min = 2, max = 3)
wordtable <- get.phrasetable(ng)
wordtable <- remove_ngram_uncommon_words_by_step(wordtable,uncommon_words,2000)
#head(arrange(wordtable,desc(freq)),20)

#tri-gram
ng3 <- ngram(concatenate(content(dat[[1]]),content(dat[[2]]),content(dat[[3]])), n = 3)
wordtable3 <- get.phrasetable(ng3)
wordtable3 <- remove_ngram_uncommon_words_by_step(wordtable3,uncommon_words,2000)

#quad-gram
ng4 <- ngram(concatenate(content(dat[[1]]),content(dat[[2]]),content(dat[[3]])), n = 4)
wordtable4 <- get.phrasetable(ng4)
wordtable4 <- remove_ngram_uncommon_words_by_step(wordtable4,uncommon_words,2000)

#writing data
#write.csv(test,file = "training.csv")
#write.csv(wordtable,file = "bigram.csv")
#write.csv(wordtable3,file = "trigram.csv")
#write.csv(wordtable4,file = "quadgram.csv")





