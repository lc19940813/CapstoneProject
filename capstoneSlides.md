Slide Deck for Capstone Project of Coursera Data Science Specialization
========================================================
author: Chao Liu
date: 2016-06-09
transition: rotate

Data Source
========================================================
**Introduction**  

The data is from a corpus called [HC Corpora](www.corpora.heliohost.org). See the [readme file](http://www.corpora.heliohost.org/aboutcorpus.html) for details on the corpora available. The files have been language filtered but may still contain some foreign text. In this Capstone, I choose the en_US dataset as our training dataset.  

---

**Dataset**  

This is the training data:[Dataset](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)

Description of the Algorithm
========================================================

The main algorithm used to predict the next word is ngrams model with some naive backoff models. To be more concrete, this algorithm consists of the following parts: 
  
1. Extracting and Cleaning ngrams from Corpus  
    * Noise Removal and Profane Filtering
    * Getting and Storing ngrams  
2. Creating Backoff Models
    * Simple Backoff Model
    * Google Stupid Backoff Model

Extracting and Cleaning ngrams from Corpus
========================================================
  
* Noise Removal and Profane Filtering  
    1. We use `tm` package to remove the necessary numbers, punctuations, stopwords of the original Corpus and input.
    2. According to the definition of seven dirty words from Wikipedia, we remove some fundamental profane words without hurting too much.
    3. Since there are so many words in this big corpus, so I decide only to use around 15% words which can cover 90% contents of the whole corpus to reduce the dimensions.  

---  
* Getting and Storing ngrams   
    1. We took advantage of `ngrams` package to parse the original Corpus and did some explantory analysis. And then remove those ngrams that contains the removed words defined in the formal step.  
    2. Since the analyzing process might be time-consuming, I wrote the bigram,trigram and quadtram results into local .csv file for further computation.  

Creating Backoff Models
========================================================
* Simple Backoff Model  
    1. Suppose we have a input "a b", after cleaning the words as the desired form, we first check the trigram we have, and choose the most frequent words as the next words if there exists trigram that starts with "a b".
    2. On the other hand, if we didn't have trigrams that start with "a b", we will then seek the bigrams that begin with "b". If we cann't find a proper bigram again, then I will suggest this input is not being learned so this model will return the most frequant unigrams.  
        
---
* Google Stupid Backoff Model
    1. As a slightly more sophisticated model, google's stupid backoff model will do almost the same thing as above, but it will use a data smoothing algorithm to expand the probablity of unseen ngrams. I alse write this google backoff model in the Functionality.R file.  
    2. This model is more general than the first one, and the result is the same when we input the seen ngrams. However, since we have 8000 words, so each time we have to calculate the scores for 8000 words which will take half an hour in my laptop to get the right scores for one input.  
    
ShinyApp Instructions
======================================================== 
* Input
    1. On the side panel, the user can enter the sentence or a series of words as input for the prediction model. After hitting the "Submit", the user can obtain the final result in the main panel.
    2. Moreover, by choosing figures, the user can make some comparison of the attributes of the relative next words and totol words of the corpus to see how specific environment matters.  

---
* Output
    1. The first output is corresponding predicted word with alternatives by a decreasing order of probability.
    2. The second output is the figures consist of wordcloud and density of freq of words chosen by the users.
    

