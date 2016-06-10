source("Functionality.R")

#Test 1: Simple Backoff Model
x <- proc.time()
pred_model("Data Science Rocks")
proc.time() - x

#Test 2: Google's Stupid Backoff Model
x <- proc.time()
pointer <- 0
prob <- numeric()
l <- length(training$freq)
for(pointer in 1:l){
    prob[pointer] <- prob_ngrams("Data", training$name[pointer])
}
proc.time() - x

#Some Intermediate Test:
x <- proc.time()
pred_ngram("seeing")
proc.time() - x

x <- proc.time()
pred_ngram("let us")
proc.time() - x

x <- proc.time()
prob_ngrams("just","see")
proc.time() - x
