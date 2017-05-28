# Setting working directory
setwd("/Users/tyagraj/desktop/TweetsClassification")

set.seed(11)

# Loading data in to R
Tweets = read.csv("TweetsDataSet.csv", header = T, stringsAsFactors = F)

#Choosing sample of data using sample method
samp <- sample(nrow(Tweets),0.30* nrow(Tweets))
Tweets <- Tweets[samp,]
Tweets = Tweets[51000:60000,]
# Converting Tweets$label as factor
Tweets$label = factor(Tweets$label)

#Splitting data in to Train and test Set using sample method

samp <- sample(nrow(Tweets),0.80* nrow(Tweets))
Tweets_train <- Tweets[samp,]
Tweets_test <- Tweets[-samp,]

# Loading necessary libraries
library(tm)
library(wordcloud)
library(slam)
library(SnowballC)

# Preprocessing

#Converting Tweets/Data in to Corpus
Docs = Corpus(VectorSource(Tweets$tweet))

# Removing punctuations 
for(i in seq(Docs))   
{   
  Docs$content[[i]] <- gsub("b'", "", gsub('b"', '',Docs$content[[i]]))
} 
# converting to lower case
Docs = tm_map(Docs, tolower)

# removing punctuation
Docs = tm_map(Docs, removePunctuation)

# remove numbers
Docs = tm_map(Docs, removeNumbers)

#remove stop words
Docs = tm_map(Docs, removeWords, stopwords('english'))

# stem document
Docs <- tm_map(Docs, stemDocument)

 
#remove unnecesary spaces
Docs = tm_map(Docs, stripWhitespace)

#Dividing Corpus in to train and test

Corpus_train = 
# Document term matrix
Dtm = DocumentTermMatrix(Docs)


# Transforming Dtm to matrix to data frame as dateframe is easier to work with
mat.df <- as.data.frame(data.matrix(Dtm), stringsAsfactors = FALSE)

#Splitting mat.df in to Train and Test Set
amp <- sample(nrow(mat.df),0.80* nrow(mat.df))
train <- mat.df[amp,]
test <- mat.df[-amp,]

findFreqTerms(sms_dtm_train, 5)

library('e1071')
library('SparseM')

#Creating NaiveBayes Model
model <- naiveBayes(as.matrix(train) , as.factor(Tweets_train$label),laplace = 2)

#Predicting Results
result <- predict(model,as.matrix(test))

library(caret)
confusionMatrix(result, Tweets_test$label)
#Preparing confusion matrix
Confusion = table(result, Tweets_test$label)

#Testing Accuracy of model
accuracy <- sum(diag(Confusion))/length(test)
accuracy * 100

