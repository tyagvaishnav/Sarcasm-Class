# Setting working directory
setwd("/Users/tyagraj/desktop/TweetsClassification")
# Packages
library(tm)
library(class)
library(SnowballC)
library(ggplot2) 
library(wordcloud)
library(e1071)
library(dplyr)
library(caret)
# Library for parallel processing
library(doMC)
registerDoMC(cores=detectCores())

# Loading data in to R
Tweets = read.csv("TweetsDataSet.csv", header = T, stringsAsFactors = F)

# Checking how many Tweets are labelled sarcastic and non-sarcastic
table(Tweets$label)

# Checking the percentage of sarcastic and non-sarastic labels
prop.table(table(Tweets$label))

# Checking Data Structure 
str(Tweets)
# Changing to factor
Tweets$label = as.factor(Tweets$label)

# Converting Tweets in to Corpus
Docs = Corpus(VectorSource(Tweets$tweet))

# to clean the data
# Removing b' & b" which is in starting of almost every Tweet
for(i in seq(Docs))   
{   
  Docs$content[[i]] <- gsub("b'", "", gsub('b"', '',Docs$content[[i]]))
}

Docs = tm_map(Docs, tolower)
Docs = tm_map(Docs, removePunctuation)
Docs = tm_map(Docs, removeNumbers)
Docs = tm_map(Docs, removeWords, stopwords('english'))
Docs = tm_map(Docs, stemDocument)
Docs = tm_map(Docs, stripWhitespace)

# Creating Document term matrix
Dtm = DocumentTermMatrix(Docs)

# Creating Term Document matrix
Tdm = TermDocumentMatrix(Docs)

# Most frequent terms which appears in the Tweets atleast 4000 times
findFreqTerms(Tdm, 2500)


# Most frequent terms
freq = colSums(as.matrix(Dtm))
wf = data.frame(word=names(freq), freq=freq)   
head(wf)  
  
p <- ggplot(subset(wf, freq > 2700), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))  
p


set.seed(162)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 

set.seed(101)
dark1 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq =1000, rot.per=0.2, colors=dark1) 

# Non-Sarcastic Analysis
nonsarcastic = Tweets[51301:91298,]
prop.table(table(nonsarcastic$label))

Docs = Corpus(VectorSource(nonsarcastic$tweet))

# to clean the data
# Removing b' & b" which is in starting of almost every Tweet
for(i in seq(Docs))   
{   
  Docs$content[[i]] <- gsub("b'", "", gsub('b"', '',Docs$content[[i]]))
}

Docs = tm_map(Docs, tolower)
Docs = tm_map(Docs, removePunctuation)
Docs = tm_map(Docs, removeNumbers)
Docs = tm_map(Docs, removeWords, stopwords('english'))
Docs = tm_map(Docs, stemDocument)
Docs = tm_map(Docs, stripWhitespace)

# Creating Document term matrix
Dtm = DocumentTermMatrix(Docs)

# Creating Term Document matrix
Tdm = TermDocumentMatrix(Docs)

# Most frequent terms which appears in the Tweets atleast 4000 times
findFreqTerms(Tdm, 2500)


# Most frequent terms
freq = colSums(as.matrix(Dtm))
wf = data.frame(word=names(freq), freq=freq)   
head(wf)  

p <- ggplot(subset(wf, freq > 2700), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))  
p


set.seed(162)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 

set.seed(101)
dark1 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq =1000, rot.per=0.2, colors=dark1) 

# Sarcastic Analysis
sarcastic = Tweets[1:51300,]
prop.table(table(sarcastic$label))


Docs = Corpus(VectorSource(sarcastic$tweet))

# to clean the data
# Removing b' & b" which is in starting of almost every Tweet
for(i in seq(Docs))   
{   
  Docs$content[[i]] <- gsub("b'", "", gsub('b"', '',Docs$content[[i]]))
}

Docs = tm_map(Docs, tolower)
Docs = tm_map(Docs, removePunctuation)
Docs = tm_map(Docs, removeNumbers)
Docs = tm_map(Docs, removeWords, stopwords('english'))
Docs = tm_map(Docs, stemDocument)
Docs = tm_map(Docs, stripWhitespace)

# Creating Document term matrix
Dtm = DocumentTermMatrix(Docs)

# Creating Term Document matrix
Tdm = TermDocumentMatrix(Docs)

# Most frequent terms which appears in the Tweets atleast 4000 times
findFreqTerms(Tdm, 2500)


# Most frequent terms
freq = colSums(as.matrix(Dtm))
wf = data.frame(word=names(freq), freq=freq)   
head(wf)  

p <- ggplot(subset(wf, freq > 2700), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))  
p


set.seed(162)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2) 

set.seed(101)
dark1 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, min.freq =1000, rot.per=0.2, colors=dark1) 



# Model Based KNN 1

# selecting Sample to work on 
samp = sample(nrow(Tweets),0.10*nrow(Tweets))
Tweets = Tweets[samp,]

# Checking sample carries the ratios of labels for the selected sample
prop.table(table(Tweets$label))

# Create corpus
Docs <- Corpus(VectorSource(Tweets$tweet))

# Clean corpus
for(i in seq(Docs))   
{   
  Docs$content[[i]] <- gsub("b'", "", gsub('b"', '',Docs$content[[i]]))
} 
Docs = tm_map(Docs, tolower)
Docs = tm_map(Docs, removePunctuation)
Docs = tm_map(Docs, removeNumbers)
Docs = tm_map(Docs, removeWords, stopwords('english'))
Docs = tm_map(Docs, stemDocument)
Docs = tm_map(Docs, stripWhitespace)
# Create dtm
Dtm <- DocumentTermMatrix(Docs)

# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(Dtm), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df <- cbind(mat.df, Tweets$label)

# Change name of new column to "category"
colnames(mat.df)[ncol(mat.df)] <- "Label"

# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .75))
test <- (1:nrow(mat.df))[- train]

# Isolate classifier
cl <- mat.df[, "Label"]

# Models based Knn can't be saved as it doesn't train a model

# Create model data and remove "Label"
modeldata <- mat.df[,!colnames(mat.df) %in% "Label"]

# Set seed for reproducible results
set.seed(100)

# Create model: training set, test set, training set classifier
knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train])

library(caret)
confusionMatrix(knn.pred, cl[test])




# Model based on Naive Bayes 2
# Loading data in to R
Tweets = read.csv("TweetsDataSet.csv", header = T, stringsAsFactors = F)
# selecting Sample
samp = sample(nrow(Tweets),0.20*nrow(Tweets))
Tweets = Tweets[samp,]

# Checking sample carries the ratios of labels for the selected sample
prop.table(table(Tweets$label))

# Converting label in to factos
Tweets$label = as.factor(Tweets$label)
# Preprocessing
#Converting Tweets/Data in to Corpus
Docs = Corpus(VectorSource(Tweets$tweet))

# Removing punctuations 
for(i in seq(Docs))   
{   
  Docs$content[[i]] <- gsub("b'", "", gsub('b"', '',Docs$content[[i]]))
} 
Docs = tm_map(Docs, tolower)
Docs = tm_map(Docs, removePunctuation)
Docs = tm_map(Docs, removeNumbers)
Docs = tm_map(Docs, removeWords, stopwords('english'))
Docs = tm_map(Docs, stemDocument)
Docs = tm_map(Docs, stripWhitespace)

# Document term matrix
Dtm = DocumentTermMatrix(Docs)

#Partitioning the Data
Tweets.train <- Tweets[1:13000,]
Tweets.test <- Tweets[13001:18259,]

Dtm.train <- Dtm[1:13000,]
Dtm.test <- Dtm[13001:18259,]

Docs.train <- Docs[1:13000]
Docs.test <- Docs[13001:18259]

#Feature Selection
fivefreq <- findFreqTerms(Dtm.train, 2)
length((fivefreq))

Dtm.train.nb <- DocumentTermMatrix(Docs.train, control=list(dictionary = fivefreq))

dim(Dtm.train.nb)

Dtm.test.nb <- DocumentTermMatrix(Docs.test, control=list(dictionary = fivefreq))

dim(Dtm.train.nb)

# Function to convert the word frequencies to yes (presence) and no (absence) labels
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}

# Apply the convert_count function to get final training and testing DTMs
trainNB <- apply(Dtm.train.nb, 2, convert_count)
testNB <- apply(Dtm.test.nb, 2, convert_count)

# Training the Naive Bayes Model
set.seed(101)
NbModel <- naiveBayes(trainNB, Tweets.train$label, laplace = 1) 

# saving the model to disk
saveRDS(NbModel, "./NbModel.rds")

# load the model
Model <- readRDS("./NbModel.rds")

# Use the NB classifier we built to make predictions on the test set.
pred <- predict(Model, newdata=testNB) 

# Checking the Accuracy for Model
confusionMatrix(pred, Tweets.test$label)
pred


