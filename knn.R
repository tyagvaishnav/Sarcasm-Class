# Setting working directory
setwd("/Users/tyagraj/desktop/TweetsClassification")

# Packages
library(tm)
library(class)
library(SnowballC)

# Read csv
Tweets = read.csv("TweetsDataSet.csv", header = T, stringsAsFactors = F)

# selecting Sample
samp = sample(nrow(Tweets),0.10*nrow(Tweets))
Tweets = Tweets[samp,]
# Create corpus
docs <- Corpus(VectorSource(Tweets$tweet))

# Clean corpus
docs <- tm_map(docs, tolower)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument, language = "english")
for(i in seq(docs))   
{   
  docs$content[[i]] <- gsub("b'", "", gsub('b"', '',docs$content[[i]]))
} 
# Create dtm
dtm <- DocumentTermMatrix(docs)

# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(dtm), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df <- cbind(mat.df, Tweets$label)

# Change name of new column to "category"
colnames(mat.df)[ncol(mat.df)] <- "Label"

# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .75))
test <- (1:nrow(mat.df))[- train]

# Isolate classifier
cl <- mat.df[, "Label"]

# Create model data and remove "Label"
modeldata <- mat.df[,!colnames(mat.df) %in% "Label"]

# Set seed for reproducible results
set.seed(100)

# Create model: training set, test set, training set classifier
knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train])

library(caret)
confusionMatrix(knn.pred, cl[test])
