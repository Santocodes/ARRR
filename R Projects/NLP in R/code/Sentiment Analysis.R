###############################        Project 2- We R Ready        #######################################
#########################################  Text mining  ###################################################
rm(list=ls())
################################
#   Package Preparation
################################

install.packages("devtools")
install.packages("tm")
install.packages("plyr")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("party")
install.packages("caret")
install.packages("rminer")
install.packages("topicmodels")
library(devtools)
library(rword2vec)
library(tm)
library(plyr)
library(wordcloud)
library(SnowballC)
library(party)
library(caret)
library(rminer)
library(e1071)
library(topicmodels)
library(dplyr)

# Read dataset

Reviews <- read.csv("F:/Projects/Project 2 Monday Class/We R Ready/raw dataset/Reviews.csv", stringsAsFactors=FALSE)
View(Reviews)

# Extract 20% observations as a sample for the further analysis
# Remove the observations with neutral sentiment (score = 3)
# Split the sample dataset into train and test

Reviews_sample = Reviews[sample(nrow(Reviews), size=0.2*nrow(Reviews), replace=F),]
Reviews_sample = Reviews_sample[which(Reviews_sample$Score !=3),]
inTrain = createDataPartition(y=Reviews_sample$Id,p = 0.6,list = FALSE)
train = Reviews_sample[inTrain,]
test = Reviews_sample[-inTrain,]

# Add a new column in train and test showed the sentiment: "Positive" and "Negative"

train$Sentiment = ifelse(train$Score>3, "positive","negative")
test$Sentiment = ifelse(test$Score>3, "positive","negative")


############################            Part 1  Text Preparation            ####################################

# Prepare train dataset and test dataset according to the following steps:

# 1. Extract the "reviews" column as textdata for transformation
# 2. Convert to corpus
# 3. Transform all characters to lower cases
# 4. Remove punctuations 
# 5. Remove numbers
# 6. Remove white spaces
# 7. Remove stop words (use stoplist 'smart' in rm package and other custom stop words)
# 8. Applying the stemming
# 9. Covert the corpus to a plain text document
# 10. Create the DTM[Doument Term Matrix] based on the corpus
# 11. Reduce the sparsity of dtm to 98%

textdata = train[,9]
myCorpus = Corpus(VectorSource(textdata))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, stripWhitespace)
mystopwords = c(stopwords('smart'),"amazon","find","found","make","makes","made","making","buy","buying","bought")
myCorpus = tm_map(myCorpus,removeWords, mystopwords)
myCorpus = tm_map(myCorpus, stemDocument)
# inspect the first 5 documents
inspect(myCorpus[1:5])  
myCorpus = tm_map(myCorpus, PlainTextDocument)
train_dtm = DocumentTermMatrix(myCorpus)
train_dtm
train_dtm = removeSparseTerms(train_dtm, sparse = 0.98)
train_dtm


# Repeat the above preparation processes for test dataset:

textdata2 = test[,9]
myCorpus2 = Corpus(VectorSource(textdata2))
myCorpus2 = tm_map(myCorpus2, removePunctuation)
myCorpus2 = tm_map(myCorpus2, tolower)
myCorpus2 = tm_map(myCorpus2, removeNumbers)
myCorpus2 = tm_map(myCorpus2, stripWhitespace)
myCorpus2 = tm_map(myCorpus2,removeWords, mystopwords)
myCorpus2 = tm_map(myCorpus2, stemDocument)
myCorpus2 = tm_map(myCorpus2, PlainTextDocument)
test_dtm = DocumentTermMatrix(myCorpus2, control = list(dictionary=findFreqTerms(train_dtm)))
test_dtm
test_dtm = removeSparseTerms(test_dtm, sparse = 0.98)
test_dtm
  


##############################      Part 2  Sentiment Analysis           ###################################

# Combine the sentiment with the term frequencies into a data frame for training
train_m = data.frame(y=as.factor(train$Sentiment), x=as.matrix(train_dtm))
View(train_m)

# Combine the sentiment with the term frequencies into a data frame for testing
test_m = data.frame(y=test$Sentiment, x=as.matrix(test_dtm))
View(test_m)

#match columns

test_m <- select(test_m, which(colnames(test_m)%in% colnames(train_m)))
train_m <- select(train_m, which(colnames(train_m)%in% colnames(test_m)))

# Sentiment classification Methods: train with train_m and test with test_m

#############################
#   1. Ctree classifier
#############################

# Train the model
ctree_m = ctree(y~., data = train_m)
summary(ctree_m)
plot(ctree_m)

# Predict the test dataset based on the model
testPred1 = predict(ctree_m, test_m)

# Create the Confusion Matrix for evaluating the prediction results
confusionMatrix(testPred1, test_m[,1])
mmetric(testPred1,test_m[,1],c("ACC","TPR","PRECISION","F1"))

##Layout the sentiment prediction result
testPred1 <- as.vector(testPred1)
prediction <- as.data.frame(test$Text,testPred1)
View(prediction)
write.csv(prediction,"C:/Users/siqiwan/Desktop/amazon-fine-foods/prediction.csv")

#############################
#  2. NaiveBayes Classifier
#############################

# A Naive Bayes algorithm is used to predict a binary classification model that would predict if the review is positive or negative.
# Train the model, predict the test dataset and use Confusion Matrix for evaluation.

library(e1071)
nb_m = naiveBayes(y~., data=train_m)
summary(nb_m)
testPred2 = predict(nb_m,as.matrix(test_dtm))
confusionMatrix(testPred2,test_m[,1])
mmetric(testPred2,test_m[,1],c("ACC","TPR","PRECISION","F1"))

#############################
#   3. Ksvm classifier
#############################

# Train the model, predict the test dataset and use Confusion Matrix for evaluation.

library(kernlab)
ksvm_m = ksvm(y~., data=train_m)
summary(ksvm_m)
testPred3 = predict(ksvm_m,as.matrix(test_dtm))
confusionMatrix(testPred3,test_m[,1])
mmetric(testPred3,test_m[,1],c("ACC","TPR","PRECISION","F1"))

