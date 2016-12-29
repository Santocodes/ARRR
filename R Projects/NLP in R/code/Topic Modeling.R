###############################        Project 2- We R Ready        #######################################
#############################  Sentiment Analysis & Recommendation System  ################################
rm(list=ls())

##############################      Part 2  Topic Modeling           #######################################

# Split the sample of reviews into positive and negative depending on the score range.
# Extract the text part
Reviews <- read.csv("C:/Users/siqiwan/Desktop/amazon-fine-foods/Reviews.csv", stringsAsFactors=FALSE)

inTrain = createDataPartition(y=Reviews$Id,p = 0.05,list = FALSE)
Reviews_sample = Reviews[inTrain,]
topicCorpus = Reviews_sample[,9]

############ Cleaning the text data for both positive################
mystopwords_pos = c(stopwords('english'),
                    "amazon","find","found","make","makes","made","making","buy","buying","bought",
                    "can","one","just","will","get","also","like","really","good","now","love","great",
                    "better","use","best","try","nice","much","food","work","add","per","think","eat","ive",
                    "well","pretty","bit","time","stuff","say","even","used","always","without","product",
                    "little","tried","eat","favoriate","know","order","taste","tastes","got","since","way",
                    "first","day","dont")

topicCorpus = Corpus(VectorSource(topicCorpus))
topicCorpus = tm_map(topicCorpus, removePunctuation)
topicCorpus = tm_map(topicCorpus, tolower)
topicCorpus = tm_map(topicCorpus, removeNumbers)
topicCorpus = tm_map(topicCorpus, stripWhitespace)
topicCorpus = tm_map(topicCorpus,removeWords, mystopwords_pos)
topicCorpus = tm_map(topicCorpus, stemDocument)
topicCorpus = tm_map(topicCorpus, PlainTextDocument)
topic_dtm = DocumentTermMatrix(topicCorpus)
topic_dtm = removeSparseTerms(topic_dtm, sparse = 0.98)
rowTotals <- apply(topic_dtm , 1, sum) #Find the sum of words in each Document
topic_dtm  <- topic_dtm[rowTotals> 0, ]  

###########Topic Model For Positive reviews################
##1.optimize the number of topic
best.model <- lapply(seq(2,10, by=1), 
                     function(k)
                     {LDA(topic_dtm,k,method="VEM")})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
best.model.logLik.df <- data.frame(topics=c(2:10), LL=as.numeric(as.matrix(best.model.logLik)))

library(ggplot2)
ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()
best.model.logLik.df[which.max(best.model.logLik.df$LL),]

##2 final topic model
library(topicmodels)
#Run LDA using VEM algorithm
ldaOut <- LDA(topic_dtm,4,method="VEM")
ldaOut.terms <- as.matrix(terms(ldaOut,5))
ldaOut.terms
#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities
