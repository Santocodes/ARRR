#import libraries to work with
library(plyr)
library(tm)
library(wordcloud)
library(topicmodels)
library(ggplot2)
library(RColorBrewer)
library(syuzhet)
library(coreNLP)

############ Word cloud and Topic modeling###################################
SAMSUNG <- read.csv("C:/Users/Santy/Downloads/SAMSUNG.csv")
# Prepare  dataset and test dataset according to the following steps:

# 1. Extract the "text" column as textdata for transformation
# 2. Convert to corpus
# 3. Transform all characters to lower cases
# 4. Remove punctuations 
# 5. Remove numbers
# 6. Remove white spaces
# 7. Remove stop words (use stoplist 'smart' in rm package and other custom stop words)
# 8. Applying the stemming
# 9. Covert the corpus to a plain text document
# 10. Create Wordcloud
# 11. Create the DTM[Doument Term Matrix] based on the corpus
# 12. Reduce the sparsity of dtm to 98%
myCorpus = Corpus(VectorSource(SAMSUNG$text))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, stripWhitespace)
mystopwords = c(stopwords('smart'),"amazon","find","found","make","makes","made","making","buy","buying","bought")
myCorpus = tm_map(myCorpus,removeWords, mystopwords)
myCorpus = tm_map(myCorpus, stemDocument)
myCorpus=tm_map(myCorpus,PlainTextDocument)
col=brewer.pal(6,"Dark2")
wordcloud(myCorpus, min.freq=25,max.word=Inf,width=1000,height=1000, random.order=F,colors=col)
topic_dtm = DocumentTermMatrix(myCorpus)
topic_dtm = removeSparseTerms(topic_dtm, sparse = 0.98)
rowTotals <- apply(topic_dtm , 1, sum) #Find the sum of words in each Document
topic_dtm  <- topic_dtm[rowTotals> 0, ]  

###########Topic Model s################
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

#######################################Sentiment analysis###########################
SAMSUNG <- read.csv("C:/Users/Santy/Downloads/SAMSUNG.csv")
S_V=get_text_as_string("C:/Users/Santy/Downloads/SAMSUNG.csv")#converts data.frame into string
class(S_V)
C_V=get_sentences(S_V,strip_quotes = T)#convert string into character
class(C_V)
afinn_vector <- get_sentiment(C_V, method="afinn")#Afinn method
head(afinn_vector)
nrc_data <- get_nrc_sentiment(C_V)
a=data.frame(ncr_data)
barplot(
  sort(colSums(prop.table(nrc_data[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Sample text", xlab="Percentage"
)
a=(nrc_data[, 1:10])

write.csv(a,file = "a.csv")





