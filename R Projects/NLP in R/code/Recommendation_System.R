install.packages("proxy")
install.packages("recommenderlab")
install.packages("reshape2")
library(proxy)
library(recommenderlab)
library(reshape2)
Rec <- read.csv("C:/Users/Santy/Desktop/amazon-fine-foods/Recommendation_System.csv")#read datset with userid, Productid and ratings
#Examine data
str(Rec)
summary(Rec)
Rec = unique(Rec)#remove duplicates
RatingMatrix = as(Rec,"realRatingMatrix")#convert to realRatingMatrix format
RatingMatrix
RatingMatrix = RatingMatrix[rowCounts(RatingMatrix)>20,colCounts(RatingMatrix)>20]#considering users who have reviewed atleast 20 products
recom_popular = Recommender(RatingMatrix[1:50], method = "POPULAR")#popular method 
names(getModel(recom_popular))
predictrating  = predict(recom_popular,RatingMatrix[27],n=15)#15 popular items recommended for user A1B05INWIDZ74O
as(predictrating,"list")
recom_UBCF=Recommender(RatingMatrix[1:50],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5))#User based collaborative filtering
names(getModel(recom_UBCF))
predictratingu  = predict(recom_UBCF,RatingMatrix[27],n=15)
as(predictratingu,"list")#top 15 items recommended for user $A1B05INWIDZ74O based on UBCF
recom_IBCF=Recommender(RatingMatrix[1:50],method="IBCF", param=list(normalize = "Z-score",method="Cosine"))#Item based collaborative filtering
names(getModel(recom_IBCF))
predictratingi  = predict(recom_IBCF,RatingMatrix[27],n=15)
as(predictratingi,"list")#top 15 items recommended for user $A1B05INWIDZ74O based on IBCF
