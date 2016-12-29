rm(list=ls())
train <- read.csv("C:/Users/Santy/Desktop/R Project 1/train.csv")
test <- read.csv("C:/Users/Santy/Desktop/R Project 1/test.csv")
install.packages("scales")
install.packages("ggplot2")
install.packages("caret")
install.packages("RANN")
install.packages("VIM")
install.packages("Metrics")
install.packages("randomForest")
library(randomForest)
library(Metrics)
library(VIM)
library(ggplot2)
library(caret)
library(RANN)
library(scales)

#############4.Exploring Data Analysis####################################

##4.1 Sales Price
attach(train)
ggplot(train)+
  geom_histogram(data = train,position = "stack",bins=50, show.legend = T, inherit.aes = TRUE, color="yellow",fill="brown")+
  aes(x=SalePrice)+
  scale_x_continuous(labels = comma)+
  geom_bar(width=0.2,position='dodge')
detach(train)

###4.##Extract continuous and catagorical columns from data frame
train=train[,-1]
test=test[,-1]
x=sapply(train, is.numeric)
y=sapply(train, is.factor)
train_cont=train[,x]
train_cat=train[,y]
##histogram 
d <- melt(train_cont[,-c(37)])
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()



#############5.1 Missing Values: count missing value####################################

Missing=function(x)
{
  a=as.vector(colnames(x))
  b=seq(from=1,to=ncol(x),by=1)
  result=data.frame()
  for (i in b)
  {
    c=sum(is.na(x[,i]))
    result=rbind(result,c)
  }
  output=data.frame(a,result)
  colnames(output)=c("Column Name","No. of Missing Values")
  output
}
Missing(train)
Missing(test)



###### 5.1 (1) Recode categorical variable missing values into No category ############ 
#train
summary(train$Alley)
train$Alley <- ifelse(is.na(train$Alley) == T, "No alley access", ifelse(train$Alley == "Grvl", "Grvl", "Pave"))
table(train$Alley)
train$Alley <- as.factor(train$Alley)

summary(train$BsmtQual)
train$BsmtQual <- ifelse(is.na(train$BsmtQual) == T, "No Basement", ifelse(train$BsmtQual == "Ex", "Ex", ifelse(train$BsmtQual == "Fa","Fa",ifelse(train$BsmtQual == "Gd","Gd","TA"))))
table(train$BsmtQual)
train$BsmtQual <- as.factor(train$BsmtQual)

summary(train$BsmtCond)
train$BsmtCond <- ifelse(is.na(train$BsmtCond) == T, "No Basement", ifelse(train$BsmtCond == "Fa", "Fa", ifelse(train$BsmtCond == "Gd","Gd",ifelse(train$BsmtCond == "Po","Po","TA"))))
table(train$BsmtCond)
train$BsmtCond <- as.factor(train$BsmtCond)

summary(train$BsmtExposure)
train$BsmtExposure <- ifelse(is.na(train$BsmtExposure) == T, "No Basement", ifelse(train$BsmtExposure == "Av", "Av", ifelse(train$BsmtExposure == "Gd","Gd",ifelse(train$BsmtExposure == "Mn","Mn","No"))))
table(train$BsmtExposure)
train$BsmtExposure <- as.factor(train$BsmtExposure)

summary(train$BsmtFinType1)
train$BsmtFinType1 <- ifelse(is.na(train$BsmtFinType1) == T, "No Basement", ifelse(train$BsmtFinType1 == "ALQ", "ALQ", ifelse(train$BsmtFinType1 == "BLQ","BLQ",ifelse(train$BsmtFinType1 == "GLQ","GLQ",ifelse(train$BsmtFinType1=="LwQ","LwQ",ifelse(train$BsmtFinType1=="Rec","Rec","Unf"))))))
table(train$BsmtFinType1)
train$BsmtFinType1 <- as.factor(train$BsmtFinType1)

summary(train$BsmtFinType2)
train$BsmtFinType2 <- ifelse(is.na(train$BsmtFinType2) == T, "No Basement", ifelse(train$BsmtFinType2 == "ALQ", "ALQ", ifelse(train$BsmtFinType2 == "BLQ","BLQ",ifelse(train$BsmtFinType2 == "GLQ","GLQ",ifelse(train$BsmtFinType2=="LwQ","LwQ",ifelse(train$BsmtFinType2=="Rec","Rec","Unf"))))))
table(train$BsmtFinType2)
train$BsmtFinType2 <- as.factor(train$BsmtFinType2)

summary(train$FireplaceQu)
train$FireplaceQu <- ifelse(is.na(train$FireplaceQu) == T, "No Fireplace", ifelse(train$FireplaceQu == "Ex", "Ex", ifelse(train$FireplaceQu == "Fa","Fa",ifelse(train$FireplaceQu == "Gd","Gd",ifelse(train$FireplaceQu == "Po","Po","TA" )))))
table(train$FireplaceQu)
train$FireplaceQu <- as.factor(train$FireplaceQu)

summary(train$GarageType)
train$GarageType <- ifelse(is.na(train$GarageType) == T, "No Garage", ifelse(train$GarageType == "2Types", "2Types", ifelse(train$GarageType == "Attchd", "Attchd", ifelse(train$GarageType == "Basment" , "Basment", ifelse(train$GarageType == "BuiltI", "BuiltI",ifelse(train$GarageType=="Carport","Carport","Detchd"))))))
table(train$GarageType)
train$GarageType <- as.factor(train$GarageType)

summary(train$GarageFinish)
train$GarageFinish <- ifelse(is.na(train$GarageFinish) == T, "No Garage", ifelse(train$GarageFinish == "Fin", "Fin", ifelse(train$GarageFinish == "RFn","RFn","Unf")))
table(train$GarageFinish)
train$GarageFinish <- as.factor(train$GarageFinish)

summary(train$GarageQual)
train$GarageQual <- ifelse(is.na(train$GarageQual) == T, "No Garage", ifelse(train$GarageQual == "Ex", "Ex", ifelse(train$GarageQual == "Fa","Fa",ifelse(train$GarageQual== "Gd", "Gd",ifelse(train$GarageQual=="Po","Po","TA")))))
table(train$GarageQual)
train$GarageQual <- as.factor(train$GarageQual)

summary(train$GarageCond)
train$GarageCond <- ifelse(is.na(train$GarageCond) == T, "No Garage", ifelse(train$GarageCond == "Ex", "Ex", ifelse(train$GarageCond == "Fa","Fa",ifelse(train$GarageCond=="Gd", "Gd" ,ifelse(train$GarageCond=="Po","Po","TA")))))
table(train$GarageCond)
train$GarageCond <- as.factor(train$GarageCond)

summary(train$PoolQC)
train$PoolQC <- ifelse(is.na(train$PoolQC) == T, "No Pool", ifelse(train$PoolQC == "Ex", "Ex", ifelse(train$PoolQC == "Fa","Fa","Gd")))
table(train$PoolQC)
train$PoolQC <- as.factor(train$PoolQC)

summary(train$Fence)
train$Fence <- ifelse(is.na(train$Fence) == T, "No Fence", ifelse(train$Fence == "GdPrv", "GdPrv", ifelse(train$Fence == "GdWo","GdWo",ifelse(train$Fence=="MnPrv","MnPrv","MnWw"))))
table(train$Fence)
train$Fence <- as.factor(train$Fence)

summary(train$MiscFeature)
train$MiscFeature <- ifelse(is.na(train$MiscFeature) == T, "None", ifelse(train$MiscFeature == "Gar2", "Gar2", ifelse(train$MiscFeature == "Othr","Othr",ifelse(train$MiscFeature=="Shed","Shed","TenC"))))
table(train$MiscFeature)
train$MiscFeature <- as.factor(train$MiscFeature)


## test
summary(test$Alley)
test$Alley <- ifelse(is.na(test$Alley) == T, "No alley access", ifelse(test$Alley == "Grvl", "Grvl", "Pave"))
table(test$Alley)
test$Alley <- as.factor(test$Alley)

summary(test$BsmtQual)
test$BsmtQual <- ifelse(is.na(test$BsmtQual) == T, "No Basement", ifelse(test$BsmtQual == "Ex", "Ex", ifelse(test$BsmtQual == "Fa","Fa",ifelse(test$BsmtQual == "Gd","Gd","TA"))))
table(test$BsmtQual)
test$BsmtQual <- as.factor(test$BsmtQual)

summary(test$BsmtCond)
test$BsmtCond <- ifelse(is.na(test$BsmtCond) == T, "No Basement", ifelse(test$BsmtCond == "Fa", "Fa", ifelse(test$BsmtCond == "Gd","Gd",ifelse(test$BsmtCond == "Po","Po","TA"))))
table(test$BsmtCond)
test$BsmtCond <- as.factor(test$BsmtCond)

summary(test$BsmtExposure)
test$BsmtExposure <- ifelse(is.na(test$BsmtExposure) == T, "No Basement", ifelse(test$BsmtExposure == "Av", "Av", ifelse(test$BsmtExposure == "Gd","Gd",ifelse(test$BsmtExposure == "Mn","Mn","No"))))
table(test$BsmtExposure)
test$BsmtExposure <- as.factor(test$BsmtExposure)

summary(test$BsmtFinType1)
test$BsmtFinType1 <- ifelse(is.na(test$BsmtFinType1) == T, "No Basement", ifelse(test$BsmtFinType1 == "ALQ", "ALQ", ifelse(test$BsmtFinType1 == "BLQ","BLQ",ifelse(test$BsmtFinType1 == "GLQ","GLQ",ifelse(test$BsmtFinType1=="LwQ","LwQ",ifelse(test$BsmtFinType1=="Rec","Rec","Unf"))))))
table(test$BsmtFinType1)
test$BsmtFinType1 <- as.factor(test$BsmtFinType1)

summary(test$BsmtFinType2)
test$BsmtFinType2 <- ifelse(is.na(test$BsmtFinType2) == T, "No Basement", ifelse(test$BsmtFinType2 == "ALQ", "ALQ", ifelse(test$BsmtFinType2 == "BLQ","BLQ",ifelse(test$BsmtFinType2 == "GLQ","GLQ",ifelse(test$BsmtFinType2=="LwQ","LwQ",ifelse(test$BsmtFinType2=="Rec","Rec","Unf"))))))
table(test$BsmtFinType2)
test$BsmtFinType2 <- as.factor(test$BsmtFinType2)

summary(test$FireplaceQu)
test$FireplaceQu <- ifelse(is.na(test$FireplaceQu) == T, "No Fireplace", ifelse(test$FireplaceQu == "Ex", "Ex", ifelse(test$FireplaceQu == "Fa","Fa",ifelse(test$FireplaceQu == "Gd","Gd",ifelse(test$FireplaceQu == "Po","Po","TA" )))))
table(test$FireplaceQu)
test$FireplaceQu <- as.factor(test$FireplaceQu)

summary(test$GarageType)
test$GarageType <- ifelse(is.na(test$GarageType) == T, "No Garage", ifelse(test$GarageType == "2Types", "2Types", ifelse(test$GarageType == "Attchd", "Attchd", ifelse(test$GarageType == "Basment" , "Basment", ifelse(test$GarageType == "BuiltI", "BuiltI",ifelse(test$GarageType=="Carport","Carport","Detchd"))))))
table(test$GarageType)
test$GarageType <- as.factor(test$GarageType)

summary(test$GarageFinish)
test$GarageFinish <- ifelse(is.na(test$GarageFinish) == T, "No Garage", ifelse(test$GarageFinish == "Fin", "Fin", ifelse(test$GarageFinish == "RFn","RFn","Unf")))
table(test$GarageFinish)
test$GarageFinish <- as.factor(test$GarageFinish)

summary(test$GarageQual)
test$GarageQual <- ifelse(is.na(test$GarageQual) == T, "No Garage", ifelse(test$GarageQual == "Ex", "Ex", ifelse(test$GarageQual == "Fa","Fa",ifelse(test$GarageQual== "Gd", "Gd",ifelse(test$GarageQual=="Po","Po","TA")))))
table(test$GarageQual)
test$GarageQual <- as.factor(test$GarageQual)

summary(test$GarageCond)
test$GarageCond <- ifelse(is.na(test$GarageCond) == T, "No Garage", ifelse(test$GarageCond == "Ex", "Ex", ifelse(test$GarageCond == "Fa","Fa",ifelse(test$GarageCond=="Gd", "Gd" ,ifelse(test$GarageCond=="Po","Po","TA")))))
table(test$GarageCond)
test$GarageCond <- as.factor(test$GarageCond)

summary(test$PoolQC)
test$PoolQC <- ifelse(is.na(test$PoolQC) == T, "No Pool", ifelse(test$PoolQC == "Ex", "Ex", ifelse(test$PoolQC == "Fa","Fa","Gd")))
table(test$PoolQC)
test$PoolQC <- as.factor(test$PoolQC)

summary(test$Fence)
test$Fence <- ifelse(is.na(test$Fence) == T, "No Fence", ifelse(test$Fence == "GdPrv", "GdPrv", ifelse(test$Fence == "GdWo","GdWo",ifelse(test$Fence=="MnPrv","MnPrv","MnWw"))))
table(test$Fence)
test$Fence <- as.factor(test$Fence)

summary(test$MiscFeature)
test$MiscFeature <- ifelse(is.na(test$MiscFeature) == T, "None", ifelse(test$MiscFeature == "Gar2", "Gar2", ifelse(test$MiscFeature == "Othr","Othr",ifelse(test$MiscFeature=="Shed","Shed","TenC"))))
table(test$MiscFeature)
test$MiscFeature <- as.factor(test$MiscFeature)

train$SalePrice <- as.numeric(train$SalePrice)



######################## 5.1 (2) Imputation Based on logical rules####################
#train
#pick mode type for missing categorical values
summary(train$Electrical)
train$Electrical[is.na(train$Electrical)==T] <- "SBrkr"

summary(train$MasVnrType)
train$MasVnrType[is.na(train$MasVnrType)==T] <- "None"

summary(train$MasVnrArea)
train$MasVnrArea[is.na(train$MasVnrArea)==T] <- 0

#test
#pick mode type for missing categorical values
summary(test$MSZoning) 
test$MSZoning[is.na(test$MSZoning)==T] <- "RL"

summary(test$Utilities) 
test$Utilities[is.na(test$Utilities)==T] <- "AllPub"

summary(test$Exterior1st)
test$Exterior1st[is.na(test$Exterior1st)==T] <- "VinylSd"

summary(test$Exterior2nd)
test$Exterior2nd[is.na(test$Exterior2nd)==T] <- "Stucco"

summary(test$MasVnrType)
test$MasVnrType[is.na(test$MasVnrType)==T] <- "None"

summary(test$MasVnrArea)
test$MasVnrArea[is.na(test$MasVnrArea)==T] <- 0

summary(test$BsmtFullBath)
test$BsmtFullBath[is.na(test$BsmtFullBath)==T] <- 0

summary(test$BsmtHalfBath)
test$BsmtHalfBath[is.na(test$BsmtHalfBath)==T] <- 0

summary(test$BsmtFinSF1)
test$BsmtFinSF1[is.na(test$BsmtFinSF1)==T] <- 0

summary(test$BsmtFinSF2)
test$BsmtFinSF2[is.na(test$BsmtFinSF2)==T] <- 0

summary(test$BsmtUnfSF)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)==T] <- 0

summary(test$TotalBsmtSF)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)==T] <- 0

summary(test$KitchenQual)
test$KitchenQual[is.na(test$KitchenQual)==T] <- "TA"

summary(test$Functional)
test$Functional[is.na(test$Functional)==T] <- "Typ"

summary(test$GarageType)
test$GarageType[is.na(test$GarageType)==T] <- "No Garage"

summary(test$GarageCars)
test$GarageCars[is.na(test$GarageCars)==T] <- 0

summary(test$GarageArea)
test$GarageArea[is.na(test$GarageArea)==T] <- 0
test$GarageType[test$Id==2577] <- "No Garage"

summary(test$SaleType)
test$SaleType[is.na(test$SaleType)==T] <- "WD"


#############################5.1 (3) Variable with low variation#########################
nsv <- nearZeroVar(train)
nsv
train <- train[,-nsv]
test <- test[,-nsv]

##############################5.1 (4)  Input and Standardize Variable##############################################
#visualization of missing values
#train
aggr(train,plot=T, col=c('sky blue', 'red'),numbers=T,prop= F, sortVars=T, cex.axis= 0.7,gap=3,labels=names(train),cex.numbers=0.6)
#test
aggr(test,plot=T, col=c('sky blue', 'red'),numbers=T,prop= F, sortVars=T, cex.axis= 0.5,gap=3,labels=names(train),cex.numbers=0.6)

#train
#KNN imputation for numeric variables
preObj <- preProcess(train,method="knnImpute")
train$LotFrontage <- predict(preObj,train)$LotFrontage

# change year into number of years build
train$GarageYrBlt = 2016-train$GarageYrBlt
train$GarageYrBlt[is.na(train$GarageYrBlt)==T] <- 0
train$YearBuilt = 2016-train$YearBuilt
train$YearRemodAdd = 2016-train$YearRemodAdd


#test
#KNN imputation for numeric variables
preObj1 <- preProcess(test,method="knnImpute")
test$LotFrontage <- predict(preObj1,test)$LotFrontage

# change year into number of years build
test$GarageYrBlt[test$Id==2593] <- 2007
test$GarageYrBlt = 2016-test$GarageYrBlt
test$GarageYrBlt[is.na(test$GarageYrBlt)==T] <- 0
test$YearBuilt = 2016-test$YearBuilt
test$YearRemodAdd = 2016-test$YearRemodAdd


##############################5.3 Raw Data Splitting############################################
set.seed(9999)
inTrain <- createDataPartition(y=train$SalePrice,p=0.6,list=FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]
dim(training)



###########################6 Predictive Model###############################

############6.1 Benchmark Linear Regression Model##########################
######################(2)PCA######################################
preProc <- preProcess(training[,-59],method="pca",pcacomp=2)
trainingLR <- predict(preProc,training)
testingLR <- predict(preProc,testing)

###(3) Model: Linear regression modeling and testing
lr <- train(SalePrice~ ., data=trainingLR, method="lm")
lrResult <- lr$finalModel
lrResult

#Linear regression predicting on testing
lrPred <- predict(lr,testingLR)
predLR <- data.frame(testingLR,lrPred)

#residual plot and RMSE
predLR$residual <- predLR$SalePrice-predLR$lrPred
plot(predLR$residual,pch=19)
rmse(log(lrPred),log(testingLR$SalePrice))
plot(testing$SalePrice,predLR$lrPred)  
abline(0,1,col="red") 

################Random Forest Modeling ##################################

#match the level of factor values
common <- intersect(names(train), names(test)) 
for (p in common) { 
  if (class(train[[p]]) == "factor") { 
    levels(test[[p]]) <- levels(train[[p]]) 
  } 
}


rf1 <- randomForest(SalePrice~ ., training, importance=TRUE,ntrees=300) 
rf1
round(importance(rf1),2)


##predict on testing dataset
pred <- predict(rf1, newdata=testing)
pred <- data.frame(testing,pred)

#residual plot and RMSE
pred$residual <- pred$SalePrice-pred$pred
plot(pred$residual,pch=19)
rmse(log(pred$pred),log(pred$SalePrice))
plot(x=testing$SalePrice,pred$pred)        
abline(0,1,col="red") 



#########################Gradient Boost Model###########################
install.packages("gbm")
library(gbm)
library(caret)
modFit <- train(SalePrice~.,method="gbm",data=training,verbose=FALSE)
print(modFit)
pred2 <- predict(modFit,testing)
pred2 <- data.frame(testing,pred2)

#residual plot 
pred2$residual <- pred2$SalePrice-pred2$pred2
plot(pred2$residual,pch=19)

#calculate rmse
rmse(log(pred2$pred2),log(pred2$SalePrice))
plot(x=testing$SalePrice,pred2$pred2)  
abline(0,1,col="red") 


