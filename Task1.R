library(corrplot) # for plotting correlation matrix
library(ggplot2)
library(leaps)  # for best subset selection

# import data
data <- read.csv('data_task1.csv')
data <- data[,-1]
# correlation matrix, representing linear relationship
corrMat <- cor(data)
corrplot(corrMat,
         method = "circle",       # 使用圆圈
         type = "upper",         
         col = COL2("RdBu", 10), # 使用红蓝配色
         tl.col = "black",
         tl.srt = 45,
         order = "original")
# The low correlation means that the feature is not linearly related 
# to the label, but it may be related to the label in a non-linear way.
# Also think about the multicollinearity between features.
# Cross validation can be used to evaluate the feature

# scale data using the min-max normalization
# define Min-Max function
normalization <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

feature <- data[,-1]
scaledFeature <- data.frame(apply(feature, 2, normalization))
scaledData <- data.frame(cbind(data$DV, scaledFeature))

# set the ramdom seed
set.seed(2024)
# split the data into training and testing sets
trainIndex <- sample(1:nrow(data), 0.7*nrow(data))
trainData <- data[trainIndex,]
testData <- data[-trainIndex,]

# best subset selection
m1 <- regsubsets(DV~., data = trainData, nvmax = 18)
summary(m1)
# model selection, e.g.: adjusted R^2, Mallows' CP, BIC

# plot the best subset selection
plot(m1)
plot(m1, scale = "Cp")
plot(m1, scale = "adjr2")
plot(m1, scale = "bic")

# find the best model
resm1 <- summary(m1)
data.frame(
  Adj.R2 = which.max(resm1$adjr2),
  CP = which.min(resm1$cp),
  BIC = which.min(resm1$bic)
)

# validerr <- rep(NA,18)
# for(i in 1:18){
#   coefi <- coef(m1,id=i)
#   pred <- testData[,names(coefi)]%*%coefi
#   validerr[i]<-mean((testData$DV-pred)^2)
# }
# plot(sqrt(validerr),ylab="Root MsE",type="b")
# plot(sgrt(m1$rss[-1]/70),ylab="residual s.s.",type="b")

# The best subset selection is a greedy algorithm, which is not guaranteed to find the best model.
# The best subset selection is computationally expensive, especially when the number of features is large.
# The best subset selection is not suitable for high-dimensional data.

# stepwise regression
# backward stepwise regression
m2 <- regsubsets(DV~., data = trainData, nvmax = 18, method = "backward")
summary(m2)
# model selection, e.g.: adjusted R^2, Mallows' CP, BIC
resm2 <- summary(m2)
data.frame(
  Adj.R2 = which.max(resm2$adjr2),
  CP = which.min(resm2$cp),
  BIC = which.min(resm2$bic)
)


# forward stepwise regression
m3 <- regsubsets(DV~., data = trainData, nvmax = 18, method = "forward")
summary(m3)
# model selection, e.g.: adjusted R^2, Mallows' CP, BIC
resm3 <- summary(m3)
data.frame(
  Adj.R2 = which.max(resm3$adjr2),
  CP = which.min(resm3$cp),
  BIC = which.min(resm3$bic)
)

