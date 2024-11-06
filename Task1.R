library(corrplot) # for plotting correlation matrix
library(ggplot2)
library(leaps)  # for best subset selection
library(gridExtra)  # for combining plots

# import data
data <- read.csv('data_task1.csv')
data <- data[,-1]

# scale data, make sure that mean is 0 and standard deviation is 1
# # define Min-Max function
# normalization <- function(x) {
#   return((x - min(x)) / (max(x) - min(x)))
# }

feature <- data[,-1]
scaledFeature <- data.frame(scale(feature))
scaledData <- data.frame(cbind(DV=data$DV, scaledFeature))

# correlation matrix, representing linear relationship
corrMat <- cor(scaledData)
corrplot(corrMat,
         method = "circle",       # 使用圆圈
         type = "upper",         
         col = COL2("RdBu", 10), # 使用红蓝配色
         tl.col = "black",
         tl.srt = 45,
         order = "original")
# 3 pairs of features have high correlation, which may cause multicollinearity.
# TC-LDL, HDL-APOA1, WBC-NEU

#### PCA ####
pca_result <- prcomp(scaledFeature, center = TRUE, scale. = TRUE)
summary(pca_result)
# biplot(pca_result)
# 获取每个主成分的方差
pca_variance <- pca_result$sdev^2
# 绘制Scree Plot
plot(pca_variance, type = "b", main = "Scree Plot", xlab = "Principal Components", ylab = "Variance Explained")

# 计算每个主成分的方差解释比例
variance_explained <- pca_variance / sum(pca_variance)
# 计算累计方差解释比例
cumulative_variance_explained <- cumsum(variance_explained)
# 选择累计方差解释比例大于90%的主成分
n_components <- which(cumulative_variance_explained >= 0.95)[1]
# 查看选择的主成分数
n_components

pca_feature <- data.frame(pca_result$x[, 1:n_components])
pca_data <- data.frame(cbind(DV=data$DV, pca_feature))

# # 画出带标签的散点图
# ggplot(pca_data, aes(x = PC3, y = PC2, color = DV)) +
#   geom_point() +
#   labs(title = "PCA - First Two Principal Components")




# tcldl <- data.frame(TC=scaledData$TC, LDL=scaledData$LDL)
# hdlapoa1 <- data.frame(HDL=scaledData$HDL, APOA1=scaledData$APOA1)
# wbcneu <- data.frame(WBC=scaledData$WBC, NEU=scaledData$NEU)
# # scatter plot
# p1 <- ggplot(tcldl, aes(x = TC, y = LDL)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE, color = "blue") +
#   theme_minimal() +
#   labs(title = "TC-LDL scatter plot")
# 
# p2 <- ggplot(hdlapoa1, aes(x = HDL, y = APOA1)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE, color = "blue") +
#   theme_minimal() +
#   labs(title = "HDL-APOA1 scatter plot")
# 
# p3 <- ggplot(wbcneu, aes(x = WBC, y = NEU)) + 
#   geom_point() +
#   geom_smooth(method = "lm", se = TRUE, color = "blue") +
#   theme_minimal() +
#   labs(title = "WBC-NEU scatter plot")
# 
# # 使用 grid.arrange 将三幅图排列在一起
# grid.arrange(p1, p2, p3, ncol = 3)
# 
# 
# # PCA
# pca_result <- prcomp(tcldl, scale. = TRUE)
# # summary(pca_result)
# # 提取第一个主成分
# pca_tcldl <- data.frame(TCLDL=pca_result$x[, 1:1])  # 保留前两个主成分
# # head(pca_tcldl)
# 
# pca_result <- prcomp(hdlapoa1, scale. = TRUE)
# # summary(pca_result)
# # 提取第一个主成分
# pca_hdlapoa1 <- data.frame(HDLAPOA1=pca_result$x[, 1:1])  # 保留前两个主成分
# # head(pca_hdlapoa1)
# 
# pca_result <- prcomp(wbcneu, scale. = TRUE)
# # summary(pca_result)
# # 提取第一个主成分
# pca_wbcneu <- data.frame(WBCNEU=pca_result$x[, 1:1])  # 保留前两个主成分
# # head(pca_wbcneu)
# print(pca_result$rotation)




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

