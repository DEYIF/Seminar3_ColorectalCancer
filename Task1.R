library(corrplot) # for plotting correlation matrix
library(ggplot2)
library(leaps)  # for best subset selection
library(gridExtra)  # for combining plots
library(glmnet)  # for ridge regression

# import data
data <- read.csv('data_task1.csv')
data <- data[,-1]
feature <- data[,-1]
scaledFeature <- data.frame(scale(feature)) # without label
scaledData <- data.frame(cbind(DV=data$DV, scaledFeature))  # with label

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

# Divide into training and testing sets
set.seed(2024)
# split the data into training and testing sets
trainIndex <- sample(1:nrow(data), 0.7*nrow(data))
trainFea <- scaledFeature[trainIndex,]
trainLabel <- scaledData[trainIndex,1]
testFea <- scaledFeature[-trainIndex,]
testLabel <- scaledData[-trainIndex,1]


x = as.matrix(trainFea) # transfer the data frame to matrix
y = as.matrix(trainLabel)
# Linear Regression
linear_reg = lm(y ~ ., data = trainFea)

# Stepwise by AIC
stepwise_reg = step(linear_reg, direction = "both",trace = 0)

#Ridge Regression

ridgefit <- glmnet(x, y, alpha = 0, standardize = TRUE)
plot(ridgefit, xvar = "lambda")
# cross-validation
ridgecv <- cv.glmnet(x, y, alpha = 0,nfolds = 10) # 10-fold cross-validation
plot(ridgecv)
# coef(ridgecv)
best_lambda <- ridgecv$lambda.min
ridgefit_best <- glmnet(x, y, alpha = 0, lambda = best_lambda)
# coef(ridgefit_best)
important_features <- which(abs(coef(ridgefit_best)[-1]) > 0.01) # 根据阈值选择特征

# Lasso Regression
lassofit <- glmnet(x, y, alpha = 1, standardize = TRUE)
plot(lassofit, xvar = "lambda")
# cross-validation
set.seed(1)
lassocv <- cv.glmnet(x, y, alpha = 1,nfolds = 10) # 10-fold cross-validation
plot(lassocv)

# predict
pre_linear <- predict(linear_reg, testFea)
pre_stepwise <- predict(stepwise_reg, testFea)
pre_ridge <- predict(ridgefit, newx = as.matrix(testFea))
pre_lasso <- predict(lassofit, newx = as.matrix(testFea))
# 将预测结果转换为01分类类型
pre_linear <- ifelse(pre_linear > 0.5, 1, 0)
pre_stepwise <- ifelse(pre_stepwise > 0.5, 1, 0)
pre_ridge <- ifelse(pre_ridge > 0.5, 1, 0)
pre_lasso <- ifelse(pre_lasso > 0.5, 1, 0)

# 计算评估指标：准确率、精确率、召回率、F1值、AUC、ROC曲线

#calculate the accuracy
accuracy_linear <- sum(pre_linear == testLabel) / length(testLabel)
print(accuracy_linear)
accuracy_stepwise <- sum(pre_stepwise == testLabel) / length(testLabel)
print(accuracy_stepwise)
accuracy_ridge <- sum(pre_ridge == testLabel) / length(testLabel)
print(accuracy_ridge)
accuracy_lasso <- sum(pre_lasso == testLabel) / length(testLabel)
print(accuracy_lasso)



# confusion matrix
confusion_matrix_linear <- table(pre_linear, testLabel)
confusion_matrix_stepwise <- table(pre_stepwise, testLabel)
confusion_matrix_ridge <- table(pre_ridge, testLabel)
confusion_matrix_lasso <- table(pre_lasso, testLabel)






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

#### Stepwise Regression ####


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





