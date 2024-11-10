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
newx <- as.matrix(testFea)

x = as.matrix(trainFea) # transfer the data frame to matrix
y = as.matrix(trainLabel)
# Linear Regression
linear_reg = lm(y ~ ., data = trainFea)

#Elastic Net
# 设置 alpha 为 0.5 表示 Lasso 和 Ridge 的混合
elastic_net_model <- cv.glmnet(x, y, alpha = 0.5, family = "binomial")  # 对于分类问题，选择 family = "binomial"
plot(elastic_net_model)


best_lambda <- elastic_net_model$lambda.min  # 选择使得均方误差最小的lambda值
print("best alpha:", best_lambda)

# 提取最佳模型的系数
best_model_coeff <- coef(elastic_net_model, s = "lambda.min")
print(best_model_coeff)
# 查看哪些特征被选择了（非零系数表示该特征被选择）
selected_features <- rownames(best_model_coeff)[which(best_model_coeff != 0)]
selected_features <- selected_features[-1]  # 去除截距项
print(selected_features)


# predict
pre_linear <- predict(linear_reg, testFea)
pre_elas <- predict(elastic_net_model, s = "lambda.min", newx = newx, type = "response")
# 将预测结果转换为01分类类型
pre_linear <- ifelse(pre_linear > 0.5, 1, 0)
pre_elas <- ifelse(pre_elas > 0.3, 1, 0)

# 计算评估指标：准确率、精确率、召回率、F1值、AUC、ROC曲线
# 准确率
accuracy_linear <- sum(pre_linear == testLabel) / length(testLabel)
accuracy_elas <- sum(pre_elas == testLabel) / length(testLabel)
# 精确率
precision_linear <- sum(pre_linear == 1 & testLabel == 1) / sum(pre_linear == 1)
precision_elas <- sum(pre_elas == 1 & testLabel == 1) / sum(pre_elas == 1)
# 召回率
recall_linear <- sum(pre_linear == 1 & testLabel == 1) / sum(testLabel == 1)
recall_elas <- sum(pre_elas == 1 & testLabel == 1) / sum(testLabel == 1)
# F1值
f1_linear <- 2 * precision_linear * recall_linear / (precision_linear + recall_linear)
f1_elas <- 2 * precision_elas * recall_elas / (precision_elas + recall_elas)
# AUC
roc_linear <- roc(testLabel, pre_linear)
roc_elas <- roc(testLabel, pre_elas)
# ROC曲线
plot(roc_linear, col = "red", lwd = 2, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
lines(roc_elas, col = "blue", lwd = 2)
legend("bottomright", legend = c("Linear Regression", "Elastic Net"), col = c("red", "blue"), lty = 1, lwd = 2)
# 输出评估指标
print(paste("Linear Regression Accuracy:", accuracy_linear))





# confusion matrix





