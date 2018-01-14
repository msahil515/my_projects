library(ggplot2)
library(ggpairs)
library(car)
library(caret)
library(pROC)
library(plotROC)
library(mlbench)
data <- read.csv("/Users/msahil515/Data mining/Project/Predicting employee attrition/data.csv")
data <- data[,-c(4,10,20)]
data <- subset(data,select = -c(EmployeeCount,StandardHours))
data$Attrition<- relevel(data$Attrition,"Yes")
head(data)
summary(data)
cor(data)
dept <- ggplot(data,aes(x=Attrition)) + geom_bar() + facet_grid(.~ Department)
dept
data$Attrition <- recode(data$Attrition,"'Yes'=1;'No'=0")


## checking if the factor variables have more than one level
## extracting all the factor variables
l <- sapply(data,function (x) is.factor(x))
l

m<- data[,l]
m
o <- ifelse(n <- sapply(m, function(x) length(levels(x))) == 1, "DROP", "NODROP")

o
drop_variables <- names(o)[o=="DROP"]

#(this also works by subsetting)  data <- subset(test_data,select = -eval(parse(text=drop_variables)))
data <- data[,!names(data) == drop_variables]
## Splitting the dataset
set.seed(515)
n <- nrow(data)
sam <- sample(1:n,floor(0.8*n))
train_data <- data[sam,]
test_data <- data[-sam,]








## random forest
tctrl <- trainControl(method = "cv",number = 10,classProbs = T,savePredictions = T)
ctrl <- trainControl(summaryFunction = twoClassSummary,method = "cv",number = 10,classProbs = TRUE,savePredictions = T)
mtryGrid <- data.frame(mtry = seq(1,28))







## Not required
# feature.names=names(train_data)
# 
# for (f in feature.names) {
#         if (class(train_data[[f]])=="factor") {
#                 levels <- unique(c(train_data[[f]]))
#                 train_data[[f]] <- factor(train_data[[f]],
#                                      labels=make.names(levels))
#         }
# }

rftune <- train(Attrition~.,data=train_data,method="rf",metric="ROC",prePoc = c("center","scale,"),tuneGrid=mtryGrid,ntree=seq(200,1000,by=50),trControl = tctrl)

rftune
plot(rftune)
plot(varImp(rftune))
rftune$times$everything/60

selectedIndices <- rftune$pred$mtry == 9

g <- ggplot(rftune$pred[selectedIndices, ], aes(m=Yes, d=factor(obs, levels = c("No", "Yes")))) + geom_roc(n.cuts=0) + coord_equal() + style_roc()

plot.roc(rftune$pred$obs[selectedIndices],rftune$pred$Yes[selectedIndices])

g + ggplot2::annotate("text", x=0.75, y=0.25, label=paste("AUC =",round((calc_auc(g))$AUC,4)))




#calculating probabilities
probsTest_rf <- predict(rftune, test_data, type = "prob")

#confusion matrix for threshold=0.5
threshold_50 <- 0.50
pred_50      <- factor( ifelse(probsTest_rf[, "Yes"] > threshold_50, "Yes", "No") )
pred_50      <- relevel(pred_50, "Yes")   # you may or may not need this; I did
cm_50 <-confusionMatrix(pred_50, test_data$Attrition)
cm_50$table
fourfoldplot(cm_50$table)
cm_50$byClass

# determining the best threshold
result.roc <- roc(test_data$Attrition, probsTest_rf$Yes)
AUC_value <- as.numeric(result.roc$auc)
print(AUC_value)
# Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy


#confusion matrix for the best threshold
best_threshold <- 0.191
pred_best      <- factor(ifelse(probsTest_rf[, "Yes"] > best_threshold, "Yes", "No") )
#pred      <- relevel(pred, "Yes")   # not required
cm_best <-confusionMatrix(pred_best, test_data$Attrition)
cm_best$table
fourfoldplot(cm_best$table)
cm_50$byClass
perf <- cm_best$byClass


## Logistic regression
tctrl <- trainControl(method = "cv",number = 10,classProbs = T)
ctrl <- trainControl(summaryFunction = twoClassSummary,method = "cv",number = 10,classProbs = TRUE,savePredictions = T)

logReg1 <- train(Attrition~.,data=train_data,method = "glm",metric = "ROC",preProc = c("center","scale"),trControl = ctrl)
confusionMatrix(logReg1)

confusionMatrix(table(predict(logReg1, type="prob")[,"Yes"] >= 0.4,
                      train_data$Attrition == "Yes"),positive = "TRUE")



plot(varImp(logReg1))





#calculating probabilities
probsTest <- predict(logReg1, test_data, type = "prob")

#confusion matrix for threshold=0.5
threshold_50 <- 0.50

pred_50      <- factor(ifelse(probsTest[, "Yes"] > threshold_50, "Yes", "No") )
pred_50      <- relevel(pred_50, "Yes")   # you may or may not need this; I did
cm_50 <-confusionMatrix(pred_50, test_data$Attrition,positive = "Yes")
cm_50$table
fourfoldplot(cm_50$table)
df <-as.data.frame(cm_50$byClass)
colnames(df)<-"Value"
df$perf_Measures <- rownames(df)
df <- df[,c(2,1)]


# determining the best threshold
result.roc <- roc(test_data$Attrition, probsTest$Yes)

AUC_value <- as.numeric(result.roc$auc)
print(AUC_value)
# Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy


#confusion matrix for the best threshold


best_threshold <- 0.169
pred_best      <- factor( ifelse(probsTest[, "Yes"] > best_threshold, "Yes", "No") )
pred_best      <- relevel(pred_best, "Yes")   # not required
test_data$Attrition<- relevel(test_data$Attrition,"No")
cm_best <-confusionMatrix(pred_best, test_data$Attrition,positive = "Yes")
cm_best$table
fourfoldplot(cm_best$table)
cm_best$byClass
cm_best$overall



# SVM

library(caret)
set.seed(100)
tctrl <- trainControl(method = "cv",number = 10,classProbs = T,savePred=T,summaryFunction=twoClassSummary)
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3,classProbs = T,savePred=T,summaryFunction=twoClassSummary)
svm_tune <- expand.grid(.)
grid <- expand.grid(C = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5),sigma=seq(0,1,by=0.01))
svm_linear <- train(Attrition~.,data=train_data,method = "svmLinear",metric="ROC",preProc = c("center","scale"),trControl = trctrl,tuneLength=10,tuneGrid=grid)
svm_radial <- train(Attrition~.,data=train_data,method = "svmRadial",metric="ROC",preProc = c("center","scale"),trControl = trctrl,tuneGrid=grid)
plot(svm1,roc)

probsTest_svm <- predict(svm_linear, test_data, type = "prob")

# Draw ROC curve.
result.roc.svm <- roc(test_data$Attrition, probsTest_svm$Yes)
plot(result.roc.svm, print.thres="best", print.thres.best.method="closest.topleft")
result.coords.svm <- coords(result.roc.svm, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords.svm)
AUC_svm <- as.numeric(result.roc.svm$auc)
print(AUC_svm)

#confusion matrix for threshold=0.5
threshold_50_svm <- 0.50
pred_50_svm      <- factor( ifelse(probsTest_svm[, "Yes"] > threshold_50_svm, "Yes", "No") )
#pred      <- relevel(pred, "Yes")   # you may or may not need this; I did
cm_50_svm <-confusionMatrix(pred_50_svm,test_data$Attrition)
plot(cm_50_svm$table)
fourfoldplot(cm_50_svm$table)
cm_50_svm$byClass

# determining the best threshold

# Draw ROC curve.
result.roc.svm <- roc(test_data$Attrition, probsTest_svm$Yes)
plot(result.roc.svm, print.thres="best", print.thres.best.method="closest.topleft")
result.coords <- coords(result.roc.svm, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy


#confusion matrix for the best threshold
best_threshold_svm <- 0.1686060
pred_best_svm      <- factor( ifelse(probsTest_svm[, "Yes"] > best_threshold_svm, "Yes", "No") )
#pred      <- relevel(pred, "Yes")   # not required
cm_best_svm <-confusionMatrix(pred_best_svm, test_data$Attrition)
cm_best_svm$table
fourfoldplot(cm_best_svm$table)
cm_best_svm$byClass


#Boosting

fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 3,summaryFunction = twoClassSummary,classProbs = TRUE)
set.seed(825)
gbmGrid <-  expand.grid(interaction.depth = c(1:6), n.trees = (1:30)*50, shrinkage = c(0.01,0.1),n.minobsinnode = c(10,15,20))
gbmFit1 <- train(Attrition ~ ., data = train_data, method = "gbm", trControl = fitControl,verbose = FALSE,tuneGrid = gbmGrid,metric="ROC")
gbmFit1

probsTest_gbm <- predict(gbmFit1, test_data, type = "prob")
# Draw ROC curve.
roc.gbm <- roc(test_data$Attrition,probsTest_gbm$No)
plot(roc.gbm, print.thres="best", print.thres.best.method="closest.topleft")
coords.gbm <- coords(roc.gbm, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
coords.gbm
auc_gbm <- as.numeric(roc.gbm$auc)
auc_gbm

#confusion matrix for the best threshold
best_threshold_gbm <- 0.8350227
pred_best_gbm      <- factor( ifelse(probsTest_gbm[,"No"] > best_threshold_gbm, "No", "Yes") )
#pred      <- relevel(pred, "Yes")   # not required
cm_best_gbm <-confusionMatrix(pred_best_gbm, test_data$Attrition,positive = "No")
cm_best_gbm$table
fourfoldplot(cm_best_gbm$table)
cm_best_gbm$byClass
cm_best_gbm$overall

plot(varImp(gbmFit1))
