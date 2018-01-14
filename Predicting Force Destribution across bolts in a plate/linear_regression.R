predictors <- read.csv("/Users/msahil515/IE 2065-Machine learning/Project/data/_predictors.csv")
predictors <- predictors[-1]
response <- read.csv("/Users/msahil515/IE 2065-Machine learning/Project/data/_responses.csv")
responses <- response[-c(1,106:209)]
sum_zeros <- numeric()
for(i in 1:nrow(predictors)) {
        sum_zeros[i] <- sum(predictors[i,]==0)
}

range(sum_zeros)       
ind_15 <- which(sum_zeros>15)                
ind_zeros <- which(sum_zeros==0)[-1]
ind <- unique(c(ind_15,ind_zeros))
pred <- predictors[-ind,]
resp <- responses[-ind,]


## creating an input matrix of 208 columns with X,Y coordinates of each bolt location
r <- rep(1:8,13)
c <- rep(1:13,each=8)
input <- data.frame()
for(i in 1:nrow(pred)) {
        for (j in 1:104) {
                if (pred[i,j]==0) {
                input[i,2*j-1] <- r[j]
                input[i,2*j] <- c[j]
                } else {
                        input[i,2*j-1] <- 0
                        input[i,2*j] <- 0
                }
                
        }
}
# Splitting the dataset into train and test data
nobs <- floor(0.8*nrow(input))
trainx <- sample(1:nrow(input),nobs)
train_predictors <- input[trainx,]
test_predictors <- input[-trainx,]
train_responses <- resp[trainx,]
test_responses <- resp[-trainx,]

# running linear regression for each location
ctrl<-trainControl(method = 'cv',number = 5)
train_pred <- data.frame(matrix(unlist(train_predictors), nrow=nrow(train_predictors)),stringsAsFactors=FALSE)
train_data <- cbind(train_responses,train_predictors)
#prediction dataframe
pred.matrix <- data.frame(matrix(nrow=nrow(test_predictors),ncol = 104))
for(i in 1:104) {
        colname<- colnames(train_responses[i])
        lm_fit <- train(train_predictors,train_responses[[i]],method="lm",trControl = ctrl, preProc = c("center", "scale"))
        assign(paste("lmfit_",colname,sep=""),lm_fit)
        pred.matrix[i] <- predict(lm_fit, test_predictors)
        
}

colnames(pred.matrix)<- colnames(test_responses)
# tweaking prediction matrix by rounding off the values less than 0.5 to 0.This is done because most force values in failure 
# locations have some value between -0.5 to 1
prediction <- pred.matrix
for (i in 1:nrow(prediction)) {
        for (j in 1:104) {
                if (prediction[i,j]<=0.5) {
                        prediction[i,j] <- 0
                }
                        
        }
}
#RMSE-out-of-sample

# calculating rmse without any changes to prediction data frame

res <- test_responses-pred.matrix
rmse1 <- sapply(res,function (x) sqrt(mean(x^2)))

# calculating rmse after tweaks to prediction data frame
residual <-test_responses-prediction
rmse <- sapply(residual,function (x) sqrt(mean(x^2)))





