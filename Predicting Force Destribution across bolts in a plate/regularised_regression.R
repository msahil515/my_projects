library(glmnet)
predictors<-read.csv("_predictors.csv")
responses<-read.csv("_responses.csv")
predictors<-predictors[,-1]
responses<-responses[,-1]
responses<-responses[,1:104]
sam <- sample(1:nrow(predictors),floor(0.8*nrow(predictors)))
train.x <- as.matrix(predictors[sam,])
train.y <- as.matrix(responses[sam,"r4c7"])
test.y <- as.matrix(predictors[-sam,"r4c7"])
test.x <- as.matrix(responses[-sam,])
fit.ordinary <- lm(train.y ~ train.x)
hist(length(fit.ordinary$coefficients))
fit.lasso    <- glmnet(train.x, train.y, family="gaussian", alpha=1)
dev.new(); hist(fit.lasso$df)
fit.ridge    <- glmnet(train.x, train.y, family="gaussian", alpha=0)
dev.new(); hist(fit.ridge$df)
fit.elnet    <- glmnet(train.x, train.y, family="gaussian", alpha=.5)
dev.new(); hist(fit.elnet$df,breaks=c(10:200:10))
#
# 10-fold cross validation for each alfa (ranging between Ridge Regression and Lasso across the Elastic Net)
#
for (i in 0:10) {
        assign(paste("fit", i, sep=""), cv.glmnet(train.x, train.y, type.measure="mse", alpha=i/10,family="gaussian"))
}
#
# Plot solution paths (to choose regularized models within each "family")
#
dev.new()
par(mfrow=c(1,2))
plot(fit10)
grid()
plot(fit.lasso, xvar="lambda")
#
dev.new()
par(mfrow=c(1,2))
plot(fit0)
grid()
plot(fit.ridge, xvar="lambda")
#
dev.new()
par(mfrow=c(1,2))
plot(fit5)
grid()
plot(fit.elnet, xvar="lambda")
#
# Calculating MSE for each ALPHA
#
yhat0 <- predict(fit0, s=fit0$lambda.1se, newx=test.x)
yhat1 <- predict(fit1, s=fit1$lambda.1se, newx=test.x)
yhat2 <- predict(fit2, s=fit2$lambda.1se, newx=test.x)
yhat3 <- predict(fit3, s=fit3$lambda.1se, newx=test.x)
yhat4 <- predict(fit4, s=fit4$lambda.1se, newx=test.x)
yhat5 <- predict(fit5, s=fit5$lambda.1se, newx=test.x)
yhat6 <- predict(fit6, s=fit6$lambda.1se, newx=test.x)
yhat7 <- predict(fit7, s=fit7$lambda.1se, newx=test.x)
yhat8 <- predict(fit8, s=fit8$lambda.1se, newx=test.x)
yhat9 <- predict(fit9, s=fit9$lambda.1se, newx=test.x)
yhat10 <- predict(fit10, s=fit10$lambda.1se, newx=test.x)

mse0 <- mean((test.y - yhat0)^2)
mse1 <- mean((test.y - yhat1)^2)
mse2 <- mean((test.y - yhat2)^2)
mse3 <- mean((test.y - yhat3)^2)
mse4 <- mean((test.y - yhat4)^2)
mse5 <- mean((test.y - yhat5)^2)
mse6 <- mean((test.y - yhat6)^2)
mse7 <- mean((test.y - yhat7)^2)
mse8 <- mean((test.y - yhat8)^2)
mse9 <- mean((test.y - yhat9)^2)
mse10 <- mean((test.y - yhat10)^2)


coeff_ridge<-coef(fit0,s='lambda.min')
coeff_elastic<-coef(fit5,s='lambda.min')
coeff_lasso<-coef(fit10,s='lambda.min')
sig_coeff_ridge<-as.matrix(coeff_ridge[which(abs(coeff_ridge)>=.01),])
sig_coeff_elastic<-as.matrix(coeff_elastic[which(abs(coeff_elastic)>=.01),])
sig_coeff_lasso<-as.matrix(coeff_lasso[which(abs(coeff_lasso)>=.01),])


viz_grid<-function(coeff){
        
        
        a = as.data.frame(expand.grid(1:13,1:8))
        colnames(a) = c('x', 'y')
        b<-strsplit(row.names(coeff),"[a-z]+")
        matrix<-as.matrix(coeff)
        x.c<-rep(0,length(matrix))
        y.c<-rep(0,length(matrix))
        for(i in 2:length(matrix)){
                x.c[i]<-as.numeric(b[[i]][3])
                y.c[i]<-as.numeric(b[[i]][2])
        }
        x.c<-x.c[-1]
        y.c<-y.c[-1]
        sigbolts<-as.data.frame(cbind(x.c,y.c)) #contains the coordinates
        a$indicator<-0
        
        for (i in 1:length(sigbolts[,2])){
                
                for(j in 1:length(a[,2])){
                        
                        if(sigbolts[i,1]==a[j,1] & sigbolts[i,2]==a[j,2])
                                a[j,3]<-1
                }
        }
        a[46,3] <-2
        ggplot() + geom_point(data = a, aes(x = x, y = y,color=factor(indicator))) + geom_point() + scale_y_continuous(trans='reverse') + scale_color_discrete(label=c("insignificant","significant","failure"))
}

viz_grid(sig_coeff_ridge)
viz_grid(sig_coeff_elastic)
viz_grid(sig_coeff_lasso)