#!/usr/bin/env Rscript

# generateModels function -------------------------------------------------


generateModels<-function(dat,meta_df,nThread=4,controls=list(),methods=c()) {
  cl <- makeCluster(nThread)
  registerDoSNOW(cl)
  classes<-names(meta_df)
  print(classes)
  models<-foreach(Class=classes,.export = c("controls","methods","dat","meta_df")) %do% {
    training<-data.frame(meta_df[[Class]],dat)
    print(nrow(training))
    print(ncol(training))
    names(training)<-Class
    print(names(training))
    perclassmodels<-foreach(m=methods,ctrl=controls,.export=c("Class","training")) %dopar% {
      caret::train(eval(parse(Class))~.,
                      data=training,
                      method = m, 
                      trControl = fitControl)
    }
    return(perclassmodels)
  }
  return(models)
}

# Main --------------------------------------------------------------------


## prep --------------------------------------------------------------------


source("eigenfaces.R")
library(caret)
library(doSNOW)
dat<-importFaceMatrix()
meta_TR<-importMetaMatrix("faces/faceDR")
meta_TR<-meta_TR[meta_TR$n %in% rownames(dat),] # Remove any examples from metadata that are not in the data.
dat_TR<-dat[meta_TR$n,]
rm(dat)
meta_TR$age<-as.factor(meta_TR$age);meta_TR$sex<-as.factor(meta_TR$sex);meta_TR$race<-as.factor(meta_TR$race);meta_TR$face<-as.factor(meta_TR$face)
nzv<-nearZeroVar(dat_TR, uniqueCut = 5) # This may change
##
sel_class<-"sex"
training<-data.frame(Class=meta_TR[[sel_class]],dat_TR[,-nzv]) ### Only thing needed to change selected class
##
preProcValues <- preProcess(training, method = c("center"))
trainTransformed <- predict(preProcValues, training)

liftCtrl <- trainControl(method = "cv", classProbs = TRUE,
                     summaryFunction = twoClassSummary)

## models ------------------------------------------------------------------
registerDoSEQ()
cl<-makeCluster(1)
registerDoSNOW(cl)
c5 <- train(Class ~ ., data = trainTransformed,
                 method = "C5.0", metric = "ROC",
                 tuneLength = 10,
                 trControl = liftCtrl,
                 control = C50::C5.0Control(earlyStopping = FALSE))
fda<- train(Class ~ ., data = trainTransformed,
                  method = "fda", metric = "ROC",
                  tuneLength = 20,
                  trControl = liftCtrl)
glmBoost_grid = expand.grid(mstop = c(50, 100, 150, 200, 250, 300),
                           prune = c('yes', 'no'))
glmboost<-train(Class~.,data=trainTransformed,
                method="glmboost",metric='ROC',
                trControl=liftCtrl,tuneGrid=glmBoost_grid)
XGB_grid <- expand.grid(nrounds=c(100,200,300,400), 
                         max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))
rf_fit <- train(Class ~., data = trainTransformed, method = "xgbTree",
                trControl=liftCtrl,
                tuneGrid = XGB_grid,
                tuneLength = 10,
                metric='ROC')
#confusionMatrix(data = preds, reference = testTransformed$Class)
save.image(file.path("models",paste0(sel_class,"liftcent_models.Rdata")))
stopCluster(cl)
