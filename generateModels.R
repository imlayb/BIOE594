#!usr/bin/env Rscript
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
source("eigenfaces.R")
library(caret)
dat<-importFaceMatrix()
meta_TR<-importMetaMatrix("faces/faceDR")
meta_T<-importMetaMatrix("faces/faceDS")
meta_TR<-meta_TR[meta_TR$n %in% rownames(dat),] # Remove any examples from metadata that are not in the data.
meta_T<-meta_T[meta_T$n %in% rownames(dat),]
dat_TR<-dat[meta_TR$n,]
dat_T<-dat[meta_T$n,]
rm(dat)
meta_TR$age<-as.factor(meta_TR$age);meta_TR$sex<-as.factor(meta_TR$sex);meta_TR$race<-as.factor(meta_TR$race);meta_TR$face<-as.factor(meta_TR$face)
meta_T$age<-as.factor(meta_T$age);meta_T$sex<-as.factor(meta_T$sex);meta_T$race<-as.factor(meta_T$race);meta_T$face<-as.factor(meta_T$face)
nzv<-nearZeroVar(dat_TR)
NZV_dat_TR<-dat_TR[,-nzv]
##
sel_class<-"age"
training<-data.frame(Class=meta_TR[[sel_class]],NZV_dat_TR) ### Only thing needed to change selected class
##
trainIndex <- createDataPartition(training$Class, p = .1, 
                                  list = FALSE, 
                                  times = 1)
training_training<-training[trainIndex,]
training_test<-training[-trainIndex,]
preProcValues <- preProcess(training, method = c("center", "scale"))
trainTransformed <- predict(preProcValues, training_training)
testTransformed <- predict(preProcValues, training_test)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats = 10)
registerDoSEQ() # necessary due to bug
#gbmFit1 <- train(Class ~ ., data = trainTransformed, 
#                 method = "gbm", 
#                 trControl = fitControl,
#                 verbose = FALSE)
#preds<-predict(gbmFit1,testTransformed)

trctrl<-trainControl(method="cv",number=5)
tune_grid <- expand.grid(nrounds=c(100,200,300,400), 
                         max_depth = c(3:7),
                         eta = c(0.05, 1),
                         gamma = c(0.01),
                         colsample_bytree = c(0.75),
                         subsample = c(0.50),
                         min_child_weight = c(0))
rf_fit <- train(Class ~., data = trainTransformed, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 10)

save.image(file.path("models",paste0(sel_class,"_models.Rdata")))