#!/usr/bin/env Rscript
## This script loads the Robject from Azure_classification
library(httr)
library(dplyr)
library(data.table)
### Loading azure results list ----------------------------
load(file.path("models","Azure_results.Rdata")) # loads azure_list
extractDF<-function(azure_list,name_feature="n") {
  all_images<-names(azure_list)
  results<-lapply(X = azure_list,FUN = unlist)
  null_images<-sapply(results,is.null)
  results<-do.call("bind_rows", results)
  rownames(results)<-all_images[!null_images]
  results[[name_feature]]<-all_images[!null_images]
  return(list(results,all_images[null_images]))
}
extractNullImages<-function(azure_list) {
  all_images<-names(azure_list)
  results<-lapply(X = azure_list,FUN = unlist)
  null_images<-sapply(results,is.null)
  return(all_images[null_images])
}

all_images<-names(azure_list)
results_df<-extractDF(azure_list)
null_images<-results_df[[2]]
results_df<-results_df[[1]]

fwrite(results_df,file = "Azure_Results.tsv",sep = "\t",col.names = T)
fwrite(data.frame(null_images),file = "Failed_Azure_Images.tsv",sep = "\t",row.names = F,col.names = T)