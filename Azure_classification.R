#!/usr/bin/env Rscript
## This script simply polls the Cognitive Services API in Azure and collects responses into a list, which it saves as an Robject.
library(httr)
library(doSNOW)
library(foreach)
library(jsonlite)
library(stringr)
source("eigenfaces.R")
### Load all images as vectors / Specific to Eigenfaces use case ------------------------------------------------------
dat<-importFaceMatrix()
meta_TR<-importMetaMatrix("faces/faceDR")
meta_T<-importMetaMatrix("faces/faceDS")
meta_TR<-meta_TR[meta_TR$n %in% rownames(dat),] # Remove any examples from metadata that are not in the data.
meta_T<-meta_T[meta_T$n %in% rownames(dat),]
meta<-rbind(meta_TR,meta_T)
rownames(meta)<-meta$n
meta<-meta[rownames(dat),]

### Convert all images to jpeg / Specific to Eigenfaces use case ------------------------------------
outputImages<-function(dat,color_scale=gray.colors(
  256,
  start = 0.3,
  end = 0.9,
  gamma = 2.2,
  alpha = NULL
)) {
  cl <- makeCluster(20)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = nrow(dat), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  img_mats<-foreach(v=iter(dat,by='row'),n=rownames(dat),.options.snow = opts) %dopar% {
    v<-as.vector(v)
    v<-matrix(v,nrow=sqrt(length(v)))
    jpeg(file.path("jpeg_images",paste0(n,".jpeg")))
    par(mar = c(0,0,0,0))
    image(
      v[, nrow(v):1],
      col = color_scale,
      axes = FALSE
    )
    dev.off()
  }
  close(pb)
  stopCluster(cl)
}
dir.create(path = "jpeg_images",showWarnings = F)
outputImages(dat)

### Prepare API / General to any use case ---------------------------------

keys<-fromJSON("api-keys/keys") # JSON with name,key1,key2 from AZURE
AZURE_URL="https://centralus.api.cognitive.microsoft.com/face/v1.0/detect"
QUERY="?returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,smile,facialHair,glasses,emotion,accessories&recognitionModel=recognition_02"
F_URL<-paste0(AZURE_URL,QUERY)

### POST to API and collect results / General to any use case -----------------------------------------------

getAzure<-function(image_dir,delay_interval=3.1,image_type="jpeg",n=F,debug=F) {
  file_list<-Sys.glob(file.path(image_dir,paste0("*.",image_type)))
  if(n) {file_list<-file_list[1:n]}
  image_titles<-str_remove(file_list,pattern = paste0(image_dir,"/")) %>% str_remove(pattern=paste0("\\.",image_type))
  results_list<-foreach(FILE=file_list,NAME=image_titles,.packages = "httr") %do% {
    Sys.sleep(delay_interval)
    pic<-upload_file(FILE)
    response<-POST(url=F_URL, body=pic, add_headers(.headers =
                                                       c('Content-Type'='application/octet-stream', 'Ocp-Apim-Subscription-Key'=as.character(keys["key1"]))))
    result <- content(response)
    if(debug){print(paste0("Obtained: ",FILE))}
    return(result)
  }
  names(results_list)<-image_titles
  return(results_list)
}
azure_list<-getAzure("jpeg_images",debug=T)
save(list=c("azure_list"),file = file.path("models","Azure_results.Rdata"))