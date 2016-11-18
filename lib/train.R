#######load and process music feature dataset##########
library("rhdf5")
######load track id
id<-read.table("common_id.txt",col.names="track_id")
id$track_id<-as.character(id$track_id)
id$first<-substr(id$track_id,3,3)
id$second<-substr(id$track_id,4,4)
id$third<-substr(id$track_id,5,5)
######load music features
songs<-data.frame(matrix(0,2350,1))
pitch<-data.frame(matrix(0,2350,12))
timbre<-data.frame(matrix(0,2350,12))
for (i in c(1:1324,1326:1374,1376:1657,1659:1704,1706:2350)){
  setwd(paste("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data/data/",as.character(id$first[i]),'/',id$second[i],'/',id$third[i],sep=""))
  file<-h5read(file=paste(id[i,1],".h5",sep=""),name="analysis")
  file2<-h5read(file=paste(id[i,1],".h5",sep=""),name="metadata")
  songs$bar_number[i]<-length(file$bars_confidence)
  songs$beats_number[i]<-length(file$beats_confidence)
  songs$duration<-file$beats_start[length(file$beats_start)]+diff(file$beats_start)[length(file$beats_start)-1]
  songs$tempo<-songs$beats_number/(songs$duration/60)
  songs$section_number[i]<-length(file$sections_confidence)
  songs$segment_number[i]<-length(file$segments_confidence)
  songs$segment_loudness_max_mean[i]<-mean(file$segments_loudness_max)
  songs$segment_loudness_start_mean[i]<-mean(file$segments_loudness_start)
  pitch[i,]<-apply(file$segments_pitches,1,median)
  timbre[i,]<-apply(file$segments_timbre,1,median)
  print(i)}
songs<-songs[,-1]
#####delete useless obs
songs<-songs[-c(1325,1375,1658,1705),]
pitch<-pitch[-c(1325,1375,1658,1705),]
timbre<-timbre[-c(1325,1375,1658,1705),]
songsfeature<-cbind(songs,pitch,timbre)

setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
save(songsfeature,file="songsfeature.RData")


#######train topic model##############
####load lyrics dataset
setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
load("lyr.Rdata")
col<-c(1,2,3,6:30)
lyr<-lyr[,-col]
lyr<-lyr[-c(1325,1375,1658,1705),]
lyrlabel<-lyr
lyrlabel[lyrlabel>0]<-1

####divide lyrics vocab into three sets: 
####most frequent(first), less frequent(middle), least frequent(last)
freq<-sort(colSums(lyrlabel),decreasing=T)
freq_l<-sort(colSums(lyrlabel))
last<-lyr[,names(head(freq_l,600))]
first<-lyr[,names(head(freq,20))]
middle<-lyr[-match(c(names(head(freq_l,600)),names(head(freq,20))),colnames(lyr))]
middlelabel<-middle
middlelabel[middlelabel>0]<-1
vocab<-colnames(middle)

####train topic model with the "middle" set
documents<-list()
for (i in 1:2346){
  names<-colnames(middlelabel[i,][which(middlelabel[i,]==1)])
  dataframe<-rbind(as.integer(match(names,vocab)-1),as.integer(rep(1,length(names))))
  list<-list(dataframe)
  documents[i]<-list
  print(i)
}
K <- 20
G <- 5000
alpha <- 0.02
eta <- 0.02
library(lda)
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)

topics<-fit$topics
doc_sum<-fit$document_sum

####process topic model result
doc_assig_total<-colSums(doc_sum)
doc_assig<-rep(1,2346)
doc_assig2<-rep(1,2346)
doc_assig3<-rep(1,2346)
doc_assig4<-rep(1,2346)
doc_assig_w<-rep(1,2346)
doc_assig2_w<-rep(1,2346)
doc_assig3_w<-rep(1,2346)
doc_assig4_w<-rep(1,2346)
doc_assig_n<-rep(1,2346)
doc_assig2_n<-rep(1,2346)
doc_assig3_n<-rep(1,2346)
doc_assig4_n<-rep(1,2346)
for (i in 1:2346){
  doc_assig[i]<-which(doc_sum[,i]==sort(doc_sum[,i],decreasing = T)[1])[1]
  doc_assig2[i]<-which(doc_sum[,i]==sort(doc_sum[,i],decreasing = T)[2])[1]
  doc_assig3[i]<-which(doc_sum[,i]==sort(doc_sum[,i],decreasing = T)[3])[1]
  doc_assig4[i]<-which(doc_sum[,i]==sort(doc_sum[,i],decreasing = T)[4])[1]
  doc_assig_w[i]<-(sort(doc_sum[,i],decreasing = T)[1])/doc_assig_total[i]
  doc_assig2_w[i]<-(sort(doc_sum[,i],decreasing = T)[2])/doc_assig_total[i]
  doc_assig3_w[i]<-(sort(doc_sum[,i],decreasing = T)[3])/doc_assig_total[i]
  doc_assig4_w[i]<-(sort(doc_sum[,i],decreasing = T)[4])/doc_assig_total[i]
  doc_assig_n[i]<-round(doc_assig_w[i],1)*10
  doc_assig2_n[i]<-round(doc_assig2_w[i],1)*10
  doc_assig3_n[i]<-round(doc_assig3_w[i],1)*10
  doc_assig4_n[i]<-round(doc_assig4_w[i],1)*10
  print(i)
}
topics_rank<-topics
for (i in 1:K){
  topics_rank[i,]<-rank(-topics[i,])
  print(i)
}
##save necessary data for model training/testing
save(topics_rank,file="topics_rank.RData")
save(topics,file="topics.RData")
save(doc_sum,file="doc_sum.RData")
save(doc_assig,file="doc_assign.RData")
save(doc_assig2,file="doc_assign2.RData")
save(doc_assig3,file="doc_assign3.RData")
save(doc_assig4,file="doc_assign4.RData")
save(doc_assig_n,file="doc_assign_n.RData")
save(doc_assig2_n,file="doc_assign2_n.RData")
save(doc_assig3_n,file="doc_assign3_n.RData")
save(doc_assig4_n,file="doc_assign4_n.RData")

#####load necessary data from topic model
load("topics_rank.RData")
load("topics.RData")
load("doc_sum.RData")
load("doc_assign.RData")
load("doc_assign2.RData")
load("doc_assign3.RData")
load("doc_assign4.RData")
load("doc_assign_n.RData")
load("doc_assign2_n.RData")
load("doc_assign3_n.RData")
load("doc_assign4_n.RData")

##########train classification model##########
####load feature#####
load("songsfeature.Rdata")
####load lyrics#####
load("lyr.Rdata")
col<-c(1,2,3,6:30)
lyr<-lyr[,-col]
lyr<-lyr[-c(1325,1375,1658,1705),]
lyrlabel<-lyr
lyrlabel[lyrlabel>0]<-1
freq<-sort(colSums(lyrlabel),decreasing=T)
freq_l<-sort(colSums(lyrlabel))
last<-lyr[,names(head(freq_l,600))]
first<-lyr[,names(head(freq,20))]
middle<-lyr[-match(c(names(head(freq_l,600)),names(head(freq,20))),colnames(lyr))]
middlelabel<-middle
middlelabel[middlelabel>0]<-1
vocab<-colnames(middle)
firstname<-colnames(first)
middlename<-colnames(middle)
lastname<-colnames(last)
save(firstname,file="firstname.RData")
save(middlename,file="middlename.RData")
save(lastname,file="lastname.RData")


####set training set and test set### use 100% data for final model
train<-sample(1:2346,2346,replace=FALSE)
input_train<-songsfeature[train,]
label_train<-lyrlabel[train,]
####Train with  TOPIC Distribution randomforest Method
library(randomForest)
doc_assig_train<-doc_assig[train]
doc_assig2_train<-doc_assig2[train]
doc_assig3_train<-doc_assig3[train]
doc_assig4_train<-doc_assig4[train]
doc_assig_n_train<-doc_assig_n[train]
doc_assig2_n_train<-doc_assig2_n[train]
doc_assig3_n_train<-doc_assig3_n[train]
doc_assig4_n_train<-doc_assig4_n[train]
training<-rep(1,2346)
for (i in 1:2346){
topicid<-c(rep(doc_assig_train[i],doc_assig_n_train[i]),
           rep(doc_assig2_train[i],doc_assig2_n_train[i]),
           rep(doc_assig3_train[i],doc_assig3_n_train[i]),
           rep(doc_assig4_train[i],doc_assig4_n_train[i]))
n_row<-sum(doc_assig_n_train[i],doc_assig2_n_train[i],doc_assig3_n_train[i],doc_assig4_n_train[i])
feature<-rep(input_train[i,],n_row)
subtraining<-cbind(topicid,feature)
training<-rbind(training,subtraining)
print(i)
}
training<-training[-1,]
training[,1]<-as.factor(training[,1])
fit.rf<-randomForest(topicid~.,training,type="classification",importance=TRUE,mtry=8)
save(fit.rf,file="model.RData")