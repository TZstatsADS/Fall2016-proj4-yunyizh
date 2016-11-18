setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
load("lyr.Rdata") 
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
load("model.RData")


library("rhdf5")
library("randomForest")
######load music features
##song feature
songs<-data.frame(matrix(0,100,1))
pitch<-data.frame(matrix(0,100,12))
timbre<-data.frame(matrix(0,100,12))
track_id<-rep(0,100)
setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data/TestSongFile100")
doc_list<-dir()
for (i in c(1:100)){
  file<-h5read(file=doc_list[i],name="analysis")
  track_id[i]<-doc_list[i]
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
songsfeature<-cbind(songs,pitch,timbre)

setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
save(songsfeature,file="songsfeaturetest.RData")

topicid<-rep(0,100)
testing<-data.frame(cbind(topicid,songsfeature))

resultprob<-predict(fit.rf,testing,type="prob")
result<-predict(fit.rf,testing)

load("firstname.RData")
load("middlename.RData")
load("lastname.RData")


###test

rank<-lyr[,middlename]
rank<-rank[1:100,]
for (i in 1:100){
  rank1prob<-sort(resultprob[i,],decreasing=T)[1]
  rank2prob<-sort(resultprob[i,],decreasing=T)[2]
  rank3prob<-sort(resultprob[i,],decreasing=T)[3]
  rank4prob<-sort(resultprob[i,],decreasing=T)[4]
  rank5prob<-sort(resultprob[i,],decreasing=T)[5]
  rank1weight<-rank1prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank2weight<-rank2prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank3weight<-rank3prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank4weight<-rank4prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank5weight<-rank5prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank1<-topics_rank[result[i],]
  rank2<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[2])),]
  rank3<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[3])),]
  rank4<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[4])),]
  rank5<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[5])),]
  ranksum<-(rank1weight*rank1)+(rank2weight*rank2)+(rank3weight*rank3)+(rank4weight*rank4)+(rank5weight*rank5)
  rank[i,]<-rank(ranksum)+20
  print(i)
}

finalresult<-lyr
finalresult<-finalresult[1:100,]
i=1
for (i in 1:100){
  finalresult[i,firstname]<-1:20
  finalresult[i,lastname]<-4673.5
  finalresult[i,middlename]<-rank[i,middlename]
  finalresult[i,c(2,3,6:30)]<-4987
  print(i)
}


finalresult[,1]<-track_id

library(xlsx)
write.xlsx(finalresult,file="finalresult.xlsx")




