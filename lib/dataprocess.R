#install rhdf5 package
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

#load library
library("rhdf5")


######load track id
id<-read.table("common_id.txt",col.names="track_id")
id$track_id<-as.character(id$track_id)
id$first<-substr(id$track_id,3,3)
id$second<-substr(id$track_id,4,4)
id$third<-substr(id$track_id,5,5)

######load music features
##song feature
#songs<-matrix(1,nrow=2350,ncol=31)
#songs<-data.frame(songs)
songs<-data.frame(matrix(0,2350,1))
pitch<-data.frame(matrix(0,2350,12))
#pitch_sd<-data.frame(matrix(0,2350,12))
timbre<-data.frame(matrix(0,2350,12))
#timbre_sd<-data.frame(matrix(0,2350,12))
#for (i in 1:2350){

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
#songs$segment_loudness_max_sd[i]<-sd(file$segments_loudness_max)
songs$segment_loudness_start_mean[i]<-mean(file$segments_loudness_start)
#songs$segment_loudness_start_sd[i]<-sd(file$segments_loudness_start)

pitch[i,]<-apply(file$segments_pitches,1,median)
#pitch_sd[i,]<-apply(file$segments_pitches,1,sd)
#pitch$segment_pitch_median_mean[i]<-mean(apply(file$segments_pitches,2,median))
#pitch$segment_pitch_sd_mean[i]<-mean(apply(file$segments_pitches,2,sd))
timbre[i,]<-apply(file$segments_timbre,1,median)
#timbre_sd[i,]<-apply(file$segments_timbre,1,sd)

print(i)}

songs<-songs[,-1]

#####delete useless obs#####
songs<-songs[-c(1325,1375,1658,1705),]
pitch<-pitch[-c(1325,1375,1658,1705),]
pitch_sd<-pitch_sd[-c(1325,1375,1658,1705),]
timbre<-timbre[-c(1325,1375,1658,1705),]
timbre_sd<-timbre_sd[-c(1325,1375,1658,1705),]

###########

songsfeature<-cbind(songs,pitch,timbre)

setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
save(songsfeature,file="songsfeature.RData")
save(songs,file="songs.RData")
save(pitch,file="pitch.RData")
#save(pitch_sd,file="pitch_sd.RData")
save(timbre,file="timbre.RData")
#save(timbre_sd,file="timbre_sd.RData")




#######lyrics processing
######load lyrics
setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
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

###label topic

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

t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
t2 - t1  # about 24 minutes on laptop


topics<-fit$topics
doc_sum<-fit$document_sum

######doc_sum summary#######
doc_assig_total<-colSums(doc_sum)

####first rank###
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

####topic rank summary#####

topics_rank<-topics

for (i in 1:K){
topics_rank[i,]<-rank(-topics[i,])
print(i)
}



######save topic dataframe#####

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
