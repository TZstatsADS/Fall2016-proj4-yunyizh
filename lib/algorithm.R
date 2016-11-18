setwd("~/Documents/ads/proj4/Fall2016-proj4-yunyizh/data")
#####load topic#####

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


####set training set and test set###


train<-sample(1:2346,2346,replace=FALSE)
input_train<-songsfeature[train,]
input_test<-songsfeature[-train,]
label_train<-lyrlabel[train,]
label_test<-lyrlabel[-train,]



############glm##############

prob<-matrix(1,470,4973)
result<-data.frame(prob)

for (i in 1:4973){
label<-as.factor(label_train[,i])
train<-data.frame(cbind(label,input_train))
fit<-glm(label~.,train,family="binomial")
label<-as.factor(label_test[,i])
test<-data.frame(cbind(label,input_test))
prob<-predict(fit,test,type="response")
result[,i]<-prob
print(i)
}

rank<-result

for (j in 1:470) {
rank[j,]<-rank(-result[j,])
print(j)}

meanrank_base_glm<-rep(1,470)
 
for (z in 1:470){
testlabel<-label_test
col=which(testlabel[z,]==1)
ranksum=sum(rank[z,col])
meanrank_base_glm[z]<-ranksum/length(col)
print(z)
}
meanrank_base_glm
cbind(mean(meanrank_base_glm),median(meanrank_base_glm),sd(meanrank_base_glm),max(meanrank_base_glm),min(meanrank_base_glm))


######### TOPIC DIST randomforest ########
###train
library(randomForest)
doc_assig_train<-doc_assig[train]
doc_assig2_train<-doc_assig2[train]
doc_assig3_train<-doc_assig3[train]
doc_assig4_train<-doc_assig4[train]

doc_assig_n_train<-doc_assig_n[train]
doc_assig2_n_train<-doc_assig2_n[train]
doc_assig3_n_train<-doc_assig3_n[train]
doc_assig4_n_train<-doc_assig4_n[train]

i=1
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

###test
topicid<-as.factor(doc_assig[-train])
testing<-data.frame(cbind(topicid,input_test))
resultprob<-predict(fit.rf,testing,type="prob")
result<-predict(fit.rf,testing)
rank<-middle
rank<-rank[1:470,]
for (i in 1:470){
  rank1prob<-sort(resultprob[i,],decreasing=T)[1]
  rank2prob<-sort(resultprob[i,],decreasing=T)[2]
  rank3prob<-sort(resultprob[i,],decreasing=T)[3]
  rank4prob<-sort(resultprob[i,],decreasing=T)[4]
  rank5prob<-sort(resultprob[i,],decreasing=T)[5]
# rank5prob<-0
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
  #ranksum<-rank1+rank2
  #ranksum<-(rank1weight*rank1)+(rank2weight*rank2)
  ranksum<-(rank1weight*rank1)+(rank2weight*rank2)+(rank3weight*rank3)+(rank4weight*rank4)+(rank5weight*rank5)
  rank[i,]<-rank(ranksum)+20
  print(i)
}

finalresult<-lyr
finalresult<-finalresult[1:470,]
for (i in 1:470){
  finalresult[i,colnames(first)]<-1:20
  finalresult[i,colnames(last)]<-4673.5
  finalresult[i,colnames(middle)]<-rank[i,colnames(middle)]
  print(i)
}


#meanrank_rf_3<-rep(1,470)
#meanrank_rf_4<-rep(1,470)
#meanrank_rf_5_dist<-rep(1,470)
meanrank_rf_5_dist4<-rep(1,470)
#meanrank_rf_4_dist4<-rep(1,470)

#meanrank_rf_1_dist<-rep(1,470)

for (i in 1:470){
  col<-which(label_test[i,]==1)
  meanrank_rf_5_dist4[i]<-sum(finalresult[i,col])/length(col)
  print(i)
}


#cbind(mean(meanrank_rf_5_dist),median(meanrank_rf_5_dist),sd(meanrank_rf_5_dist),max(meanrank_rf_5_dist),min(meanrank_rf_5_dist))
cbind(mean(meanrank_rf_5_dist4),median(meanrank_rf_5_dist4),sd(meanrank_rf_5_dist4),max(meanrank_rf_5_dist4),min(meanrank_rf_5_dist4))

#cbind(mean(meanrank_rf_1_dist),median(meanrank_rf_1_dist),sd(meanrank_rf_1_dist),max(meanrank_rf_1_dist),min(meanrank_rf_1_dist))



##################svm##########

library(caret)

topicid_train<-as.factor(doc_assig[train])
topicid_test<-as.factor(doc_assig[-train])
training<-data.frame(cbind(topicid_train,input_train))
testing<-data.frame(cbind(topicid_test,input_test))


ctr <- trainControl(method='cv',number=3)
grid <- data.frame(C=c(0.01, 0.1,1))
svm.tune.poly <- train(topicid_train~.,training,
                  method='svmPoly',
                  trControl=ctr)
result_svm <- predict(svm.tune.poly, testing)

rank<-middle
rank<-rank[1:470,]
for (i in 1:470){
  rank[i,]<-topics_rank[result[i],]
  #ranksum<-rank1+rank2
  #ranksum<-(rank1weight*rank1)+(rank2weight*rank2)
  #rank[i,]<-rank(ranksum)+20
  print(i)
}

finalresult<-lyr
finalresult<-finalresult[1:470,]
for (i in 1:470){
  finalresult[i,colnames(first)]<-1:20
  print(i)
}
for (i in 1:470){
  finalresult[i,colnames(last)]<-4798.5
  print(i)
}
i=1
for (i in 1:470){
  finalresult[i,colnames(middle)]<-rank[i,colnames(middle)]
}

meanrank_svm<-rep(1,470)

for (i in 1:470){
  col<-which(label_test[i,]==1)
  meanrank_svm[i]<-sum(finalresult[i,col])/length(col)
  print(i)
}

cbind(mean(meanrank_svm),median(meanrank_svm),sd(meanrank_svm),max(meanrank_svm),min(meanrank_svm))

#######topic#######naive bayes
library(e1071)

topicid_train<-as.factor(doc_assig[train])
topicid_test<-as.factor(doc_assig[-train])
training<-data.frame(cbind(topicid_train,input_train))
testing<-data.frame(cbind(topicid_test,input_test))
testing<-as.matrix(testing)
rownames(testing)<-NULL
testing<-data.frame(testing)
training<-as.matrix(training)
rownames(training)<-NULL
training<-data.frame(rbind(as.matrix(training),as.matrix(training)))
fit.nb<-naiveBayes(topicid_train~.,training)
resultprob<-predict(fit.nb,testing,type="raw")
result<-predict(fit.nb,testing,type="class")
rank<-middle
rank<-rank[1:470,]
for (i in 1:470){
  rank1prob<-sort(resultprob[i,],decreasing=T)[1]
  rank2prob<-sort(resultprob[i,],decreasing=T)[2]
  rank3prob<-sort(resultprob[i,],decreasing=T)[3]
  rank4prob<-sort(resultprob[i,],decreasing=T)[4]
  rank5prob<-sort(resultprob[i,],decreasing=T)[5]
  rank1weight<-rank1prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank2weight<-rank2prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank3weight<-rank3prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank4weight<-rank4prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank5weight<-rank4prob/(rank1prob+rank2prob+rank3prob+rank4prob+rank5prob)
  rank1<-topics_rank[result[i],]
  rank2<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[2])),]
  rank3<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[3])),]
  rank4<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[4])),]
  rank5<-topics_rank[as.numeric(names(sort(resultprob[i,],decreasing=T)[5])),]
  #ranksum<-rank1+rank2
  #ranksum<-(rank1weight*rank1)+(rank2weight*rank2)
  ranksum<-(rank1weight*rank1)+(rank2weight*rank2)+(rank3weight*rank3)+(rank4weight*rank4)+(rank5weight*rank5)
  rank[i,]<-rank(ranksum)+20
  print(i)
}

finalresult<-lyr
finalresult<-finalresult[1:470,]
for (i in 1:470){
  finalresult[i,colnames(first)]<-1:20
  print(i)
}
for (i in 1:470){
  finalresult[i,colnames(last)]<-4798.5
  print(i)
}
i=1
for (i in 1:470){
  finalresult[i,colnames(middle)]<-rank[i,colnames(middle)]
print(i)
  }

#meanrank_rf_3<-rep(1,470)
#meanrank_rf_4<-rep(1,470)
meanrank_nb<-rep(1,470)

for (i in 1:470){
  col<-which(label_test[i,]==1)
  meanrank_nb[i]<-sum(finalresult[i,col])/length(col)
  print(i)
}

cbind(mean(meanrank_nb),median(meanrank_nb),sd(meanrank_nb),max(meanrank_nb),min(meanrank_nb))

cbind(mean(meanrank_rf_3),median(meanrank_rf_3),sd(meanrank_rf_3),max(meanrank_rf_3),min(meanrank_rf_3))
cbind(mean(meanrank_rf_4),median(meanrank_rf_4),sd(meanrank_rf_4),max(meanrank_rf_4),min(meanrank_rf_4))
cbind(mean(meanrank_rf_5),median(meanrank_rf_5),sd(meanrank_rf_5),max(meanrank_rf_5),min(meanrank_rf_5))
