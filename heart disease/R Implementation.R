#Read csv file
library (readr)
heart<-read.csv("C:/Users/nEW u/Desktop/heart disease/hd.csv",sep=",",header=TRUE)
View(heart)
str(heart)
summary(heart)
is.na(heart)

#Scatter Plot
plot(restbps ~ cholestrol,heart)
with(heart,text(restbps ~ cholestrol, labels=patient.ID,pos=3,cex=.1))
del<-heart[,-c(1,1,3,3,8,8)]
del

#Normalization
m<-apply(del,2,mean)
s<-apply(del,2,sd)
del<-scale(del,m,s)
del

#Euclidean Distance
distance<-dist(del)
print(distance,digits = 3)

#cluster dendogram with complete linkage
hc.c<-hclust(distance)
plot(hc.c,labels = heart$patient.ID)
plot(hc.c,hang=-1)

#cluster dendogram with average linkage
hc.a<-hclust(distance,method = "average")
plot(hc.a,hang=-1)

#cluster membership
member.c<-cutree(hc.c,2)
member.a<-cutree(hc.a,2)
table(member.c,member.a)

#Cluster Means
aggregate(del,list(member.c),mean)
aggregate(heart[,-c(1,1)],list(member.c),mean)

#Silhouette Plot
library(cluster)
plot(silhouette(cutree(hc.c,2),distance))

#Scree Plot
wss<-(nrow(del)-1)*sum(apply(del,2,var))
for(i in 2:20)wss[i]<-sum(kmeans(del,centers=i)$withinss)
plot(1:20,wss,type="b",xlab="Number of Clusters",ylab="Within group SS")

#K-means clustering
kc<-kmeans(del,2)
kc
plot(restbps~cholestrol,heart,col=kc$cluster)
