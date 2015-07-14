###clustering portugees score data##
por=read.csv(file.choose())

#Finding the adequate number of clusters

wss=0
for (i in 1:15) {
	wss[i]=sum(kmeans(por,centers=i)$withinss)
}
plot(1:15,wss,type='b',xlab="number of clusters",ylab="withn group sum of squares")

#optimize the degree of within sum of square

por1=por[,-c(3,4,5,6,10)]

#using kmeans function to cluster
v1=kmeans(por1,5)

por2=por1[,-c(1,7,23)]

v2=kmeans(por2,5)

#adding cluster result
por2$cluster=v2$cluster

por2


###clustering math score data##
math=read.csv(file.choose())

#Finding the adequate number of clusters

wss=0
for (i in 1:15) {
	wss[i]=sum(kmeans(math,centers=i)$withinss)
}
plot(1:15,wss,type='b',xlab="number of clusters",ylab="withn group sum of squares")

#optimize the degree of within sum of square
math1=math[,-c(3,4,5,6,10)]

math2=math1[,-c(1,7,23)]

#using kmeans function to cluster
u2=kmeans(math2,5)

#adding cluster result
math2$cluster=u2$cluster

math2