# filter evaporation data using moving average/median
#
# Author: M. Weynants
# Date created: 2013/05/30
######################################################
mov.filter <- function(data,N,method=c("median","mean")){
output<-as.data.frame(matrix(NA,nrow=nrow(data)/N,ncol=2))
names(output)<-names(data)
indc=0
count=0
while (indc+N <= nrow(data)){
#print("#########")
count=count+1
indc <- (indc+1):(indc+N)
#print(indc)
output[count,2] <- eval(as.name(method[1]))(data[indc,2])
indc<-indc[ceiling(N/2)]
#print(indc)
output[count,1] <- data[indc,1]
}
return(output)
}

# D <- hydi$RET[hydi$RET$SAMPLE_ID==6200022701,c("HEAD","THETA")]
# plot(THETA~log10(HEAD),data=D)
# D <- D[order(D$HEAD),]
# # N: number of points used at a time
# # we want 15-16 points
# N <- ceiling(nrow(D)/10)
# D2 <- mov.filter(D,N,"median")
# D3 <- mov.filter(D,N,"mean")
# points(THETA~log10(HEAD),data=D2,col="green",pch=4)
# points(THETA~log10(HEAD),data=D3,col="red",pch=3)

# to be changed: needs to work only in the range where diff(h) is small
h.filter <- function(data){
output<-as.data.frame(matrix(NA,nrow=1,ncol=2))
names(output)<-names(as.data.frame(data))
d <- data[order(data[,1]),]
# remove duplicates
d <- d[!duplicated(d),]
# average duplicated heads
if (any(duplicated(d[,1]))){
wdup <- which(duplicated(d[,1]))
hu<-unique(d[wdup,1])
d.a <- d[! d[,1] %in% hu,]
for (ih in hu){
d.a <- rbind(d.a,colMeans(d[d[,1]==ih,]))
}
# sort again
d <- d.a[order(d.a[,1]),]
}
# keep first measurement
output[1,] <- d[1,]
# after randomly select 2 points by range
r <- c(0, 5, 10,20, 50, 100, 200, 250, 500, 1000, 2000, 5000, 10000,15000,16000)
for (ir in 1:14){
ind <- which(d[,1] > r[ir] & d[,1] <= r[ir+1])
if (length(ind)>0){if (ind[1] == 1){ind <- ind[-1]}}
if (length(ind)>2){
# sample
indr <- sample(ind,2)
} else {indr <- ind}
output <- rbind(output,d[indr,])
}
return(output)
}

# new <- h.filter(D)

# plot(THETA~log10(HEAD),data=D)
# points(THETA~log10(HEAD),data=new,col="red")
