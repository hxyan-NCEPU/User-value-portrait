#Hopkins Statistics for mixed data via Gower distance
library(readxl) #for reading the local data
library(stats)
library(StatMatch) #for getting the gower distance betwen samples
##distance betwen samples
df <- ModelData #data load
str(df) #check the data frame
set.seed(123)
w <- gower.dist(data.x=as.data.frame(df),data.y=as.data.frame(df),KR.corr =TRUE) #gower distance betwen samples
w.s <- apply(w,2,function(w){w[w==0]<-NA;min(w, na.rm = TRUE)}) #find the second  minimum value of the gower distance
sum(w.s)
##distance betwen artificial samples
df.r <- ModelData.r #artificial samples data load
str(df.r) #check the data frame
set.seed(123)
q <- gower.dist(data.x=as.data.frame(df),data.y=as.data.frame(df.r),KR.corr =TRUE)#gower distance betwen samples and artificial samples
q.s <- apply(q,2,function(q){q[q==0]<-NA;min(q, na.rm = TRUE)}) #find the second  minimum value of the gower distance
sum(q.s)
H.G <- sum(q.s)/(sum(q.s)+sum(w.s)) #value of Hopkins Statistic 
list(H.G=H.G, dist.r=sum(q.s), dist.s=sum(w.s)) #list of the result
