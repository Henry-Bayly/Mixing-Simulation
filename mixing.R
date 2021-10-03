library(statnet)
#First thing is to create a random Bernoulli network
#I'll use 10 nodes for the initial graph
n<-10
g<- network(n, directed=F)
plot(g)
#Now suppose we add in k nodes
# I'll define k as 5 for this simulation
k<-5
adj <- as.matrix(g)
for ( x in 1:k){
a <- c()
for ( i in 1:(n+x)){
  hold<-runif(1)
  if (hold >= 0.5){
    a[i] <- 1
  }
  else{
    a[i] <- 0
  }
}
## to avoid self directed loops we alter the final node to be 0 in all cases
a[n+x]<-0
adj<-cbind(adj,a)
adj<-rbind(adj,a)
}
#check to make sure matrix is still symmetric
isSymmetric(adj)
newNet <- as.network(adj,directed = F)
plot(newNet)
#Now need to check to see how many ties were formed between old and new nodes
count <- 0
for (i in 11:(n+5)){
  for (x in 1:n){
    if(adj[i,x] == 1){
      count <- count+1
    }
  }
}
print(count)
