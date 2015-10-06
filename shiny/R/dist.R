
meilaVI <- function(x,y){
   cluster.stats(clustering=x,alt.clustering=y,compareonly=TRUE,
                 silhouette=FALSE)$vi
}

meila <- function(x,...)
{
   M <- matrix(0,nrow=nrow(x),ncol=nrow(x))
   for(i in 1:(nrow(x)-1)){
      a <- x[i,]
      for(j in (i+1):nrow(x)){
         b <- x[j,]
         M[i,j] <- meilaVI(a,b)
         M[j,i] <- M[i,j]
      }
   }
   M
}

eqDist <- function(x,...)
{
   M <- matrix(0,nrow=nrow(x),ncol=nrow(x))
   for(i in 1:(nrow(x)-1)){
      a <- x[i,]
      for(j in (i+1):nrow(x)){
         b <- x[j,]
         M[i,j] <- sum(a!=b)
         M[j,i] <- M[i,j]
      }
   }
   as.dist(M)
}
