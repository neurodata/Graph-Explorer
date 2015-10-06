
graph_spectral_embedding <- function(h,d=10,weighted=FALSE,
                                     normalize=TRUE,
                                     m=1,
                                     diagonal,
                                     svd.method=1,
                                     ...)
{
   if(vcount(h)==1) return(list(u=matrix(0,nrow=1,ncol=d),
                                v=matrix(0,nrow=1,ncol=d),d=0))
   if(vcount(h)==2) {
      if(d==1) return(list(u=matrix(0,nrow=1,ncol=1),
                           v=matrix(0,nrow=1,ncol=1),d=1))
      u <- matrix(0,nrow=2,ncol=d)
      u[1,1] <- 1
      u[2,2] <- 1
      v <- matrix(0,nrow=2,ncol=d)
      return(list(u=u,v=v,d=c(1,1)))
   }
   if(any(diagonal<0) && m==2) {
      warning("negative diagonal -- matrix might not be positive definite")
   }
   if(m==1){
      if(weighted){
         A <- get.adjacency(h,attr='weight')
      } else {
         A <- get.adjacency(h)
      }
   } else {
      if(is.directed(h)){
         h <- as.undirected(h,mode='collapse')
      }
      if(weighted){
         A <- graph.laplacian(h,normalize=normalize,weights=NULL)
      } else {
         A <- graph.laplacian(h,normalize=normalize,weights=NA)
      }
   }
   A <- A+diagonal
   if(m==1){
      if(svd.method==1){
         out <- svds(A,k=d,nu=d,nv=d,...)
      } else {
         out <- irlba(A,nu=d,nv=d,...)
      }
   } else {
      z <- eigs(A,which="SR",k=d+1,...)
      o <- order(z$values,decreasing=FALSE)
      u <- z$vectors[,o]
      val <- z$values[o]
      u <- u[,-1]
      val <- val[-1]
      out <- list(u=u,d=val)
   }
   out
}

graph.spectral.embedding <- function(g,
                                     no=10,
                                     weighted=FALSE, 
                                     by.component=TRUE, 
                                     normalize=TRUE, 
                                     base.matrix="adjacency", 
                                     diagonal.method="RDPG", 
                                     tau=0, 
                                     svd.method='ARPACK', 
                                     ...)
{
   d <- no
   matrices <- c("adjacency","laplacian")
   m <- pmatch(tolower(base.matrix),matrices)
   if(is.na(m)) stop("improper matrix specification")
   if(m==2 && missing(diagonal.method)){
      diagonal.method <- 'Constant'
   }
   methods <- c("RDPG","Degree","Constant")
   dm <- pmatch(tolower(diagonal.method),tolower(methods))
   smethods <- c("ARPACK","IRLBA")
   sm <- pmatch(tolower(svd.method),tolower(smethods))
   v <- NA
   if(is.na(dm)) stop("improper diagonal specification")
   if(by.component){
      clusts <- clusters(g)
      u <- matrix(0,nrow=vcount(g),ncol=d)
      if(m==1)
         v <- matrix(0,nrow=vcount(g),ncol=d)
      s <- matrix(NA,nrow=clusts$no,ncol=d)
      for(i in 1:clusts$no){
         inds <- which(clusts$membership==i)
         h <- induced.subgraph(g,inds)
         nh <- vcount(h)
         if(dm==1) {
            diagonal <- degree(h)/(nh-1)
         } else if(dm==2) {
            diagonal <- degree(h)
         } else if(dm==3) {
            diagonal <- rep(tau,nh)
         }
         z <- graph_spectral_embedding(h=h,
                                       d=min(d,nh),
                                       weighted=weighted,
                                       normalize=normalize,
                                       m=m,
                                       diagonal=Diagonal(x=diagonal),
                                       svd.method=sm,
                                       ...)
         a <- 1:min(d,nh)
         u[inds,a] <- z$u
         if(m==1){
            v[inds,a] <- z$v
         }
         s[i,a] <- z$d
      }
   } else {
      ng <- vcount(g)
      if(dm==1) {
         diagonal <- degree(g)/(ng-1)
      } else if(dm==2) {
         diagonal <- degree(g)
      } else if(dm==3) {
         diagonal <- rep(tau,ng)
      }
      z <- graph_spectral_embedding(h=g,
                                    d=min(d,ng),
                                    weighted=weighted,
                                    normalize=normalize,
                                    m=m,
                                    diagonal=Diagonal(x=diagonal),
                                    svd.method=sm,
                                    ...)
      u <- z$u
      if(m==1)
         v <- z$v
      s <- matrix(z$d,nrow=1)
      clusts <- list(membership=NA)
   }
   list(u=u,v=v,components=clusts$membership,values=s)
}

computeLaplacian <- function(g,d,normalize)
{
     out <- graph.spectral.embedding(g,d,
                   normalize=normalize,base.matrix="laplacian",
                    diagonal.method="constant")
     out$u
}

