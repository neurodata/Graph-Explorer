
fuseGraphCovariates <- function(g,x,lambda,k=10,...)
{
   h <- graph.knn(x,k=k,directed=is.directed(g))
   A <- lambda*get.adjacency(g) + (1-lambda)*get.adjacency(h)
   cat("Clusters:",no.clusters(g),no.clusters(h),"\n")

   ombG <- graph.adjacency(A,
               mode=ifelse(is.directed(g),"directed","undirected"),
               weighted=TRUE,
               diag=FALSE)

   z <- graph.spectral.embedding(ombG,weighted=TRUE,...)
   if(is.directed(g)) {
      out <- cbind(z$u,z$v)
   } else {
      out <- z$u
   }
   out
}

