

graph.knn <- function(x,k=10,directed=TRUE)
{
   nn <- get.knn(x,k=k)
   a <- lapply(1:nrow(x),function(i){
            data.frame(from=rep(i,k),to=nn$nn.index[i,])
         })
   edges <- as.matrix(rbindlist(a))
   simplify(graph.edgelist(edges,directed=directed))
}
