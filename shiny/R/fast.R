
fastPlot <- function(g,layout,use.alpha,alpha,size,color=2)
{
   plot(layout,pch=20,axes=FALSE,xlab="",ylab="",cex=size,col=color)
   edges <- get.edgelist(g,names=FALSE)
   if(use.alpha){
      col <- alpha('black',alpha)
   } else {
      col <- 1
   }
   if(is.directed(g)){
      arrows(layout[edges[,1],1],layout[edges[,1],2],
             layout[edges[,2],1],layout[edges[,2],2],length=0.1,col=col)
   } else {
      segments(layout[edges[,1],1],layout[edges[,1],2],
             layout[edges[,2],1],layout[edges[,2],2],col=col)
   }
   points(layout,pch=20,cex=size,col=color)
}

fastPlot3D <- function(g,layout,use.alpha,alpha,random)
{
   if(ncol(layout)==2) {
      if(random){
         layout <- cbind(layout,runif(nrow(layout)))
      } else {
         layout <- cbind(layout,rep(0,nrow(layout)))
      }
   }
   plot3d(layout,axes=FALSE,xlab="",ylab="",zlab="",box=FALSE,col=1)
   edges <- get.edgelist(g,names=FALSE)
   x <- matrix(0,nrow=2*nrow(edges),ncol=3)
   x[(1:nrow(edges))*2-1,] <- layout[edges[,1],]
   x[(1:nrow(edges))*2,] <- layout[edges[,2],]
   cat("plotting edges\n")
   if(use.alpha){
      segments3d(x[,1],x[,2],x[,3],col='black',alpha=alpha)
   } else {
      segments3d(x[,1],x[,2],x[,3],col=1)
   }
   cat("Done\n")
}

