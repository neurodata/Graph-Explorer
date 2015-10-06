
getLayout <- function(g,plotMethod,u, FRniter, FRcoolexp,
    circular, star.center,
   n, KKniter, KKinittemp, KKcoolexp, scaleLaplacian,dim=3,
   plotOnly=TRUE,theta,xcoords,ycoords,zcoords)
{
  if(is.null(g)) return(NULL)
  layout <- paste('layout',gsub(" ",".",tolower(plotMethod)),
                  sep=".")
  if(layout == 'layout.auto'){
     layout <- layout.auto(g,dim=dim)
  } else if(layout == 'layout.laplacian'){
     layout <- computeLaplacian(g,d=dim,normalize=scaleLaplacian)
  } else if(layout == 'layout.rdpg'){
     z <- graph.spectral.embedding(g,no=dim)
     if(u=="U") {
        x <- z$u
     } else if(u=="V"){
        x <- z$v
     } else {
        if(plotOnly){
           x <- cbind(z$u[,1:2],z$v[,1])
        } else {
           x <- cbind(z$u,z$v)
        }
     }
     layout <- x
  } else if(layout == 'layout.fruchterman.reingold'){
     layout <- layout.fruchterman.reingold(g,niter=FRniter,
                                          coolexp=FRcoolexp,dim=dim)
  } else if(layout == 'layout.fruchterman.reingold.grid'){
     layout <- layout.fruchterman.reingold.grid(g,niter=FRniter,
                                          coolexp=FRcoolexp,dim=dim)
  } else if(layout == 'layout.reingold.tilford'){
     layout <- layout.reingold.tilford(g,circular=circular)
  } else if(layout == 'layout.random'){
     layout <- layout.random(g,dim=dim)
  } else if(layout == 'layout.star'){
     layout <- layout.star(g,center=min(star.center,n))
  } else if(layout == 'layout.t-sne'){
     z <- graph.spectral.embedding(g,no=min(25,vcount(g)))
     if(is.directed(g)) {
         y <- cbind(z$u,z$v)
     } else {
         y <- z$u
     }
     layout <- Rtsne(y,dim=dim,pca=FALSE,theta=theta)$Y
  } else if(layout == 'layout.kamada.kawai'){
     layout <- layout.kamada.kawai(g,niter=KKniter,
                                          inittemp=KKinittemp,
                                          coolexp=KKcoolexp,dim=dim)
  } else if(layout =='layout.coordinates'){
     if(is.null(xcoords) || is.null(ycoords) || (dim==3 && is.null(zcoords))){
         return(NULL)
     } else {
        a1 <- get.vertex.attribute(g,xcoords)
        a2 <- get.vertex.attribute(g,ycoords)
        if(!is.numeric(a1)){
           a1 <- match(a1,unique(a1))
        }
        if(!is.numeric(a2)){
           a2 <- match(a2,unique(a2))
        }
        layout <- cbind(x=a1,y=a2)
        if(dim==3) {
           a3 <- get.vertex.attribute(g,zcoords)
           if(!is.numeric(a3)){
              a3 <- match(a3,unique(a3))
           }
           layout <- cbind(layout,z=a3)
        }
     }   
  } else {
     layout <- get(layout)(g)
  }
  layout

}

