plotGraph <- function(x,g,
               sizeByVar,
               vertexAttsSize,
               vertexLabel,
               vertexSize,
               vertexAttsColor,
               colorByVar,
               fast,
               UseAlpha,
               alphaLevel,
               edgeLabel,
               edgeColor,
               useWeights,
               showLegend,
               color='SkyBlue',
               markgroups,
               groups)
{               
     if(sizeByVar && vertexAttsSize != 'None'){
        size <- get.vertex.attribute(g,vertexAttsSize)
        if(all(is.numeric(size))){
           size <- 3*size/max(size)
        } else {
           warning("sizing vertices using a non-numeric label")
           a <- sort(unique(size))
           size <- match(size,a)
           size <- 3*size/max(size)
        }
     } else {
        size <- vertexSize
     }
     if(missing(color) && colorByVar && vertexAttsColor != 'None'){
        color <- get.vertex.attribute(g,vertexAttsColor)
        vars <- color
        if(all(is.numeric(color))){
           if(diff(range(color))!=0) {
              color <- gray((max(color)-color)/(max(color)-min(color)))
           }
        } else {
           a <- sort(unique(color))
           color <- match(color,a)
        }
        legnd <- unique(data.frame(name=vars,color=color,
                        stringsAsFactors=FALSE))
        legnd <- legnd[order(legnd$name),]
     } 
     if(fast){
        fastPlot(g,x,UseAlpha,alphaLevel,size,
        color=color)
     } else {
        vl <- vertexLabel
        if(vl=='None') vl <- NA
        else vl <- get.vertex.attribute(g,vl)
        el <- edgeLabel
        if(el=='None') el <- NA
        else el <- get.edge.attribute(g,el)
        weight <- 1
        if(useWeights) {
           if('weight' %in% list.edge.attributes(g)){
              weight <- get.edge.attribute(g,'weight')
           }
        }
        ec <- edgeColor
        if(ec=='None') {
           ec <- 1
        } else {
           att <- get.edge.attribute(g,ec)
           uatt <- unique(att)
           ec <- ((match(att,uatt)-1) %% 8) + 1
        }
        if(UseAlpha) ec <- alpha(ec,alphaLevel)
        if(markgroups){
           plot(g,vertex.size=size,vertex.label=vl,
                vertex.color=color,
                edge.width=weight,edge.label=el,
                edge.color=ec,
                mark.groups=groups,
                layout=x)
        } else {
           plot(g,vertex.size=size,vertex.label=vl,
                vertex.color=color,
                edge.width=weight,edge.label=el,
                edge.color=ec,
                layout=x)
        }
     }
     if(showLegend){
        n <- nrow(legnd)
        if(n>40){
           nc <- 5
        } else if(n>30) {
           nc <- 4
        } else if(n>20) {
           nc <- 3
        } else if(n>10) {
           nc <- 2
        } else {
           nc <- 1
        }
        legend(x="topright",legend=legnd$name,col=legnd$color,
               ncol=nc,
               pch=20)
     }
}
