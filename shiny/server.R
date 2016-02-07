### Main shinyServer function
shinyServer(function(input, output, session) {

set.seed(seed)

observe({
   if(is.numeric(input$seed))
      set.seed(input$seed)
})
   
  gGraph <- reactive({
     g <- NULL
	  if(input$whereGraph=='Generate'){
		  if(input$GenerateGraph>0){
			  isolate({
				  cat("Generating:",input$genMethod,"graph\n")
				  print(system.time({
					  parameters <- list(n=input$genN,directed=input$genDirected,
					                     seed=input$seed)
					  if(input$genMethod=='Erdos Renyi'){
						  parameters$p <- input$genERP
					  } else if(input$genMethod=='K Regular'){
						  parameters$k <- input$genKRK
					  } else if(input$genMethod=='Famous'){
						  parameters$famous <- input$genFamous
					  } else if(input$genMethod=='Atlas'){
						  parameters$atlas <- input$genAtlas
					  } else if(input$genMethod=='Generated Worm'){
					    parameters$generatedworm <- input$genGeneratedworm
					  } else if(input$genMethod=='Wheel'){
						  parameters$mode <- input$genMode
					  } else if(input$genMethod=='Barabasi'){
						  parameters$power <- input$genBPower
						  parameters$m <- input$genBM
						  parameters$zero <- input$genBZero
					  } else if(input$genMethod=='Complete Bipartite'){
						  parameters$n2 <- input$genCBPn
						  parameters$mode <- input$genMode
						  if(parameters$directed==FALSE ||
						     parameters$mode=='mutual') parameters$mode <- 'all'
					  } else if(input$genMethod=='Random Bipartite'){
						  parameters$n2 <- input$genRBPn
						  parameters$p <- input$genRBPP
						  parameters$mode <- input$genMode
						  if(parameters$directed==FALSE ||
						     parameters$mode=='mutual') parameters$mode <- 'all'
					  } else if(input$genMethod=='Aging Prefatt'){
					     parameters$time.window <- input$genAPtimewindow
					     parameters$deg.coef <- input$genAPdegcoef
					     parameters$age.coef <- input$genAPagecoef
					     parameters$aging.bin <- input$genAPagingbin
					     parameters$aging.exp <- input$genAPagingexp
					     parameters$pa.exp <- input$genAPpaexp
					     parameters$m <- input$genAPm
					     parameters$zero.deg.appeal <- input$genAPzerodeg
					     parameters$zero.age.appeal <- input$genAPzeroage
					  } else if(input$genMethod=='de Bruijn'){
					     parameters$dbn <- input$gendBn
					     parameters$m <- input$gendBm
					  } else if(input$genMethod=='Stochastic Block Model'){
						  k <- input$genSBMk
						  P <- matrix(runif(k*k,0,0.5),nrow=k)
						  if(!input$genDirected){
							  P[lower.tri(P)] <- t(P[upper.tri(P)])
						  }
					     if(input$genDiagDom){
						     diag(P) <- runif(input$genSBMk,0.5,1)
						  } 
						  parameters$P <- P
						  sizes <- rep(floor(input$genN/k),k)
						  sizes[k] <- sizes[k]+(input$genN-sum(sizes))
						  parameters$sizes <- sizes
					  } else if(input$genMethod=='Random Dot Product'){
					     parameters$dim <- input$genRDPDim
						  parameters$alpha <- rep(1,parameters$dim)
						  parameters$Vect <- input$genRDPMethod
					  } else if(input$genMethod=='Kautz'){
					     parameters$kn <- input$genKn
					     parameters$m <- input$genKm
					  } else if(input$genMethod=='Forest Fire'){
					     parameters$fw.prob <- input$genFFfw
					     parameters$bw.factor <- input$genFFbf
					     parameters$ambs <- input$genFFAmbs
					  } else if(input$genMethod=='Tree'){
					        parameters$children <- input$genTreeC
					  } else if(input$genMethod=='Growing Random Graph'){
						  parameters$m <- input$genGROWM
						  parameters$citation <- input$genGROWCitation
					  } else if(input$genMethod=='Watts Strogatz'){
						  parameters$dim <- input$genWSDim
						  parameters$size <- input$genWSSize
						  parameters$nei <- input$genWSNei
						  parameters$p <- input$genWSP
					  } else if(input$genMethod=='Geometric Random Graph'){
						  parameters$torus <- input$genGRGTorus
						  parameters$coords <- input$genGRGcoords
						  parameters$radius <- input$genGRGRadius
					  } else if(input$genMethod=='Barabasi'){
						  parameters$power <- input$genBPower
						  parameters$m <- input$genBM
						  parameters$zero <- input$genBZero
					  } else if(input$genMethod=='Star'){
						  parameters$mode <- input$genMode
					  }
					  if(parameters$directed==FALSE && 
					     input$genMethod != 'Random Bipartite' &&
					     input$genMethod != 'Complete Bipartite') {
						  parameters$mode <- 'undirected'
					  }
					  g <- generateGraph(input$genMethod,parameters)
				  }))
			  })
		  }
	  } else if(input$whereGraph=='From OpenConnectome'){
		  if(nchar(input$graphOCFile)>5){
			  cat("OpenConnectome file:",input$graphOCFile,"\n")
			  t1 <- system.time(g <- read.graph(input$graphOCFile,
					  format='graphml'))
			  cat("File read\n")
			  print(t1)
		  }
	  } else {
		  if(!is.null(input$graphFile)){
			  format <- "graphml"
			  ex <- rev(strsplit(basename(input$graphFile$name),
								 split="\\.")[[1]])[[1]]
			  cat("File Extension:",ex,"\n")
			  if(ex %in% c("edgelist", "pajek", "ncol", "lgl",
								"graphml", "dimacs", "graphdb", "gml", "dl")){
				  format <- ex
				}
				cat("name:",input$graphFile$name,"\n")
				cat("datapath:",input$graphFile$datapath,"\n")
				if(grepl('\\.zip$',input$graphFile$name)){
				  tf <- input$graphFile$datapath
				  gr <- sub("\\.zip$","",input$graphFile$name)
				  t1 <- system.time(g <- read.graph(unz(tf,gr),format=format))
				} else {
				  t1 <- system.time(g <- read.graph(input$graphFile$datapath,
						  format=format))
				}
			  cat("File read\n")
			  print(t1)
		  }
	  }
     if(!is.null(g)){
        vertexLabels <- union("None",list.vertex.attributes(g))
        updateSelectInput(session,inputId="vertexLabel",
            choices=vertexLabels,selected="None")
        updateSelectInput(session,inputId="vertexAttsSize",
            choices=vertexLabels,selected="None")
        updateSelectInput(session,inputId="vertexAttsColor",
            choices=vertexLabels,selected="None")
        updateSelectInput(session,inputId="CvertexLabel",
            choices=c("None","Community",vertexLabels),selected="Community")
        updateSelectInput(session,inputId="vertexAtts",
            choices=vertexLabels,selected="None")
        variables$select <- selectSave(variables$select,
            inputId="vertexLabel",
            choices=vertexLabels,selected="None")
        variables$select <- selectSave(variables$select,
            inputId="vertexAttsSize",
            choices=vertexLabels,selected="None")
        variables$select <- selectSave(variables$select,
            inputId="vertexAttsColor",
            choices=vertexLabels,selected="None")
        variables$select <- selectSave(variables$select,
            inputId="CvertexLabel",
            choices=c("None","Community",vertexLabels),selected="Community")
        variables$select <- selectSave(variables$select,
            inputId="vertexAtts",
            choices=vertexLabels,selected="None")
        vl <- vertexLabels[-grep("None",vertexLabels)]
        if(length(vl)>0){
           updateRadioButtons(session,inputId="xcoordinates",
               choices=vl,selected=vl[1])
           updateRadioButtons(session,inputId="ycoordinates",
               choices=vl,selected=vl[min(2,length(vl))])
           updateRadioButtons(session,inputId="zcoordinates",
               choices=vl,selected=vl[min(3,length(vl))])
           variables$radio <- selectSave(variables$radio,inputId="xcoordinates",
               choices=vl,selected=vl[1])
           variables$radio <- selectSave(variables$radio,inputId="ycoordinates",
               choices=vl,selected=vl[min(2,length(vl))])
           variables$radio <- selectSave(variables$radio,inputId="zcoordinates",
               choices=vl,selected=vl[min(3,length(vl))])
        }
        edgeLabels <- union("None",list.edge.attributes(g))
        updateSelectInput(session,inputId="edgeLabel",
            choices=edgeLabels,selected="None")
        updateSelectInput(session,inputId="edgeColor",
            choices=edgeLabels,selected="None")
        updateSelectInput(session,inputId="edgeAtts",
            choices=edgeLabels,selected="None")
        variables$select <- selectSave(variables$select,inputId="edgeLabel",
            choices=edgeLabels,selected="None")
        variables$select <- selectSave(variables$select,inputId="edgeColor",
            choices=edgeLabels,selected="None")
        variables$select <- selectSave(variables$select,inputId="edgeAtts",
            choices=edgeLabels,selected="None")
        m <- max(3,floor(vcount(g)/40))
        updateSliderInput(session,inputId="subsample",
            min=2,max=m,value=min(4,m-1),step=1)
        variables$slider <- sliderSave(variables$slider,inputId="subsample",
            min=2,max=m,value=min(4,m-1),upper.value=NA,step=1)
     }
     g
  })

  ## For the RDPG method, the input$u field controls which singular
  ## vectors are plotted:
  ## if we are plotting in 2D, it concatenates u[,1] and v[,1].
  ## if in 3D, it concatenates u[,1:2] and v[,1].
  layout <- reactive({
     set.seed(input$seed)
     g <- gGraph()
     getLayout(g, 
               plotMethod=input$plotMethod, 
               u=input$u, 
               FRniter=input$FRniter,
               FRcoolexp=input$FRcoolexp, 
               circular=input$circular, 
               star.center=input$star.center,
               n=input$n, 
               KKniter=input$KKniter, 
               KKinittemp=input$KKinittemp, 
               KKcoolexp=input$KKcoolexp,
               scaleLaplacian=input$scaleLaplacian,
               dim=as.numeric(input$layoutD),plotOnly=TRUE,theta=input$theta,
               xcoords=input$xcoordinates,
               ycoords=input$ycoordinates,
               zcoords=input$zcoordinates)
  })

  getSubsampled <- reactive({
     set.seed(input$seed)
     g <- gGraph()
     if(!is.null(g)){
        A <- get.adjacency(g)
        cat("Subsample:",input$subsample,"\n")
        if(input$subsample>1){
           if(input$contour || nrow(A)>2000){
              w <- input$subsample
              cat("Window:",w,"\n")
              B <- apply(A,1,slideFunct,window=w,step=round(w/2))
              A <- apply(B,2,slideFunct,window=w,step=round(w/2))
           }
        }
     }
     A
  })

  ### Generate plot output
  output$adjacencyPlot <- renderPlot({  
     g <- gGraph()
     if(!is.null(g)){
        A <- getSubsampled()
        if(input$contour){
           contour(1:nrow(A),1:ncol(A),A,xlab="",ylab="",axes=FALSE)
        } else {
           image(t(A)[nrow(A):1,])
        }
     }
  })

  output$info <- renderText({
     x <- input$plot_click$x
     y <- input$plot_click$y
     g <- gGraph()
     values <- ""
     if(!is.null(g)){
        points <- layout()
        if(!is.null(points)){
            if(!input$fast) {
               points <- layout.norm(points,-1,1,-1,1)
            }
            a <- which.min(abs(points[,1]-x)+abs(points[,2]-y))
            atts <- list.vertex.attributes(g)
            if(length(atts)==0) return(paste("Vertex:",a))
            for(i in 1:length(atts)){
               allatts <- get.vertex.attribute(g,atts[i])
               if(is.numeric(allatts)) {
                  nstr <- paste0("range: ",
                          round(min(allatts,na.rm=TRUE),3),
                          " -- ",
                          round(max(allatts,na.rm=TRUE),3))
               } else {
                  nstr <- paste0("#values: ",length(unique(allatts)))
               }
               values <- paste(values,"\n",
                               paste(atts[i],": ",
                                     allatts[a],
                                     " (", nstr,")",
                                     sep=""))
            }
            din <- degree(g,v=a,mode='in')
            dout <- degree(g,v=a,mode='out')
            values <- paste(values,"\n",
                            "In degree:",din,"\n",
                            "Out degree:",dout)
        }
     }
     values
  })

  output$comm_info <- renderText({
     x <- input$comm_click$x
     y <- input$comm_click$y
     g <- gGraph()
     values <- ""
     if(!is.null(g)){
        points <- layout()
        if(!is.null(points)){
            z <- getCommunities()
            if(input$communities == "RDPG" ||
               input$communities == "t-SNE" ||
               input$communities == "Laplacian" 
            ){
               m <- z$classification
            } else {
               m <- membership(z)
            }
            if(input$usecomm) points[,1] <- m
            if(!input$fast) {
               points <- layout.norm(points,-1,1,-1,1)
            }
            a <- which.min(abs(points[,1]-x)+abs(points[,2]-y))
            atts <- list.vertex.attributes(g)
            if(length(atts)==0) return(paste("Vertex:",a))
            for(i in 1:length(atts)){
               allatts <- get.vertex.attribute(g,atts[i])
               if(is.numeric(allatts)) {
                  nstr <- paste0("range: ",
                          round(min(allatts,na.rm=TRUE),3),
                          " -- ",
                          round(max(allatts,na.rm=TRUE),3))
               } else {
                  nstr <- paste0("#values: ",length(unique(allatts)))
               }
               values <- paste(values,"\n",
                               paste(atts[i],": ",
                                     allatts[a],
                                     " (", nstr,")",
                                     sep=""))
            }
            din <- degree(g,v=a,mode='in')
            dout <- degree(g,v=a,mode='out')
            values <- paste(values,"\n",
                            "In degree:",din,"\n",
                            "Out degree:",dout)
           values <- paste(values,"\n","Community:",m[a])
        }
     }
     values
  })

  ### Generate plot output
  output$plotgraph <- renderPlot({  
     g <- gGraph()
     if(!is.null(g)){
        x <- layout()
        if(is.null(x)) return(NULL)
        cat("Layout (plot):",dim(x),vcount(g),"\n")
      plotGraph(x,g,
            sizeByVar=input$sizeByVar,
            vertexAttsSize=input$vertexAttsSize,
            vertexLabel=input$vertexLabel,
            vertexSize=input$vertexSize,
            vertexAttsColor=input$vertexAttsColor,
            colorByVar=FALSE,
            fast=input$fast,
            markgroups=input$markGroups,
            groups=groups,
            UseAlpha=input$UseAlpha,
            alphaLevel=input$alphaLevel,
            edgeLabel=input$edgeLabel,
            edgeColor=input$edgeColor,
            useWeights=input$useWeights,
            showLegend=input$showLegend)
     }
  })

  output$plotgraph3d <- renderWebGL({  
        progress <- Progress$new(session,min=1,max=4)
        on.exit(progress$close())
        progress$set(message = 'Computing the 3d plot',
                     detail='Please be patient...')
     g <- gGraph()
        progress$set(value=2)
     if(!is.null(g)){
        x <- layout()
        progress$set(value=3)
        if(is.null(x)) return(NULL)
        fastPlot3D(g,x,input$UseAlpha3D,input$alphaLevel3D,input$randomZ)
        progress$set(value=4)
     }
  })

  output$plotVA <- renderPlot({  
     g <- gGraph()
     if(!is.null(g)){
        if(input$vertexAtts != 'None'){
           a <- get.vertex.attribute(g,input$vertexAtts)
			  if(input$VACor==TRUE){
				  edges <- get.edgelist(g,names=FALSE)
				  if(class(a)=='numeric'){
					  par(pty='s')
				     plot(a[edges[,1]],
					       a[edges[,2]],xlab="From",ylab="To",
					       pch=20)
				  } else {
					  ta <- table(a[edges[,1]],a[edges[,2]])
					  xl <- max(dim(ta),na.rm=TRUE)
					  par(pty='s')
					  image(1:nrow(ta),1:ncol(ta),
					        ta,xlab="From",ylab="To",#col=gray((0:xl)/xl),
					        axes=FALSE)
				     axis(1,at=1:nrow(ta),labels=rownames(ta),las=2)
				     axis(2,at=1:ncol(ta),labels=colnames(ta),las=2)
				  }
			  } else {
				  if(class(a)=='numeric'){
					  hist(a,xlab=input$vertexAtts,main="")
				  } else {
					  ta <- table(a)
					  ta <- sort(ta,decreasing=TRUE)
					  m <- min(30,length(ta))
					  ta <- rev(ta[1:m])
					  mar <- par('mar')
					  par(mar=c(2,7,2,2))
					  barplot(ta,horiz=TRUE,names=names(ta),xlab="",
								 las=2,cex.axis=.75)
					  par(mar=mar)
				  }
			  }
        } 
     } 
  })

  output$plotEA <- renderPlot({  
     g <- gGraph()
     if(!is.null(g)){
        if(input$edgeAtts != 'None'){
           a <- get.edge.attribute(g,input$edgeAtts)
           if(class(a)=='numeric'){
              hist(a,xlab=input$edgeAtts,main="")
           } else {
              ta <- table(a)
              ta <- sort(ta,decreasing=TRUE)
              m <- min(30,length(ta))
              ta <- rev(ta[1:m])
              mar <- par('mar')
              par(mar=c(2,7,2,2))
              barplot(ta,horiz=TRUE,names=names(ta),xlab="",
                      las=2,cex.axis=.75)
              par(mar=mar)
           }
        } 
     } 
  })

  output$plotinvariants <- renderPlot({  
     g <- gGraph()
     if(!is.null(g)){
        if(input$invariants=="Degree Distribution"){
           if(is.directed(g)){
              a <- degree.distribution(g,mode='out')
              b <- degree.distribution(g,mode='in')
              ylim <- range(c(a,b))
              plot(a,xlab="Degree",ylab="Proportion",log='x',
                   ylim=ylim,pch=20)
              points(b,pch=20,col=2)
              legend(0.9*length(a),max(ylim),legend=c("Out","In"),pch=20,col=1:2)
              
           } else {
              plot(degree.distribution(g),xlab="Degree",ylab="Proportion",
                   log='x',pch=20)
           }
        } else if(input$invariants=='Alpha Centrality'){
           plot(alpha.centrality(g,alpha=input$alpha),xlab="Vertex",
                ylab=expression(alpha*~centrality),pch=20)
        } else if(input$invariants=='Authority Score'){
           plot(authority.score(g)$vector,xlab="Vertex",
                ylab="Authority Score",pch=20)
        } else if(input$invariants=='Hub Score'){
           plot(hub.score(g)$vector,xlab="Vertex",
                ylab="Hub Score",pch=20)
        } else if(input$invariants=='Articulation Points'){
           a <- articulation.points(g)
           cols <- rep(1,vcount(g))
           cols[a] <- 2
           x <- layout()
           if(is.null(x)) return(NULL)
           plot(g,layout=x,vertex.color=cols,
                     vertex.frame.color=cols,vertex.size=3)
           title("Articulation Points")
        } else if(input$invariants=='Boncich Power Centrality'){
           plot(bonpow(g),xlab="Vertex",
                ylab="Boncich Centrality",pch=20)
        } else if(input$invariants=='Betweenness'){
           plot(betweenness(g),xlab="Vertex",
                ylab="Betweenness",pch=20)
        } else if(input$invariants=="Burt's Constraint"){
           plot(constraint(g),xlab="Vertex",
                ylab="Burt's Constraint",pch=20)
        } else if(input$invariants=='Eigenvalue Centrality'){
           plot(evcent(g)$vector,xlab="Vertex",
                ylab="Eigenvalue Centrality",pch=20)
        } else if(input$invariants=='Page Rank'){
           plot(page.rank(g,damping=input$damping)$vector,xlab="Vertex",
                ylab="Page Rank",pch=20)
        } else if(input$invariants=='Average KNN Degree'){
           plot(graph.knn(simplify(g))$knn,xlab="Vertex",
                ylab="Average KNN Degree",pch=20)
        } else if(input$invariants=='Closeness'){
           plot(closeness(g),xlab="Vertex",
                ylab="Closeness",pch=20)
        } else if(input$invariants=='Diversity'){
           plot(graph.diversity(g),xlab="Vertex",
                ylab="Diversity",pch=20)
        }
     }
  })


  output$mytable <- DT::renderDataTable({
      g <- gGraph()
      d <- NULL
      if(!is.null(g)){
         d <- computeInvariants(g,input$invariantsList)
      }
      d
  })

  output$graphAtt <- DT::renderDataTable({
      g <- gGraph()
      if(is.null(g)) return(NULL)
      att <- list.graph.attributes(g)
      if(is.null(att) || length(att)==0) return(NULL)
      inv <- data.frame(Attribute=c(att,"|V|","|E|"),
                Value=rep("",length(att)+2),
                stringsAsFactors=FALSE)
      inv[length(att)+1,'Value'] <- vcount(g)
      inv[length(att)+2,'Value'] <- ecount(g)
      for(i in 1:length(att)){
         inv[i,2] <- get.graph.attribute(g,att[i])
      }
      datatable(inv,rownames=FALSE)
  })

  output$vertexAtt <- DT::renderDataTable({
      g <- gGraph()
      if(is.null(g)) return(NULL)
      att <- list.vertex.attributes(g)
      if(is.null(att) || length(att)==0) return(NULL)
      inv <- data.frame(Attribute=att,
                Type=rep("numeric",length(att)),
                Values=rep("",length(att)),
                stringsAsFactors=FALSE)
      for(i in 1:length(att)){
         a <- get.vertex.attribute(g,att[i])
         inv[i,"Type"] <- class(a)
         if(class(a)=='numeric'){
            inv[i,"Values"] <- paste("range:",
                                 paste(round(range(a,na.rm=TRUE),3),
                                       collapse=" -- "))
         } else {
            inv[i,"Values"] <- paste("#unique:",length(unique(a)),collapse=" ")
         }
      }
      datatable(inv,rownames=FALSE)
  })

  output$edgeAtt <- DT::renderDataTable({
      g <- gGraph()
      if(is.null(g)) return(NULL)
      att <- list.edge.attributes(g)
      if(is.null(att) || length(att)==0) return(NULL)
      inv <- data.frame(Attribute=att,
                Type=rep("numeric",length(att)),
                Values=rep("",length(att)),
                stringsAsFactors=FALSE)
      for(i in 1:length(att)){
         a <- get.edge.attribute(g,att[i])
         inv[i,"Type"] <- class(a)
         if(class(a)=='numeric'){
            inv[i,"Values"] <- paste("range:",
                                 paste(round(range(a,na.rm=TRUE),3),
                                       collapse=" -- "))
         } else {
            inv[i,"Values"] <- paste("#unique:",length(unique(a)),collapse=" ")
         }
      }
      datatable(inv,rownames=FALSE)
  })

  getCommunities <- reactive({
                 progress <- Progress$new(session,min=1,max=4)
                 on.exit(progress$close())
                 progress$set(message = 'Computing communities',
                           detail='This may take a while...')
      set.seed(input$seed)
      g <- gGraph()
      progress$set(value=2)
      if(is.null(g)) return(NULL)
      if(input$communities =="Fast Greedy"){
         z <- fastgreedy.community(as.undirected(simplify(g)))
      } else if(input$communities=="Leading Eigenvector"){
         z <- leading.eigenvector.community(as.undirected(simplify(g)))
      } else if(input$communities=="Label Propagation"){
         z <- label.propagation.community(g)
      } else if(input$communities=="Multilevel"){
         z <- multilevel.community(as.undirected(simplify(g)))
      } else if(input$communities=="Edge Betweenness"){
         z <- edge.betweenness.community(g)
      } else if(input$communities=="Infomap"){
         z <- infomap.community(g)
      } else if(input$communities=="Spinglass"){
         z <- spinglass.community(g)
      } else if(input$communities=="Walktrap"){
         z <- walktrap.community(g)
      } else if(input$communities=="Laplacian"){
         x <- computeLaplacian(g,d=input$Cd,normalize=TRUE)
         cat("Mclust model size range:",input$CG,"\nSeed:",input$seed,"\n")
         if(vcount(g)<=1000){
            z <- Mclust(x,G=input$CG[1]:input$CG[2])
         } else {
            init <- sample(vcount(g),1000)
            z <- Mclust(x,G=input$CG[1]:input$CG[2],
                    initialization=list(subset=init))
         }
      } else if(input$communities=="RDPG"){
         x <- graph.spectral.embedding(g,no=input$Cd)
         if(is.directed(g)) {
            y <- cbind(x$u,x$v)
         } else {
            y <- x$u
         }
         if(vcount(g)<=1000){
            z <- Mclust(y,G=input$CG[1]:input$CG[2])
         } else {
            init <- sample(vcount(g),1000)
            z <- Mclust(y,G=input$CG[1]:input$CG[2],
                    initialization=list(subset=init))
         }
      } else if(input$communities=="t-SNE"){
         x <- graph.spectral.embedding(g,no=25)
         if(is.directed(g)) {
            y <- cbind(x$u,x$v)
         } else {
            y <- x$u
         }
         x <- Rtsne(y,pca=FALSE,theta=input$Ctheta,dims=input$TSNEdimension)$Y
         if(vcount(g)<=1000){
            z <- Mclust(x,G=input$CG[1]:input$CG[2])
         } else {
            init <- sample(vcount(g),1000)
            z <- Mclust(x,G=input$CG[1]:input$CG[2],
                    initialization=list(subset=init))
         }
      }
      z
  })

  ### Generate plot output
  output$communityPlot <- renderPlot({  
   dp <- input$dendPlot
   if(dp &&
      (input$communities == "Infomap" ||
      input$communities == "Spinglass" ||
      input$communities == "RDPG" ||
      input$communities == "t-SNE" ||
      input$communities == "Multilevel" ||
      input$communities == "Label Propagation" ||
      input$communities == "Leading Eigenvalue" ||
      input$communities == "Laplacian")
   ){
      updateCheckboxInput(session,inputId="dendPlot",value=FALSE)
      variables$checkbox <- checkboxSave(variables$checkbox,
          inputId="dendPlot",value=FALSE)
      dp <- FALSE
      warning("Cannot dendPlot this community type\n")
   }
   g <- gGraph()
   if(is.null(g)) return(NULL)
   x <- layout()
   if(is.null(x)) return(NULL)
   cat("Layout (community):",dim(x),"|V|:",vcount(g),"\n")
   z <- getCommunities()
   if(input$communities == "RDPG" ||
      input$communities == "t-SNE" ||
      input$communities == "Laplacian" 
   ){
      m <- z$classification
   } else {
      m <- membership(z)
   }
   if(input$usecomm) x[,1] <- m
   vl <- input$vertexLabel
   if(vl == 'None') {
      labels <- NULL
   } else if(vl == 'Community'){
      labels <- m
   } else {
      labels <- get.vertex.attribute(g,vl)
   }
   if(dp){
      dendPlot(z,labels=labels,main=paste(max(m),"Communities"))
   } else {
      col <- colors.list[((m-1) %% length(colors.list))+1]
      groups <- sapply(unique(m),function(i) which(m==i))
      plotGraph(x,g,
            sizeByVar=input$sizeByVar,
            vertexAttsSize=input$vertexAttsSize,
            vertexLabel=input$vertexLabel,
            vertexSize=input$vertexSize,
            vertexAttsColor=input$vertexAttsColor,
            colorByVar=FALSE,
            fast=input$fast,
            markgroups=input$markGroups,
            groups=groups,
            UseAlpha=input$UseAlpha,
            alphaLevel=input$alphaLevel,
            edgeLabel=input$edgeLabel,
            edgeColor=input$edgeColor,
            useWeights=input$useWeights,
            showLegend=FALSE,
            color=col)
           title(paste(max(m),"Communities"))
   }
  })

  getCommunitiesMatrix <- reactive({
     set.seed(input$seed)
        withProgress(message='Computing all communities',
                     detail='This will take a while...',value=0,
        {
        g <- gGraph()
         x <- computeLaplacian(g,d=input$Cd,normalize=TRUE)
         if(vcount(g)<=1000){
            z <- Mclust(x,G=input$CG[1]:input$CG[2])
         } else {
            init <- sample(vcount(g),1000)
            z <- Mclust(x,G=input$CG[1]:input$CG[2],
                    initialization=list(subset=init))
         }
         lap <- z$classification
         incProgress(1/10,detail='Laplacian computed 1/11')
         x <- graph.spectral.embedding(g,no=input$Cd)
         if(is.directed(g)) {
            y <- cbind(x$u,x$v)
         } else {
            y <- x$u
         }
         if(vcount(g)<=1000){
            z <- Mclust(y,G=input$CG[1]:input$CG[2])
         } else {
            init <- sample(vcount(g),1000)
            z <- Mclust(y,G=input$CG[1]:input$CG[2],
                    initialization=list(subset=init))
         }
         rd <- z$classification
         incProgress(1.5/10,detail='RDPG computed 2/11')
         x <- graph.spectral.embedding(g,no=25)
         if(is.directed(g)) {
            y <- cbind(x$u,x$v)
         } else {
            y <- x$u
         }
         x <- Rtsne(y,pca=FALSE,theta=input$Ctheta)$Y
         if(vcount(g)<=1000){
            z <- Mclust(x,G=input$CG[1]:input$CG[2])
         } else {
            init <- sample(vcount(g),1000)
            z <- Mclust(x,G=input$CG[1]:input$CG[2],
                    initialization=list(subset=init))
         }
         ts <- z$classification
         incProgress(2/10,detail='TSNE computed 3/11')
         a1 <- membership(fastgreedy.community(as.undirected(simplify(g))))
         incProgress(3/10,detail='Fast Greedy computed 4/11')
         a2 <- membership(edge.betweenness.community(g))
         incProgress(4/10,detail='Edge Betweenness computed 5/11')
         a3 <- membership(walktrap.community(g))
         incProgress(5/10,detail='Walktrap computed 6/11')
         a4 <- membership(leading.eigenvector.community(
                        as.undirected(simplify(g))))
         incProgress(6/10,detail='Leading Eigenvector computed 7/11')
         a5 <- membership(label.propagation.community(g))
         incProgress(7/10,detail='Label Propagation computed 8/11')
         a6 <- membership(spinglass.community(g))
         incProgress(8/10,detail='Spinglass computed 9/11')
         a7 <- membership(multilevel.community(  
                      as.undirected(simplify(g))))
         incProgress(8.5/10,detail='Multilevel computed 10/11')
         a8 <- membership(infomap.community(g))
         incProgress(9/10,detail='Infomap computed 11/11')
         M <- rbind(lap,rd,ts,a1,a2,a3,a4,a5,a6,a7,a8)
     a <- apply(M,1,max)
     rownames(M) <- paste(c("Laplac","RDPG","t-SNE",
                      "Fast","Edge","Walk",
                      "LEigen","LabelP","SpinG","MultiL","InfoM"),a)
     colnames(M) <- V(g)
      #progress$set(value=10)
         incProgress(1,detail='Communities computed')
     a <- hclust(eqDist(t(M)),method='ward.D2')
     M[,a$order]
  })
  })

  getHeatmapOrder <- reactive({
     M <- getCommunitiesMatrix()
     D <- meila(M)
     heatmap2(D)
  })

  output$communityCompM <- renderPlot({  
     coms <<- getCommunitiesMatrix()
     D <- meila(coms)
     rownames(D) <- rownames(coms)
     colnames(D) <-  gsub(" [[:digit:]]{1,}","",rownames(coms))
     cat("communities computed\n")
     heatmap(D,col=gray((255:0)/255))
  })

  output$communityM_info <- renderText({
     M <- getCommunitiesMatrix()
     h <- getHeatmapOrder()
     D <- meila(M)
     rnames <- gsub(" [[:digit:]]{1,}","",rownames(M)[h$rowInd])
     ## constants in these formulae are empirical due to
     ## the fact that heatmap does a layout, and the coordinates
     ## are all wonky
     a <- seq(0.07,0.96,length=ncol(D))
     b <- seq(0.18,0.72,length=nrow(D))
     x <- input$communityImage_click$x
     y <- input$communityImage_click$y
     if(!is.null(x)){
        j <- which.min(abs(x-a))
        i <- which.min(abs(y-b))
        if(i != j){
           paste(rnames[i],"vs",rnames[j],"\n\t",
                 round(D[i,j],3),"of",
                 round(min(D,na.rm=TRUE),3),"--",
                 round(max(D,na.rm=TRUE),3),"\n")
        }
     }
  })

  getFusionEmbedding <- reactive({
     set.seed(input$seed)
     z <- NULL
     progress <- Progress$new(session,min=1,max=5)
     on.exit(progress$close())
     progress$set(message = 'Computing Embedding and clustering',
                        detail='This may take a while...')
     g <- gGraph()
     progress$set(value=2)
     if(!is.null(g)){
        atts <- list.vertex.attributes(g)
        if(is.null(atts)){
           return(NULL)
        }
        x <- NULL
        for(att in atts){
           y <- get.vertex.attribute(g,att)
           if(is.numeric(y)){
              cat("using attribute:",att,"\n")
              y[is.na(y)] <- 0
              x <- cbind(x,y)
           }
        }
        progress$set(value=3)
        if(is.null(x)) return(NULL)
        z <- fuseGraphCovariates(g,x,lambda=input$lambda,k=input$fusek,
                    no=input$Fd,
                    base.matrix=ifelse(input$fusion=="RDPG",
                                       "adjacency","laplacian"))
        progress$set(value=4)
        if(input$append) z <- cbind(z,x)
        z[is.na(z)] <- 0
     }
     z
  })

  output$fusionPlot <- renderPlot({  
     z <- getFusionEmbedding()
     if(!is.null(z)){
        progress <- Progress$new(session,min=1,max=4)
        on.exit(progress$close())
        progress$set(message = 'Computing Clustering',
                           detail='This may take a while...')
        progress$set(value=2)
        m <- Mclust(z,G=input$FG[1]:input$FG[2])
        progress$set(value=3)
        plot(z,pch=20,col=m$classification,main=paste(m$G,"clusters"),
             xlab=expression(x[1]),ylab=expression(x[2]))
     }
  })
  output$downloadState <- downloadHandler(
    filename = function() {
                     if(input$outputfilename==''){
                             f <- paste('ge_state-', Sys.time(),sep='')
                             f <- gsub(" [[:upper:]]{1,}$","",f)
                             f <- gsub(":|-|\\.|\\s","_",f)
                     } else {
                        f <- input$outputfilename
                     }
                     paste(f,'.RData',sep='')
    },
    content = function(con) {
        variables <- isolate(saveState(input))
        save(variables, file=con)
    }
  )

  observe({
     variables <<- loadState(session,input$saveFile,variables)
  })

  getAttData <- reactive({
     g <- gGraph()
     data <- NULL
     if(!is.null(g)){
        data <- attribute.conversion(g,list.vertex.attributes(g))
     }
     data
  })
  
  getVCor <- reactive({
     vc <- NULL
     g <- gGraph()
     if(!is.null(g)){
        z <- getAttData()
        if(!is.null(z)){
           data <- z$data
           vn <- z$varnames
           vc <- attribute.correlation(g,data,vn)
        }
     }
     vc
  })
  
  output$plotcor <- renderPlot({  
     vc <- getVCor()
     if(!is.null(vc)){
        corrplot(vc)
     }
  })
  output$plotecor <- renderPlot({  
     vc <- getVCor()
     if(!is.null(vc)){
        corrplot(vc$edge.cor)
     }
  })


})
