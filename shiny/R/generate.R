
generateGraph <- function(genMethod,parameters)
{
   set.seed(parameters$seed)
	n <- parameters$n
	directed=parameters$directed
	if(genMethod=='Erdos Renyi'){
		g <- sample_gnp(n,
		                p=parameters$p,
							 directed=directed)
	} else if(genMethod=='Barabasi'){
	   g <- sample_pa(n,
		               power=parameters$power,
							m=parameters$m,
							zero.appeal=parameters$zero,
							directed=directed)
	} else if(genMethod=='K Regular'){
	   g <- sample_k_regular(no.of.nodes=n,
		                      k=parameters$k,
		                      directed=directed)
	} else if(genMethod=='Ring'){
	   g <- make_ring(n,
		               directed=directed)
	} else if(genMethod=='Star'){
	   g <- make_star(n,
		               mode=parameters$mode)
	} else if(genMethod=='Wheel'){
	   h1 <- make_star(n,
		               mode=parameters$mode,center=n)
	   h2 <- disjoint_union(make_ring(n-1,directed=directed),
		                     make_empty_graph(1,directed=directed))
	   g <- union(h1,h2)
	} else if(genMethod=='Complete'){
	   g <- make_full_graph(n,
		                     directed=directed)
	} else if(genMethod=='Geometric Random Graph'){
	   g <- sample_grg(n,
		                radius=parameters$radius,
							 torus=parameters$torus,
							 coords=parameters$coords)
	} else if(genMethod=='Watts Strogatz'){
	   g <- simplify(sample_smallworld(dim=parameters$dim,
		                         size=parameters$size,
										 nei=parameters$nei,
										 p=parameters$p))
	} else if(genMethod=='Growing Random Graph'){
	   g <- sample_growing(n,
		                    m=parameters$m,
		                    citation=parameters$citation,
								  directed=directed)
	} else if(genMethod=='Complete Bipartite'){
	   g <- make_full_bipartite_graph(n,parameters$n2,
		                         mode=parameters$mode,
		                          directed=directed)
	} else if(genMethod=='Atlas'){
	   g <- graph_from_atlas(parameters$atlas)
	#} else if(genMethod=='Generated Worm'){
	#  library(rvest)
	#  page <- read_html("http://openconnecto.me/mrdata/static/graphs/fly/")
	#  dirs <- page %>% html_nodes("body") %>% html_nodes("a") %>% html_text()
	#  dirs <- dirs[-1]
	  
	 # files <- vector('list',length(dirs))
	 # for(i in 1:length(dirs)){
	 #   dir <- dirs[i]
	#    url <- paste("http://openconnecto.me/mrdata/static/graphs/fly/",dir,sep="/")
	 #   pagei <- read_html(url)
	#    files[[i]] <- pagei %>% html_nodes("body") %>% html_nodes("a") %>% html_text()
	#    files[[i]] <- paste0(url,files[[i]][-1])
	#  }
	  
	#  g <- read.graph(files[[1]][1],format='graphml')
	}else if(genMethod=='Tree'){
	   g <- make_tree(n,
		               children=parameters$children,
							mode=parameters$mode)
	} else if(genMethod=='Forest Fire'){
	   g <- sample_forestfire(n,
		                   fw.prob=parameters$fw.prob,
		                   bw.factor=parameters$bw.factor,
								 ambs=parameters$ambs,
		                   directed=directed)
	} else if(genMethod=='Aging Prefatt'){
	   print(parameters)
		x <<-parameters
	   g <- sample_pa_age(n=n,
		                   pa.exp=parameters$pa.exp,
		                   aging.exp=parameters$aging.exp,
		                   m=parameters$m,
								 aging.bin=parameters$aging.bin,
								 directed=directed,
								 zero.age.appeal=parameters$zero.age.appeal,
								 zero.deg.appeal=parameters$zero.deg.appeal,
		                   age.coef=parameters$age.coef,
								 deg.coef=parameters$deg.coef,
								 time.window=parameters$time.window)
	} else if(genMethod=='Stochastic Block Model'){
	   g <- sample_sbm(n,
		                pref.matrix=parameters$P,
							 block.sizes=parameters$sizes,
							 directed=directed,loops=FALSE)
	} else if(genMethod=='Random Bipartite'){
	   g <- sample_bipartite(n,
		                      parameters$n2,
									 type="gnp",
									 p=parameters$p,
									 directed=directed,
									 mode=parameters$mode)
	} else if(genMethod=='Random Dot Product'){
		if(parameters$Vect=='Dirichlet'){
		   vects <- sample_dirichlet(n,parameters$alpha)
		} else {
		   vects <- sample_sphere_volume(n=n,dim=parameters$dim)
		}
	   g <- sample_dot_product(vects,
									 directed=directed)
	} else if(genMethod=='Famous'){
	   g <- make_graph(parameters$famous)
	} else if(genMethod=='de Bruijn'){
	   g <- make_de_bruijn_graph(m=parameters$m,n=parameters$dbn)
	} else if(genMethod=='Kautz'){
	   g <- make_kautz_graph(m=parameters$m,n=parameters$kn)
	}
	g
}
