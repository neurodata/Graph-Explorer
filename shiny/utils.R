
html2txt <- function(str) {
		require(XML)
		if(nchar(str,type="bytes")==0) return(str)
		str <- paste("<html>",str,"</html>",sep="")
		paste(unlist(
			xpathApply(htmlParse(str, asText=TRUE),
						  "//body//text()", 
						  xmlValue)),
	      sep="\n",collapse="\n")
}

computeInvariants <- function(g,invariantList)
{
   cat("Computing Invariants:\n")
   inv <- data.frame(Invariants=invariants$Invariant,
                     Values=rep(0.0,nrow(invariants)),
                     Timings=rep(0.0,nrow(invariants)),
                     stringsAsFactors=FALSE)
   ind <- which(invariants$Invariant %in% invariantList)
   inv <- inv[ind,,drop=FALSE]
   invl <- invariants[ind,,drop=FALSE]
   for(i in 1:nrow(invl)){
      cat("Computing",invl$Function[i],"\n")
      t1 <- system.time(inv[i,2] <- 
          round(do.call(invl$Function[i],args=list(g=g))),3)
      inv[i,3] <- round(t1['elapsed'],3)
      cat("\t",inv[i,2],inv[i,3],"\n")
   }
   datatable(inv,rownames=FALSE)
}

## code for invariant calculations
## these must take a graph and return a single number

max.component.size <- function(g){
   max(clusters(g)$csize)
}

get.girth <- function(g) girth(g)$girth
get.cent <- function(g) centralization.degree(g)$centralization
get.rec <- function(g){
   if(is.directed(g)) return(reciprocity(g))
   else return(NA)
}
get.fastgreedy <- function(g) length(fastgreedy.community(
            as.undirected(simplify(g))))

get.pl <- function(g) power.law.fit(
             degree.distribution(g))$alpha
get.core <- function(g) max(graph.coreness(g))
get.avg.degree <- function(g) mean(degree(g))
get.max.degree <- function(g) max(degree(g))
get.min.degree <- function(g) min(degree(g))
get.var.degree <- function(g) var(degree(g))
dominate.num.greedy <- function(g) length(dominate.greedy(g))

