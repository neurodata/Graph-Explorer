

getOpenConnectome <- function()
{

	page <- read_html("http://openconnecto.me/mrdata/static/graphs/")
	dirs <- page %>% html_nodes("body") %>% html_nodes("a") %>% html_text()
	dirs <- dirs[-1]

	files <- vector('list',length(dirs))
	names(files) <- gsub("/","",dirs)
	for(i in 1:length(dirs)){
		dir <- dirs[i]
		url <- paste("http://openconnecto.me/mrdata/static/graphs",dir,sep="/")
		pagei <- read_html(url)
		files[[i]] <- pagei %>% html_nodes("body") %>% html_nodes("a") %>%
                     html_text()
		files[[i]] <- paste0(url,files[[i]][-1])
	}
	human <- grep('human',names(files))
	files <- c("",files[-human],files[human])
   files

}

ogetOpenConnectome <- function(openconnectome.dir)
{
   return(list(worm="Offline"))
   cat("Getting the openconnecto.me list of graphs\n")
   a <- try(scrape(openconnectome.dir),silent=TRUE) 
   if(inherits(a,'try-error')){
      cat("Could not access openconnecto.me.")
      return(list(worm="Offline"))
   }
   if(xpathSApply(a[[1]],'//title',xmlValue)=="403 Forbidden"){
      cat("Could not access openconnecto.me.")
      return(list(worm="Offline"))
   }
   ## get the first level of directories -- the species
   dirs <- xpathSApply(a[[1]],"//table//td/a",xmlValue)
   dirs <- dirs[grep('Parent Directory',dirs,invert=TRUE)]
   tree <- vector('list',length(dirs))
   h <- which(dirs=='human/')
   dirs <- c(dirs[-h],'human/')
   names(tree) <- gsub("/","",dirs)
   for(i in 1:length(dirs)){
      dir <- dirs[i]
      b <- scrape(paste(openconnectome.dir,dir,sep="/"))
      graphs <- xpathSApply(b[[1]],"//table//td/a",xmlValue)
      graphs <- graphs[grep('Parent Directory',graphs,invert=TRUE)]
      tree[[i]] <- graphs
   }
   cat("Done\n")
   tree
}
