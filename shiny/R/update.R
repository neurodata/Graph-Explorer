
updateFlashRPackages <- function(file='global.R',force=FALSE)
{
   a <- readLines(file)
   x <- '(library)|(require)'
   b <- grep('(library)|(require)',a,value=TRUE)
   a <- gsub(x,"",b)
   b <- grep("#",gsub("[()]","",a),value=TRUE,invert=TRUE)
   if(length(b)>0) {
		sapply(b,function(x){
		if(force | !require(x,character.only=TRUE)){
			cat("Installing:",x,"\n")
			install.packages(x)
		}})
	}
}
