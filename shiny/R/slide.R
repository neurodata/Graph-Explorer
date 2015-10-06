slideFunct <- function(data, window, step, FUN=mean){
   total <- length(data)
   spots <- seq(from=1, to=(total-window), by=step)
   result <- vector(length = length(spots))
   for(i in 1:length(spots)){
      result[i] <- do.call(FUN,list(x=data[spots[i]:(spots[i]+window)]))
   }
   return(result)
}
