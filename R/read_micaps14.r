read_micaps14 <- function(ffile,sta=NULL) {
  out <- NA
  if(!file.exists(ffile)) {print(paste(ffile,"not exist"))} else {
    tem <- scan(ffile,what="character",quiet=TRUE)
    temN <- length(tem)
    index1 <- c(1:temN)[tem=="STATION_SITUATION"]
    index2 <- c(1:temN)[tem=="WEATHER_REGION:"]
    if(length(index2)==0) index2 <- temN+1
    if(length(index1)==0) index1 <- index2-2
    if(index1<index2-2) {
      tem <- tem[(index1+1):(index2-1)]
      tem <- as.numeric(tem)
      tem <- matrix(tem,2,length(tem)/2)
      tem <- aperm(tem)
      out <- tem[,2]
      if(!is.null(sta)) {
        temout <- rep(-1,length(sta))
        sta <- sort(sta)
        tem <- tem[order(tem[,1]),]
        temout[sta%in%tem[,1]] <- tem[tem[,1]%in%sta,2]
        out <- temout
      }
    }
  }
  return(out)
}
