read_r24all <- function(ofile,sta=NULL) {
	Nsta <- length(sta)
	r24 <- scan(ofile,skip=11,quiet=TRUE,na.strings=c("****","*****","******"))
	r24 <- matrix(r24,5,length(r24)/5)
	r24 <- aperm(r24)
	if(!is.null(sta)) {
		r24all <- rep(0,Nsta)
		r24 <- r24[order(r24[,1]),]
		index <- diff(r24[,1])>0
		index <- c(TRUE,index)
		r24 <- r24[index,]
		r24 <- r24[r24[,1] %in% sta,]
		r24 <- matrix(r24,length(r24)/5,5)
		r24all[sta %in% r24[,1]] <- r24[,5]
		r24all[r24all<0] <- 0
	} else {r24all <- r24}
	r24all
}