read_micaps_r24 <- function(prefile,maxfd=10,bin=24,initFt=24) {
	out <- numeric()
	temft <- seq(initFt,maxfd*24,bin)
	for (i in temft) {
		cft <- formatC(i,format="d",width=3,flag="0")
		mfile <- paste(prefile,".",cft,sep="")
		tem <- read_micaps34(mfile)
		out <- cbind(out,as.vector(tem$outdata))
	}
	list(outdata=out,x=tem$x,y=tem$y)
}
