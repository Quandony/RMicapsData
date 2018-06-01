read_sur_plot <- function(ofile,staindex=NULL,ele=NULL,misvalue=9999) {
	if(is.null(ele)) {ele <- c(1:24); Nele <- 24} else {Nele <- length(ele)}	
	out <- matrix(NA,1,Nele)
	if(!file.exists(ofile)) {print(paste(ofile,"not exists!"))} else {
		odata <- scan(ofile,skip=2,na.strings=c("****","******"),quiet=TRUE)
		if(length(odata)%%24!= 0) {print(paste(ofile,"length of the data wrong!"))} else {
			Nobs <- length(odata)/24
			if(Nobs>1) {
				odata <- matrix(odata,24,Nobs); odata <- aperm(odata)
				odata <- odata[order(odata[,1]),]
				if(is.null(staindex)) {out <- odata[,ele]} else {
					if(any(diff(staindex)<=0)) {
						print("staindex is not increasing!")
						staindex <- staindex[order(staindex)]
					}
					out <- matrix(NA,length(staindex),Nele)
					odata <- odata[odata[,1] %in% staindex,]
					index <- c(2:nrow(odata))[diff(odata[,1])!=0]
					index <- c(1,index)
					odata <- odata[index,]
					if(length(odata)>=3) {odata <- matrix(odata,length(odata)/24,24)
					out[staindex %in% odata[,1],] <- odata[,ele]
					out[out==misvalue] <- NA}
				}
			} else {
				print(paste(ofile,"file error!"))
			}
		}
	}	
	out
}