# micaps diamond 2
# 1 StaNumber  2 Longitude  3 Latutide  4 Altitude 5 StaGrade  6 Height 7 Temperature 8 Diff. between DewPoint and Temp. 9 WindDeriction  10 WindSpeed
read_micaps2 <- function(ofile,staindex=NULL,ele=NULL,misvalue=9999) {
	NCOL <- 10
	if(is.null(ele)) {ele <- c(1:NCOL); Nele <- NCOL} else {Nele <- length(ele)}	
	out <- matrix(NA,1,Nele)
	if(!file.exists(ofile)) {print(paste(ofile,"not exists!"))} else {
		fhead <- scan(ofile,what=character(),nlines=1,quiet=TRUE)
		if(fhead[1]!="diamond" | fhead[2]!="2") stop(paste(ofile,"is wrong!"))
		odata <- scan(ofile,skip=2,na.strings=c("****","******"),quiet=TRUE,what=character())
		tem <- nchar(odata); tem <- tem>8
		if(sum(tem)>10) {
			out <- rep(NA,10)
			print(paste(ofile,"more missings!"))
		} else {
			if(sum(tem)>0) {
				naN <- c(1:length(odata))[tem]
				N <- length(naN)
				odata0 <- numeric()
				for (i in 1:N) {
					if(i==1) {odata0 <- c(odata0,odata[1:(naN[1]-1)])} else {
						if(naN[i]-naN[i-1]>1) odata0 <- c(odata0,odata[(naN[i-1]+1):(naN[i]-1)])
					}
					tem <- odata[naN[i]]
					if(nchar(tem)>16) {stop(paste(ofile,"more ***** in file!"))} else {
						#tem <- substr(tem,1,5)
						tem <- gsub("\\*.*\\*","",tem)
						if(substr(tem,2,3)=="**") tem <- NA
						odata0 <- c(odata0,tem,NA)
					}
				}
				odata0 <- c(odata0,odata[(naN[N]+1):length(odata)])
				odata <- odata0
			}
			odata <- as.numeric(odata)
			Nobs <- length(odata)/NCOL
			if(Nobs>1) {
				odata <- matrix(odata,NCOL,Nobs); odata <- aperm(odata)
				x <- c(1:nrow(odata)); y <- c(1:ncol(odata))
				xy <- expand.grid(x=x,y=y)
				xy <- xy[is.na(as.vector(odata)),]
				odata[xy[,1],xy[,2]] <- (odata[xy[,1]+1,xy[,2]]+odata[xy[,1]-1,xy[,2]])/2
				if(is.null(staindex)) {out <- odata[,ele]} else {
					out <- matrix(NA,length(staindex),Nele)
					odata <- odata[odata[,1] %in% staindex,]
					out[staindex %in% odata[,1],] <- odata[,ele]
					out[out==misvalue] <- NA
				}
			} else {
				print(paste(ofile,"file error!"))
			}
		}
	}	
	out
}