library(fields)
read_micaps34 <- function(filename,errorValue=99999.9,Nchar=NULL) {
	out <- 1
	if(!file.exists(filename)) {
		print(paste(filename,"does not exist!"))
		out <- NA
	}
	if(file.exists(filename)) {
		mhead <- scan(filename,what="character",nlines=1,quiet=TRUE)
		mhead <- as.integer(mhead[[2]])
		if(mhead!=3 & mhead!=4) {print(paste(filename,"diamond is not 3 or 4!")); out <- rep(NA,as.integer((length(xy)+1)/2))}
		if(mhead==3) {
			N <- scan(filename,skip=2,nlines=1,quiet=TRUE)
			N <- N[2]
			model <- scan(filename,skip=3,quiet=TRUE)
			if(length(model)!=N*5) print(paste(3,filename))
			model <- matrix(model,5,N); model <- aperm(model)
			model <- model[,c(2,3,5)]
			x1 <- model[1,1]; y1 <- model[1,2]
			dx <- model[2,1]-model[1,1]; dy <- dx; y2 <- model[N,2]
			seqy <- seq(min(y1,y2),max(y1,y2),dy); ny <- length(seqy)
			nx <- N/ny
			x2 <- x1+nx*dx-dx
			seqx <- seq(x1,x2,dx)
			model <- model[,3]
			model <- matrix(model,nx,ny)
			if(y1>y2) model[,1:ny] <- model[,ny:1]
			model[abs(model)>errorValue] <- NA
		}
		if(mhead==4) {
			hpara <- scan(filename,what="character",skip=1,nlines=1,quiet=TRUE)
			sk <- 1
			if(hpara[[1]]=="diamond") sk <- 2
			hpara <- scan(filename,skip=sk,nlines=1,quiet=TRUE)
			dx <- hpara[[7]]; dy <- hpara[[8]]
			x1 <- hpara[[9]]; x2 <- hpara[[10]]
			y1 <- hpara[[11]]; y2 <- hpara[[12]]
			nx2 <- hpara[[13]]; ny <- hpara[[14]]
			seqx <- seq(x1,x2,dx)
			seqy <- seq(min(y1,y2),max(y1,y2),abs(dy))
			nx1 <- length(seqx)
			if(is.null(Nchar)) {
				model <- scan(filename,skip=sk+1,quiet=TRUE)
				if(length(model)!=nx2*ny & length(model)!=nx1*ny) print(paste(4,filename))
			} else {
				model0 <- scan(filename,skip=sk+1,quiet=TRUE,what=character())
				tem <- nchar(model0); naN <- c(1:length(model0))[tem>10]
				N <- length(naN)
				if(N>0) {
					model <- numeric()
					for (i in 1:N) {
						if(i==1) {model <- c(model,model0[1:(naN[1]-1)])} else {
							if(naN[i]-naN[i-1]>1) model <- c(model,model0[(naN[i-1]+1):(naN[i]-1)])
						}
						tem <- substr(model0[naN[i]],1,Nchar)
						if(substr(tem,2,4)=="999") tem <- "99999.9"
						model <- c(model,tem,"99999.9")
					}
					model <- c(model,model0[(naN[N]+1):length(model0)])
				} else {model <- model0}
				model <- as.numeric(model)
				model[model==errorValue] <- NA
				index <- c(1:length(model))[is.na(model)]
				for (i in index) {
					model[i] <- (model[i-1]+model[i+1])/2
					if(is.na(model[i])) model[i] <- (model[i-2]+model[i+2])/2
					if(is.na(model[i])) model[i] <- (model[i-3]+model[i+3])/2
				}
			}
			tem <- length(model) %% ny
      temmax <- max(model,na.rm=TRUE)
      if(tem==0 & any(model!=temmax)) {
        nx0 <- length(model)/ny
        model <- matrix(model,nx0,ny)
        seqx <- seq(x1,by=dx,length.out=nx0)
        if(y1>y2) {model[,1:ny] <- model[,ny:1]; dy <- dy*(-1)}
        model[abs(model)>errorValue] <- NA
      } else {model <- rep(NA,10)}
		}
#		if(length(xy)>2 & !is.null(method) & !is.na(out[1])) {
#			out <- interplo(model,xy,x=seqx,y=seqy,method="near")
#		}
#		else {out <- model}
		list(outdata=model,x=seqx,y=seqy)
	}
}
