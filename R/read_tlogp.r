# micaps第五类数据格式 tlogp探空数据
# 1区站号（长整数）  2经度  3纬度  4拔海高度（均为浮点数）5单站内容长度（整数）
# 第一层气压  高度  温度  露点 风向  风速  第二层气压      
# 除风向风速外缺值时整个层次取消掉，风向风速缺值用9999表示
# 注：单站内容长度为层数×6

read_tlogp <- function(ofile,staindex=NULL,lev=NULL,misvalue=9999) {
	#if(is.null(ele)) {ele <- c(1:NCOL); Nele <- NCOL} else {Nele <- length(ele)}	
	out <- list()
	if(!file.exists(ofile)) {print(paste(ofile,"not exists!"))} else {
		fhead <- scan(ofile,what=character(),nlines=1,quiet=TRUE)
		if(fhead[1]!="diamond" | fhead[2]!="5") stop(paste(ofile,"is wrong!"))
		Nobs <- scan(ofile,what=numeric(),nlines=1,skip=1,quiet=TRUE)
		Nobs <- Nobs[5]
		Nskip <- 2
		Nout <- 1
		for (i in 1:Nobs) {
			tem <- scan(ofile,what=numeric(),nlines=1,skip=Nskip,quiet=TRUE)
			if(length(tem)==0) break
#			print(c(i,tem))
			Nrecord <- tem[5]; staNum <- as.integer(tem[1])
			if(Nrecord %% 6 !=0) stop(paste(ofile,"record error!"))
			Nrecord <- Nrecord/6
			Nskip <- Nskip+1
			temNlines <- as.integer((Nrecord+1)/2)
			if(is.null(staindex) | sum(staNum %in% staindex)>0) {
				tem <- scan(ofile,what=numeric(),nlines=temNlines,skip=Nskip,na.strings=c("****","******"),quiet=TRUE)
				tem <- matrix(tem,6,Nrecord); tem <- aperm(tem)
				if(!is.null(lev)) {
					tem2 <- matrix(NA,length(lev),6); tem2[,1] <- lev
					tem <- tem[tem[,1] %in% lev,]
					if(dim(tem)[1]!=length(lev)) {
						index <- diff(tem[,1])!=0
						index <- c(index,TRUE)
						tem <- tem[index,]
					}
					tem2[lev %in% tem[,1],] <- tem
					tem <- tem2
				}
				out[[Nout]] <- tem; names(out)[Nout] <- staNum
				Nout <- Nout+1
			}
			Nskip <- Nskip+temNlines
		}
	}
	out
}