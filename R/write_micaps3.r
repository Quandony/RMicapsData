#***********write Micaps3 data****************************
write_micaps3 <- function(mdata,prefile,ft=NULL,mymdh,mtitle,sta) {
	mdata[is.na(mdata)] <- 9999
	if(!is.null(ft)) {
		cft <- formatC(ft,format="d",width=3,flag="0")
		mfile <- paste(prefile,(mymdh[1]-2000)*1000000+mymdh[2]*10000+mymdh[3]*100+mymdh[4],".",cft,sep="")
		for (i in 1:length(ft)) {
			write.table(paste("diamond 3 ",mymdh[1]*10000+mymdh[2]*100+mymdh[3],mtitle,sep=""),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE)
			write.table(paste(mymdh[1]-2000,mymdh[2],mymdh[3],mymdh[4],1000),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
			write.table(paste(0,0,0,0),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
			temp <- length(dim(mdata))
			mdata <- round(mdata*10)/10
			#mdata <- round(mdata)
#			write.table(matrix(c(dim(mdata)[temp],dim(mdata)[1]),1,2),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
			if(temp==2) {write.table(matrix(c(1,dim(mdata)[1]),1,2),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
				write.table(cbind(sta,mdata[,i]),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)}
			if(temp==3) {write.table(matrix(c(dim(mdata)[temp],dim(mdata)[1]),1,2),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
				write.table(cbind(sta,mdata[,i,]),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)}
		}
	}
	else {
		write.table(paste("diamond 3 ",mymdh[1],"year",mymdh[2],"month",mymdh[3],"day",mymdh[4],"hour",mtitle,sep=""),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE)
		write.table(paste(mymdh[1]-2000,mymdh[2],mymdh[3],mymdh[4],1000),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
		write.table(paste(0,0,0,0),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
		write.table(matrix(c(dim(mdata)[2],dim(mdata)[1]),1,2),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
		write.table(cbind(sta,mdata),mfile[i],col.names=FALSE,row.names=FALSE,quote=FALSE,append=TRUE)
	}
}