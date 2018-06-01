read_ens <- function(cffile,pffile) {
	days <- 15; Nens <- 50
	nx <- 125; ny <- 73
	if(file.exists(cffile) & file.exists(pffile)) {
		cf.read <- file(cffile,'rb')
		pf.read <- file(pffile,'rb')
		cftem <- readBin(cf.read,'double',n=nx*ny*days,size=4)
		cftem <- matrix(cftem,nx*ny,days)
		cftem <- aperm(cftem)
		cftem <- rbind(cftem[1,],diff(cftem))
		cftem <- aperm(cftem)
		pftem <- readBin(pf.read,'double',n=nx*ny*Nens*days,size=4)
		pftem <- matrix(pftem,nx*ny*Nens,days)
		pftem <- aperm(pftem)
		pftem <- rbind(pftem[1,],diff(pftem))
		pftem <- aperm(pftem)
		close(cf.read)
		close(pf.read)
		cptem <- rbind(cftem,pftem)
		rm(cftem,pftem)
		cptem[cptem<0] <- 0
	}
	else {cptem <- NA}
	cptem
}
