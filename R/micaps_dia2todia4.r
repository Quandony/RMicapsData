micaps_dia2todia4 <- function(data) {
  if(!is.matrix(data)) stop("input data must be a matrix")
  ncols <- ncol(data); nrows <- nrow(data)
  x1 <- data[1,1]; y1 <- data[1,2]
  dx <- data[2,1]-data[1,1]
  x2 <- data[nrows,1]; y2 <- data[nrows,2]
  x <- seq(x1,x2,dx)
  nx <- length(x)
  if(nrows%%nx!=0) {print("samples not be divided by nx")} else {
    ny <- as.integer(nrows/nx)
    y <- seq(y1,y2,length.out = ny)
    outdata <- array(data[,3:ncols],dim=c(nx,ny,ncols=2))
    if(y2<y1) {
      y <- y[ny:1]
      outdata <- outdata[,ny:1,]
    }
  }
  list(outdata=outdata,x=x,y=y)
}