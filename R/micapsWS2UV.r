micapsW2UV <- function(w,grid=FALSE) {
  library(abind)
  if(!is.matrix(w)) {outdata <- w; x <- NA; y <- NA; print("the input in micapsW2UV is not matrix!")}
  u <- w[,10]*(-1)*sin(2*pi*w[,9]/360)
  v <- w[,10]*(-1)*cos(2*pi*w[,9]/360)
  if(!grid) {outdata <- w; outdata[,9:10] <- cbind(u,v); x <- NA; y <- NA} else {
    x1 <- w[1,2]; y1 <- w[1,3]
    N <- nrow(w)
    outx <- w[w[,3]==y1,2]
    #x2 <- w[N,2]; y2 <- w[N,3]
    y2 <- w[N,3]
    dx <- w[2,2]-x1; dy <- dx
    #outx <- seq(x1,x2,dx)
    outy <- seq(min(y1,y2),max(y1,y2),dy)
    nx <- length(outx); ny <- length(outy)
    if(nx*ny!=N) {
      #outdata <- w; x <- NA; y <- NA
      temN <- nx*ny-N
      u <- c(u,rep(NA,temN)); v <- c(v,rep(NA,temN))
      print("In micapsW2UV dx is not equal to dy!")
    }
    u <- matrix(u,nx,ny); v <- matrix(v,nx,ny)
    if(y1>y2) {u[,1:ny] <- u[,ny:1]; v[,1:ny] <- v[,ny:1]}
    outdata <- abind(u,v,along=3)
    x <- outx; y <- outy
  }
  list(outdata=outdata,x=x,y=y)
}
