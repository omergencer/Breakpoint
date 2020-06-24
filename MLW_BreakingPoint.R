library(gridExtra)
library(ggplot2)
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("gridExtra")) install.packages("gridExtra")
setwd("C:/Users/Omer/Desktop/Mashine/")##path where all datasets are stored

startdate<-1961# enter the starting date of data set



run<-function(){
  d <- read.csv(file="./belgium-gdp-growth-rate.csv", header=TRUE)##"./turkey-gdp-growth-rate.csv"
  d$date<- ifelse(d$date != '12-31', d$date)
  
  bool=TRUE
  startdate<-startdate-1
  maxSegLenght <- 10 # maximum number of segments
  minSegLenght <- 3  # minimum length of a segment
  Segment_Number <- 10
  
  
  n <- length(d$date)
  d1 <- d[1:n,]
  r <- calculation(d$Annual.Change,maxSegLenght,minSegLenght)
  y <- d$Annual.Change
  n <- length(y)
  Topt <- c(0,r$Result[(Segment_Number-1),1:(Segment_Number-1)],n)
  Tr <- c(0,Topt[2:Segment_Number]+0.5,n)
  dm <- data.frame()
  
  for (k in (1:Segment_Number)) {
    m <- mean(y[(Topt[k]+1):Topt[k+1]])
    dm <- rbind(dm, c(Tr[k],Tr[k+1],m,m))
  }
  names(dm) <- c("x1","x2","y1","y2")
  
  if(bool==TRUE){
    d$date<-d$date+startdate
    date<-startdate
    bool=FALSE
  }
  
  pl1 <- ggplot(data=d) + geom_line(aes(date, y), color="#6666CC")  +ylab("Annual.Change")  +xlab("Date")+
    geom_segment(aes(x=x1+startdate, y=y1, xend=x2+startdate, yend=y2), colour="green", data=dm, size=0.75)+
    geom_vline(xintercept = Tr[2:Segment_Number]+startdate, color="red", size=0.25)
  
  pl2 <- ggplot(data=d1) + geom_line(aes(date+startdate, y), color="#6666CC")  +xlab("Date")+
    geom_segment(aes(x=0+startdate,xend=n+startdate,y=0,yend=0), colour="green", data=dm, size=0.75) +ylab("Annual.Change")
  grid.arrange(pl1, pl2)
  
  
  
  
}


calculation <- function(y, Kmax, Lmin=3) {
  Nr  <- Kmax - 1
  lengthOf_y <- length(y)
  my_matrix <- matrix(Inf, nrow = lengthOf_y, ncol = lengthOf_y)
  for (j1 in (1:(lengthOf_y-Lmin+1))){
    for (j2 in ((j1+Lmin-1):lengthOf_y)) {
      yj <- y[j1:j2]
      nj <- j2-j1+1
      my_matrix[j1,j2] <- sum(yj^2) - (sum(yj)^2)/nj
    }
  }
  
  my_Vec <- vector(length=Kmax)
  my_Vec[1] <- my_matrix[1,lengthOf_y]
  D <- my_matrix[,lengthOf_y] 
  Pos <- matrix(nrow = lengthOf_y, ncol = Nr) 
  Pos[lengthOf_y,] <- rep(lengthOf_y,Nr)    
  tau.mat <- matrix(nrow = Nr,ncol = Nr) 
  for (k in 1:Nr){
    for (j in 1:(lengthOf_y-1)){
      dist <- my_matrix[j,j:(lengthOf_y-1)] + D[(j+1):lengthOf_y]
      D[j] <- min(dist)
      Pos[j,1] <- which.min(dist) + j
      if (k > 1) { Pos[j,2:k] <- Pos[Pos[j,1],1:(k-1)] }
    }
    my_Vec[k+1] <- D[1]
    tau.mat[k,1:k] <- Pos[1,1:k]-1
  }
  out <- list(Result=tau.mat, nesne=data.frame(K=(1:Kmax),my_Vec=my_Vec))
              return(out)
}


run()
