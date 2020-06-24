library(gridExtra)
library(ggplot2)
install.packages("gridExtra")

startdate<-1961-1
d <- read.csv(file="C:/Users/Omer/Desktop/Mashine/belgium-gdp-growth-rate.csv", header=TRUE)
d$date<- ifelse(d$date != '12-31', d$date)
ggplot(data=d) +  geom_line(aes(date,Annual.Change), color="#6666CC") +  ylab("Annual.Change")

n <- length(d$date)
d1 <- d[1:n,]
ggplot(data=d1) +  geom_line(aes(date,Annual.Change), color="#6666CC") + xlab("date") + ylab("Annual.Change")

y <- d1[,2]
U <- data.frame(tau=(1:(n-1)),RSS=0)
for (tau in (1:(n-1))) {
  m1 <- mean(y[1:tau])
  m2 <- mean(y[(tau+1):n])
  m <- c(rep(m1,tau),rep(m2,(n-tau)))
  e <- y - m
  U[tau,2] <- sum(e^2)
}
tau.est <- which.min(U$RSS)

#print(c(tau.est, U.min))

m1 <- mean(y[1:tau.est])
m2 <- mean(y[(tau.est+1):n])
m <- c(rep(m1,tau.est),rep(m2,(n-tau.est)))
d1$e <- y - m
dm <- data.frame(x1=c(0,tau.est+0.5), x2=c(tau.est+0.5,n), y1=c(m1,m2), y2=c(m1,m2))

d1$date<-d1$date+startdate
date<-startdate

pl1 <- ggplot(data=d1) + geom_line(aes(date, y), color="#6666CC")  +
  geom_segment(aes(x=x1+startdate, y=y1, xend=x2+startdate, yend=y2), colour="green", data=dm, size=0.75) +ylab("Annual.Change")+
  geom_vline(xintercept = (tau.est+0.5)+startdate, color="red", size=1)

pl2 <- ggplot(data=d1) + geom_line(aes(date, e), color="#6666CC")  +
  geom_segment(aes(x=0+startdate,xend=n+startdate,y=0,yend=0), colour="green", data=dm, size=0.75) +ylab("Annual.Change(-mean)")
grid.arrange(pl1, pl2)


