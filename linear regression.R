x1 <- sample(1:100, 20, replace=F)
x2 <- sample(1:100, 20, replace=F)
x <- cbind(x1,x2)

plot(x, xlim=c(1,100),ylim=c(1,100),cex=2)
abline(lm(x1~x2))
       
abline(70,-0.55)



