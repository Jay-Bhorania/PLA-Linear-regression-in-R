x1 <- sample(1:100, 20, replace=F)
x2 <- sample(1:100, 20, replace=F)
x <- cbind(x1,x2)
Y <- ifelse(x2>0.5+x1,+1,-1)

plot(x,pch=ifelse(Y>0,"+","-"), xlim=c(1,100),ylim=c(1,100),cex=2)
abline(0.5,1)

distance.from.plane = function(z,w,b) {sum(z*w) + b}

classify.linear = function(x,w,b) {distances = apply(x, 1, distance.from.plane, w, b) 
+ return(ifelse(distances < 0, -1, +1))}

classify.linear(x,c(-1,1)/sqrt(2),-sqrt(2)/4)
Y

norm = function(x) {sqrt(sum(x * x))}

perceptron = function(x, y, learning.rate=1) {
  w = vector(length = ncol(x)) 
  b = 0 
  iter = 0 
  R = max(apply(x, 1, norm))
  error = TRUE 
  while (error) {
    error=FALSE 
    yc <- classify.linear(x,w,b)
    for (i in 1:nrow(x)) {
      if (y[i] != yc[i]) {
        w <- w + learning.rate * y[i]*x[i,]
        b <- b + learning.rate * y[i]*R^2
        iter <- iter+1
        #s = norm(w)
        #return(list(w=w/s,b=b/s,iterations=iter))
       error=TRUE} } }
  s = norm(w)
  return(list(w=w/s,b=b/s,iterations=iter))
  }

(p <- perceptron(x,Y))

y <- classify.linear(x,p$w,p$b)
sum(abs(Y-y))

#plot(x,pch=ifelse(Y>0,"+","-"), xlim=c(1,100),ylim=c(1,100),cex=2, )
#abline(0.5,1)
#abline(-0.6365139,0.7712652)
#abline(-0.99959209,-0.02855977)

