# HW 6 - 2019313106 Hyungyeong Hong

## preparation
set.seed(2019313106)

veclength <- rbinom(1,40,0.5) # common length of x and y
x <- rpois(veclength,7) # sampling x
y <- rpois(veclength,1) # sampling y

## first plot
par(mfrow = c(1,2)) # two plots should be in a single frame

# determining ylim (in order to add texts)
if (max(x)>=max(y)){
  ylim_max <- max(x)
} else {
  ylim_max <- max(y)
}

if (min(x)<=min(y)){
  ylim_min <- min(x)
} else {
  ylim_min <- min(y)
}

# plotting
plot(1:length(x), x, xlim=c(0,veclength+1), ylim=c(ylim_min-1,ylim_max+1), xlab="Length of Vectors", ylab="Vectors", main="Scatter plot of X & Y vectors", pch=20) # points of x
points(1:length(y), y, pch=20) # points of y


points(1:length(x), x, pch=1, cex=2, col="red") # indicating the points of x
points(1:length(y), y, pch=3, cex=2, col="blue") # indicating the points of y

# determining maximum points and adding texts
maxfunc <- function(numvec){
  maxidx <- which(max(numvec)==numvec)
  for (i in 1:length(maxidx)){
    if (identical(numvec,x)){ # if the argument is x
      text(x=maxidx[i], y=numvec[maxidx[i]], labels=sprintf("max of x: %d", numvec[maxidx[i]]), pos=3, offset=0.7, cex=0.7, col="red")
    } else { # if the argument is y
      text(x=maxidx[i], y=numvec[maxidx[i]], labels=sprintf("max of y: %d", numvec[maxidx[i]]), pos=3, offset=0.7, cex=0.7, col="blue")
    }
  }
}

maxfunc(x)
maxfunc(y)

## second plot
boxplot(x, y, ylim=c(ylim_min-1,ylim_max+3), names=c("X","Y"), col=c("red","blue"), main="Box plot of X & Y vectors")

# finding quartiles
q1_x <- summary(x)[2]
q2_x <- summary(x)[3]
q3_x <- summary(x)[5]

q1_y <- summary(y)[2]
q2_y <- summary(y)[3]
q3_y <- summary(y)[5]

# adding texts
text(1,ylim_max+1.5,label=sprintf("1st Q: %.1f\nmedian: %.1f\n3rd Q: %.1f\n", q1_x,q2_x,q3_x), cex=0.7)
text(2,ylim_max+1.5,label=sprintf("1st Q: %.1f\nmedian: %.1f\n3rd Q: %.1f\n", q1_y,q2_y,q3_y), cex=0.7)

# adding points
for (i in 1:length(table(x))){
  points(1,as.numeric(labels(table(x))$x)[i],pch=20,cex=table(x)[i])
}

for (i in 1:length(table(y))){
  points(2,as.numeric(labels(table(y))$y)[i],pch=20,cex=table(y)[i])
}

