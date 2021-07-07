#2019313106 HW_1

# Codes for the first question
x <- c(5,4,3,-5,-6,-1,3,1,0,5); x #generating the vector x
y <- x[x > 3]; y #y is a vector with values satisfying x > 3
z <- which(x == 3); z #z is a vector with indexes satisfying x == 3
v <- x != 3; v #v is a vector with logical values by whether x != 3 (TRUE means x != 3)

#Codes for the second question
set.seed(1)
x = rnorm(100,mean=90,sd=15)
#2-(1): computing the mean of values in the vector x
sum(x) / length(x)
#2-(2): computing the standard deviation of values in the vector x
a <- sum(x) / length(x) #setting a scalar object that corresponds to the mean of x
sqrt(sum((x - a)^2) / (length(x) - 1)) #computing the standard deviation of x
#2-(3): finding the index position in x of the values less than 70 and larger than 90
y <- c(which(x<70),which(x>90)) #setting the vector y as the indexes of elements satisfying the given conditions
sort(y) #sorting the indexes satisfying the given conditions in increasing order
#2-(4): obtaining the vector x without the values corresponding to the index positions found in 2-(3)
x <- x[-y]; x
#2-(5): adding a vector (1,2,3,4,5) to the end of x from 2-(4)
c(x,1:5)

#Codes for the third question
set.seed(1)
x = rnorm(100,mean=50,sd=1)
#3-(1)
y <- (x^3) + (1.5*(x^2)) #setting a vector y that represents the formula inside the sigma notation, by using element-wise operations
z <- y[-(1:9)] #setting a sub vector of y to calculate the sum of y from index #10 to index #100
sum(z) #calculating the sum
#3-(2)
a <- ((2^x)/x) + (3/(x^3)) #setting a vector a that represents the formula inside the sigma notation, by using element-wise operations
b <- a[1:25] #setting a sub vector of a to calculate the sum of a from index #1 to index #25
sum(b) #calculating the sum
#3-(3)
c <- 2*(x^2) + 3*(x^(1/2)) #setting a vector c that represents the formula inside the sigma notation, by using element-wise operations
d <- c[2:10] #setting a sub vector of c in order to calculate the product of the vector elements of c from index #2 to index #10
prod(d) #calculating the product
#3-(4)
x <- c(3,2,1,8,8,9); y <- c(1,0,4,2,0,1) #setting the given vector x and y
z <- (x-y)^2 #setting a vector z in order to simplify upcoming calculation code
sqrt(sum(z)) #computing the Euclidean distance between x and y

#Codes for the fourth question
a <- c(1,8,3,1,4,3,3,15,4,6,5,10,6,2,1,1,9,2,5,4,4,4,3,2,1) #generating a vector a
A <- matrix(a,5,5) #generating the given matrix A by using the vector a
#4-(1): counting the numbers of rows and columns
nrow(A) #the number of rows
ncol(A) #the number of columns
#4-(2): obtaining the inverse of A
solve(A)
#4-(3): showing that the matrix product is an identity matrix
B <- solve(A) #assigning the inverse matrix of A as B
I <- matrix(0,5,5) #setting a matrix format in order to generate an 5*5 identity matrix
I[1,1] <- 1; I[2,2] <- 1; I[3,3] <- 1; I[4,4] <- 1; I[5,5] <- 1 #replacing the elements on the main diagonal to 1's
round(A %*% B) == round(B %*% A) #comparison by using logical operator
round(A %*% B) == I #comparison by using logical operator
round(B %*% A) == I #comparison by using logical operator
#4-(4): assigning column names #The paper posted for the HW announcement says that this is question 4-(3)
colnames(A) <- c("a","b","c","d","e"); A
#4-(5): matrix indexing
A[A[ ,1] >= 1 & A[ ,1] <= 3, 3]
#4-(6): transforming the matrix A into a vector object a
a <- as.vector(A); a
#4-(7): assigning the column names
colnames(A) <- c("name 1","name 2","name 3","name 4","name 5"); A
#4-(8): replacing the second column of A
A[ ,2] <- 1:5; A

#Codes for the fifth question
A <- matrix(NA,500,3) #generating a matrix format of A
A[ ,-2] <- 1.5; A[ ,2] <- -0.5 #replacing the elements by matrix indexing
#5-(1)
B <- t(A) #assigning B as the transpose of A
B %*% A #matrix multiplication
#5-(2): computing the mean of each column of A
colMeans(A) #same as: apply(A,2,mean)
#5-(3): computing the sum of each row of A
rowSums(A) #same as: apply(A,1,sum)