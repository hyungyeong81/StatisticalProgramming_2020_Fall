#HW4_2019313106_Hyungyeong Hong

#codes for the first question

mat_mul <- function(x,y){ #has two matrices as arguments
  if (ncol(x) != nrow(y)){
    return("Error: check the dimensions of the matrices") #termination
  } else {
    matrix_new <- matrix(NA,nrow(x),ncol(y)) #setting the format of the matrix
    for (i in  1:nrow(x)){ #setting the range of the computation
      for (j in 1:ncol(y)){
        element_new <- sum(x[i,]*y[,j]) #computing a matrix element (ith row and jth column)
        matrix_new[i,j] <- element_new #adding the matrix element
      }
    }
    return(matrix_new)} #output
}

A <- matrix(c(2,3,4,1,2,1),2,3)
B <- matrix(c(3,1,-2,1,0,1),3,2)
C <- matrix(c(1,4,4,5,2,2),2,3)
D <- matrix(c(2,0,-1,0,2,1),2,3)

mat_mul(A,B) #answer1
mat_mul(C,D) #answer2



#codes for the second question

uniquefunc <- function(x){
  
  if (length(x) <= 1) {
    return(x) #termination
  }
  
  comparison <- x[1]
  remaining <- x[-1] #the elements of this vector will be compared with the element "comparison"
  
  same_elements <- remaining[remaining == comparison] #duplicated elements, this part is actually unnecessary
  different_elements <- remaining [remaining != comparison] #unique elements
  
  different_elements <- uniquefunc(different_elements) #recursion
  
  return(c(comparison, different_elements)) #output
}

a <- c(4,1,1,5,4)
b <- c("b","c","b","c","c","a")
x <- c(2,2,5,2,4,5,4,2)

uniquefunc(a)
uniquefunc(b)
uniquefunc(x) #answer
