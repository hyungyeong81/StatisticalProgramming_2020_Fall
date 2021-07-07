#Codes for the first question
set.seed(1)
x <- rnorm(100, 1, 2) #given vector

Q1 <- function(x){
  min_value <- sort(x)[1] #minimum value of x
  max_value <- sort(x, decreasing=T)[1] #maximum value of x
  mean_value <- sum(x)/length(x) #mean of x
  var_value <- sum((x-mean_value)^2) / (length(x)-1) #variance of x (not included in the result list)
  sd_value <- sqrt(var_value) #standard deviation of x
  result <- list(MIN=min_value, MAX=max_value, MEAN=mean_value, SD=sd_value) #multiple outputs 
  return(result)
}
Q1(x)

#Codes for the second question

Q2 <- function(a,b,na.rm=F){
  
  datf_ab <- as.data.frame(cbind(a,b)) #combining two vectors into a data frame / used cbind() to deal with the difference in length between two input vectors (just in case)

  #dealing with NA
  if(na.rm==T){
    datf_ab <- datf_ab[complete.cases(datf_ab),] #removing rows with NA
    a <- datf_ab[,1] #removing NA values from a
    b <- datf_ab[,2] #removing NA values from b
  }
  
  #sample size of each vector
  sample_size_list <- lapply(datf_ab, length)
  
  #mean value of each vector
  sum_list <- lapply(datf_ab, sum) #sum of each vector
  mean_value_a <- sum_list$a / sample_size_list$a #mean of a
  mean_value_b <- sum_list$b / sample_size_list$b #mean of b
  
  #computing correlation coefficient(corr in abbreviation)
  corr_numerator <- sum((a-mean_value_a)*(b-mean_value_b))
  corr_denominator <- sqrt(sum((a-mean_value_a)^2)*sum((b-mean_value_b)^2))
  corr_value <- corr_numerator / corr_denominator
  
  #organizing the values in order to show the result
  sample_size <- c(sample_size_list$a, sample_size_list$b)
  names(sample_size) <- c("vector1", "vector2")
  mean_value <- c(mean_value_a, mean_value_b)
  names(mean_value) <- c("vector1", "vector2")
  
  result <- list(sample_size=sample_size, mean_value=mean_value, correlation_coefficient_value=corr_value) #multiple outputs
  return(result)
  
}

# vectors without NA
a <- c(1,2,3,4)
b <- c(6,8,7,4)
Q2(a,b)

# vectors including NA
a <- c(1,2,3,5,4,NA)
b <- c(5,6,NA,2,4,NA)
Q2(a,b,na.rm=T)
