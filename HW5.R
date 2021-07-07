# HW5_2019313106 Hyungyeong Hong

# setting 

set.seed(2019313106)

x_length <- rbinom(1, size = 100, prob = 0.35) # length of the vector x
x <- rnorm(x_length, mean = 6, sd = 1) # generating the vector x

y <- rnorm(x_length, mean = 1, sd = 4) # generating the vector y



# defining functions

###1st### ###main###
hypothesisfunc <- function(vec1, vec2 = NULL){ # setting default state: vec2 <- NULL
  
  inputvecs_num <- c(vec1, vec2) # just used for counting the number of arguments
  
  if (length(inputvecs_num) == length(vec1)){ # one input vector
    onesamplefunc(vec1) # function call
  } else { # two input vectors
    twosamplesfunc(vec1,vec2) # function call
  }
}


###sub-functions for one sample testing####

###2nd### 
onesamplefunc <- function(vec1){
  
  if (length(vec1) >= 30){ # length of vec1 >= 30: compute z test
    
    pop_var <- var(vec1)
    onesample_z.func(vec1, pop_var) # function call
    
  } else {
    
    # length of vec1 < 30: z test or t test
    # get the population variance from users
    # input value(-> pop_var) == NA: t test
    cat("Enter the population variance.","The population variance should be a numeric value greater than 0.","Enter NA for t-test.", sep = "\n")
    pop_var <- readline(":")
    
    if (pop_var == "NA"){ #input value(-> pop_var == "NA") , the value is character due to readline()
      
      onesample_t.func(vec1) # function call 
      
    } else if ((is.na(as.numeric(pop_var)) == FALSE ) & (as.numeric(pop_var) > 0)) {
      
      pop_var <- as.numeric(pop_var) # the prior value is character due to readline()
      onesample_z.func(vec1, pop_var) # function call 
      
    } else { # wrong input value
      
      cat("Error: wrong input value\n")
    }
  }
}



###3rd### one sample, z-test
onesample_z.func <- function(vec1, pop_var){
  
  # getting a null hypothesis value from users
  cat("Enter null hypothesis value for the population mean.","The null hypothesis value for the population mean should be a numeric value.", sep = "\n")
  null_val <- as.numeric(readline(":"))
  
  if (is.na(null_val) == TRUE){ # non-numeric values are automatically changed into NA's
    return(cat("Error: wrong input value\n"))
  }
  
  # computing test statistic
  # sample_mean <- mean(vec1)
  # test_statistic <- (sample_mean - null_val) / sqrt(pop_var/length(vec1))
  
  # getting an alternative hypothesis format from users
  cat("Enter the following values indicating the formats of alternative hypothesis.","1: two-tailed","2: left-tailed","3: right-tailed", sep = "\n")
  alternative_indic <- readline(":")
  
  # Determining whether the test statistic is in rejection region or not
  # Differs on the format of alternative hypothesis
  if (alternative_indic == "1"){ # two-tailed 
    onesample_twotailed_z.func(vec1, pop_var, null_val, alternative_indic)
  } else if (alternative_indic == "2") { # left-tailed
    onesample_lefttailed_z.func(vec1, pop_var, null_val, alternative_indic)
  } else if (alternative_indic == "3") { # right-tailed
    onesample_righttailed_z.func(vec1, pop_var, null_val, alternative_indic)
  } else { # wrong input value
    cat("Error: wrong input value\n")
  }
}



###3rd### one sample, t-test
onesample_t.func <- function(vec1) {
  
  # getting a null hypothesis value from users
  cat("Enter null hypothesis value for the population mean.","The null hypothesis value for the population mean should be a numeric value.", sep = "\n")
  null_val <- as.numeric(readline(":"))
  
  if (is.na(null_val) == TRUE){ # non-numeric values are automatically changed into NA's
    return(cat("Error: wrong input value\n"))
  }
  
  # getting an alternative hypothesis format from users
  cat("Enter the following values indicating the formats of alternative hypothesis.","1: two-tailed","2: left-tailed","3: right-tailed", sep = "\n")
  alternative_indic <- readline(":")
  
  # Determining whether the test statistic is in rejection region or not
  # Differs on the format of alternative hypothesis
  if (alternative_indic == "1"){ # two-tailed 
    onesample_twotailed_t.func(vec1, null_val, alternative_indic) # function call
  } else if (alternative_indic == "2") { # left-tailed
    onesample_lefttailed_t.func(vec1, null_val, alternative_indic) # function call
  } else if (alternative_indic == "3") { # right-tailed
    onesample_righttailed_t.func(vec1, null_val, alternative_indic) # function call
  } else { # wrong input value
    cat("Error: wrong input value\n")
  }
  
}



###4th### one sample, z-test, two-tailed
onesample_twotailed_z.func <- function(vec1, pop_var, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean <- mean(vec1)
  test_statistic <- (sample_mean - null_val) / sqrt(pop_var / length(vec1))
  
  p_value <- 2*(1-pnorm(abs(test_statistic)))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- sample_mean - qnorm(1-0.025)*sqrt(pop_var/length(vec1))
  upperbound <- sample_mean + qnorm(1-0.025)*sqrt(pop_var/length(vec1))
  
  # function call (for outputs)
  onesample_outputs_z.func(test_statistic, vec1, pop_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound)

}



###4th### one sample, z-test, left-tailed

onesample_lefttailed_z.func <- function(vec1, pop_var, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean <- mean(vec1)
  test_statistic <- (sample_mean - null_val) / sqrt(pop_var / length(vec1))
  
  p_value <- pnorm(test_statistic)

  if(p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  
  # confidence interval
  lowerbound <- (-Inf)
  upperbound <- sample_mean + qnorm(1-0.05)*sqrt(pop_var/length(vec1))
  
  # function call (for outputs)
  onesample_outputs_z.func(test_statistic, vec1, pop_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound)

}



###4th### one sample, z-test, right-tailed
onesample_righttailed_z.func <- function(vec1, pop_var, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean <- mean(vec1)
  test_statistic <- (sample_mean - null_val) / sqrt(pop_var / length(vec1))
  
  p_value <- (1 - pnorm(test_statistic))

  if(p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- sample_mean - qnorm(1-0.05)*sqrt(pop_var/length(vec1))
  upperbound <- Inf
  
  # function call (for outputs)
  onesample_outputs_z.func(test_statistic, vec1, pop_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound)

}



###5th### ###end of the process### one sample, z-test, outputs
onesample_outputs_z.func <- function(test_statistic, vec1, pop_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound){
  
  # common format of outputs (should be concatenated at the end)
  title <- "<HYPOTHESIS TESTING RESULT>"
  #l1
  # vec1
  # l2
  l3 <- sprintf("The sample size is %d, and the sample mean is %.3f.", length(vec1), sample_mean)
  l4 <- sprintf("The assumed value of population variance is %.3f.", pop_var)
  l5 <- sprintf("The test statistic is %.3f.", test_statistic)
  l6 <- sprintf("The p-value in this case is %.10f.", p_value)
  l7 <- sprintf("The 95%% confidence interval in this case is [ %.3f, %.3f ].", lowerbound, upperbound)
  l8 <- sprintf("If the p-value is less than or equal to 0.05, the null hypothesis is rejected.")
  l9 <- sprintf("In this case, the result based on the p-value is: %s.", pval.rejection_result);
  
  if (alternative_indic == "1") { # two-tailed
    
    l1 <- sprintf("Two-tailed z-test was conducted at 0.05 significance level, by using the numeric vector:")
    l2 <- sprintf("The null hypothesis is H0: mu = %.3f and the alternative hypothesis is H1: mu != %.3f.", null_val, null_val)

  } else if (alternative_indic == "2") { # left-tailed
    
    l1 <- sprintf("Left-tailed z-test was conducted at 0.05 significance level, by using the numeric vector:")
    l2 <- sprintf("The null hypothesis is H0: mu = %.3f and the alternative hypothesis is H1: mu < %.3f.", null_val, null_val)

  } else { # alternative_indic == "3" # right-tailed
    
    l1 <- sprintf("Right-tailed z-test was conducted at 0.05 significance level, by using the numeric vector:")
    l2 <- sprintf("The null hypothesis is H0: mu = %.3f and the alternative hypothesis is H1: mu > %.3f.", null_val, null_val)

  }
  
  
  # concatenate
  cat(title, l1, sep = "\n")
  print(vec1)
  cat(l2, l3, l4, l5, l6, l7, l8, l9, sep = "\n")
  
  # end of the process
  
}



###4th### one sample, t-test, two-tailed
onesample_twotailed_t.func <- function(vec1, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean <- mean(vec1)
  sample_var <- var(vec1) # no information about the population variance: t-test
  test_statistic <- (sample_mean - null_val) / sqrt(sample_var / length(vec1))
  
  p_value <- 2*(1-pt(abs(test_statistic),length(vec1)-1))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- sample_mean - qt(1-0.025,length(vec1)-1)*sqrt(sample_var/length(vec1))
  upperbound <- sample_mean + qt(1-0.025,length(vec1)-1)*sqrt(sample_var/length(vec1))
  
  # function call (for outputs)
  onesample_outputs_t.func(test_statistic, vec1, sample_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound)
  
}



###4th### one sample, t-test, left-tailed
onesample_lefttailed_t.func <- function(vec1, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean <- mean(vec1)
  sample_var <- var(vec1) # no information about the population variance: t-test
  test_statistic <- (sample_mean - null_val) / sqrt(sample_var / length(vec1))
  
  p_value <- pt(test_statistic,length(vec1)-1)
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (-Inf)
  upperbound <- sample_mean + qt(1-0.05,length(vec1)-1)*sqrt(sample_var/length(vec1))

  # function call (for outputs)
  onesample_outputs_t.func(test_statistic, vec1, sample_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound)
  
}



###4th### one sample, t-test, right-tailed

onesample_righttailed_t.func <- function(vec1, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean <- mean(vec1)
  sample_var <- var(vec1) # no information about the population variance: t-test
  test_statistic <- (sample_mean - null_val) / sqrt(sample_var / length(vec1))
  
  p_value <- (1-(pt(test_statistic,length(vec1)-1)))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- sample_mean - qt(1-0.05,length(vec1)-1)*sqrt(sample_var/length(vec1))
  upperbound <- Inf
  
  # function call (for outputs)
  onesample_outputs_t.func(test_statistic, vec1, sample_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound)
  
}



###5th### ###end of the process### one sample, t-test, outputs
onesample_outputs_t.func <- function(test_statistic, vec1, sample_var, sample_mean, null_val, alternative_indic, p_value, pval.rejection_result, lowerbound, upperbound){
  
  df <- length(vec1)-1 # used for formatting
  
  # common format of outputs
  title <- "<HYPOTHESIS TESTING RESULT>"
  # l1
  # vec1
  # l2 
  l3 <- sprintf("The sample size is %d,the sample mean is %.3f and the sample variance is %.3f.", length(vec1), sample_mean, sample_var)
  l4 <- sprintf("The test statistic in this case is %.3f.", test_statistic)
  l5 <- sprintf("The degrees of freedom in this case is %d.", df)
  l6 <- sprintf("The p-value in this case is %.10f.", p_value)
  l7 <- sprintf("If the p-value is less than or equal to 0.05, the null hypothesis is rejected.")
  l8 <- sprintf("In this case, the result based on the p-value is: %s.", pval.rejection_result)
  l9 <- sprintf("The 95%% confidence interval in this case is [ %.3f, %.3f ].", lowerbound, upperbound)
  
  if (alternative_indic == "1") { # two-tailed
    
    l1 <- sprintf("two-tailed t-test was conducted at 0.05 significance level, by using the numeric vector:")
    l2 <- sprintf("The null hypothesis is H0: mu = %.3f and the alternative hypothesis is H1: mu != %.3f.", null_val, null_val)

  } else if (alternative_indic == "2") { # left-tailed
    
    l1 <- sprintf("left-tailed t-test was conducted at 0.05 significance level, by using the numeric vector:")
    l2 <- sprintf("The null hypothesis is H0: mu = %.3f and the alternative hypothesis is H1: mu < %.3f.", null_val, null_val)

  } else { # alternative_indic == "3" # right-tailed
    
    l1 <- sprintf("right-tailed t-test was conducted at 0.05 significance level, by using the numeric vector:")
    l2 <- sprintf("The null hypothesis is H0: mu = %.3f and the alternative hypothesis is H1: mu > %.3f.", null_val, null_val)

  }
  
  # concatenate
  cat(title, l1, sep = "\n")
  print(vec1)
  cat(l2, l3, l4, l5, l6, l7, l8, l9, sep = "\n")
  
  # end of the process

}



###sub-functions for two sample testing###
###2nd### 
twosamplesfunc <- function(vec1, vec2){
  
  if (length(vec1) == length(vec2)){ # vectors with the same length
    
    if (length(vec1) >= 30){ # common length >= 30: compute z test
      
      pop_var1 <- var(vec1)
      pop_var2 <- var(vec2)
      twosamples_z.func(vec1, vec2, pop_var1, pop_var2) # function call
      
    } else {
      
      # length_common < 30: z test or t test
      # get the population variance from users
      # input value(-> pop_var1 or pop_var2) == NA: t test
      cat("Enter the population variance of the first vector.","The population variance should be a numeric value greater than 0.","Enter NA for t-test.", sep = "\n")
      pop_var1 <- readline(":")
      cat("Enter the population variance of the second vector.","The population variance should be a numeric value greater than 0.","Enter NA for t-test.", sep = "\n")
      pop_var2 <- readline(":")
      
      if (pop_var1 == "NA" & pop_var2 == "NA"){
        
        twosamples_t.func(vec1, vec2) # function call
        
      } else if (is.na(as.numeric(pop_var1)) == FALSE & is.na(as.numeric(pop_var2)) == FALSE & as.numeric(pop_var1) > 0 & as.numeric(pop_var2) > 0){
        
        pop_var1 <- as.numeric(pop_var1)
        pop_var2 <- as.numeric(pop_var2)
        twosamples_z.func(vec1, vec2, pop_var1, pop_var2) # function call
        
      } else {# wrong input value
        
        cat("Error: wrong input value\n")
        
      }
    }
    
  } else { # vectors with different lengths
    
    cat("Error: The length of two input vectors should be the same.","Check the length of each vectors", sep="\n")
    
  }
  
}



###3rd### two samples, z-test
twosamples_z.func <- function(vec1, vec2, pop_var1, pop_var2){
  
  # getting a null hypothesis value from users
  cat("Enter null hypothesis value for the difference of population means.","The null hypothesis value for the difference of population means should be a numeric value.", sep = "\n")
  null_val <- as.numeric(readline(":"))
  
  if (is.na(null_val) == TRUE){ # non-numeric values are automatically changed into NA's
    return(cat("Error: wrong input value\n")) # termination
  }
  
  # # computing test statistic
  # sample_mean1 <- mean(vec1)
  # sample_mean2 <- mean(vec2)
  # test_statistic <- ((sample_mean1 - sample_mean2) - null_val) / sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  
  # getting an alternative hypothesis format from users
  cat("Enter the following values indicating the formats of alternative hypothesis.","1: two-tailed","2: left-tailed","3: right-tailed", sep = "\n")
  alternative_indic <- readline(":")
  
  # Determining whether the test statistic is in rejection region or not
  # Differs on the format of alternative hypothesis
  if (alternative_indic == "1"){ # two-tailed 
    twosamples_twotailed_z.func(vec1, vec2, pop_var1, pop_var2, null_val, alternative_indic) # function call
  } else if (alternative_indic == "2") { # left-tailed
    twosamples_lefttailed_z.func(vec1, vec2, pop_var1, pop_var2, null_val, alternative_indic) # function call
  } else if (alternative_indic == "3") { # right-tailed
    twosamples_righttailed_z.func(vec1, vec2, pop_var1, pop_var2, null_val, alternative_indic) # function call
  } else { # wrong input value
    cat("Error: wrong input value\n")
  }
} 



###3rd### two samples, t-test
twosamples_t.func <- function(vec1, vec2){
  
  # getting a null hypothesis value from users
  cat("Enter null hypothesis value for the difference of population means.","The null hypothesis value for the difference of population means should be a numeric value.", sep = "\n")
  null_val <- as.numeric(readline(":"))
  
  if (is.na(null_val) == TRUE){ # non-numeric values are automatically changed into NA's
    return(cat("Error: wrong input value\n")) # termination
  }
  
  # getting an alternative hypothesis format from users
  cat("Enter the following values indicating the formats of alternative hypothesis.","1: two-tailed","2: left-tailed","3: right-tailed", sep = "\n")
  alternative_indic <- readline(":")
  
  # Determining whether the test statistic is in rejection region or not
  # Differs on the format of alternative hypothesis
  if (alternative_indic == "1"){ # two-tailed 
    twosamples_twotailed_t.func(vec1, vec2, null_val, alternative_indic) # function call 
  } else if (alternative_indic == "2") { # left-tailed
    twosamples_lefttailed_t.func(vec1, vec2, null_val, alternative_indic) # function call
  } else if (alternative_indic == "3") { # right-tailed
    twosamples_righttailed_t.func(vec1, vec2, null_val, alternative_indic) # function call
  } else { # wrong input value
    cat("Error: wrong input value\n")
  }
}



###4th### two samples, z-test, two-tailed
twosamples_twotailed_z.func <- function(vec1, vec2, pop_var1, pop_var2, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean1 <- mean(vec1)
  sample_mean2 <- mean(vec2)
  test_statistic <- ((sample_mean1 - sample_mean2) - null_val) / sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  
  p_value <- 2*(1-pnorm(abs(test_statistic)))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (sample_mean1 - sample_mean2) - qnorm(1-0.025)*sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  upperbound <- (sample_mean1 - sample_mean2) + qnorm(1-0.025)*sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))

  twosamples_outputs_z.func(vec1, vec2, pop_var1, pop_var2, null_val, sample_mean1, sample_mean2, test_statistic, p_value, pval.rejection_result, lowerbound, upperbound, alternative_indic)
  
}



###4th### two samples, z-test, left-tailed
twosamples_lefttailed_z.func <- function(vec1, vec2, pop_var1, pop_var2, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean1 <- mean(vec1)
  sample_mean2 <- mean(vec2)
  test_statistic <- ((sample_mean1 - sample_mean2) - null_val) / sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  
  p_value <- pnorm(test_statistic)
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (-Inf)
  upperbound <- (sample_mean1 - sample_mean2) + qnorm(1-0.05)*sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  
  twosamples_outputs_z.func(vec1, vec2, pop_var1, pop_var2, null_val, sample_mean1, sample_mean2, test_statistic, p_value, pval.rejection_result, lowerbound, upperbound, alternative_indic)
  
}


###4th### two samples, z-test, right-tailed
twosamples_righttailed_z.func <- function(vec1, vec2, pop_var1, pop_var2, null_val, alternative_indic){
  
  # computing test statistic
  sample_mean1 <- mean(vec1)
  sample_mean2 <- mean(vec2)
  test_statistic <- ((sample_mean1 - sample_mean2) - null_val) / sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  
  p_value <- (1 - pnorm(test_statistic))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (sample_mean1 - sample_mean2) - qnorm(1-0.05)*sqrt((pop_var1/length(vec1))+(pop_var2/length(vec2)))
  upperbound <- Inf
  
  twosamples_outputs_z.func(vec1, vec2, pop_var1, pop_var2, null_val, sample_mean1, sample_mean2, test_statistic, p_value, pval.rejection_result, lowerbound, upperbound, alternative_indic)
  
}


###5th### ###end of the process### two samples, z-test, outputs
twosamples_outputs_z.func <- function(vec1, vec2, pop_var1, pop_var2, null_val, sample_mean1, sample_mean2, test_statistic, p_value, pval.rejection_result, lowerbound, upperbound, alternative_indic){
  
  # common format of outputs (should be concatenated at the end)
  title <- "<HYPOTHESIS TESTING RESULT>"
  # l1
  l2 <- sprintf("The first numeric vector is:")
  # vec1
  l3 <- sprintf("The second numeric vector is:")
  # vec2
  # l4
  l5 <- sprintf("The sample size of each vector is %d.", length(vec1))
  l6 <- sprintf("The sample mean of the first vector is %.3f, and the sample mean of the second vector is %.3f.", sample_mean1, sample_mean2)
  l7 <- sprintf("The assumed value of the first population variance is %.3f, and the assumed value of the second population variance is %.3f.", pop_var1, pop_var2)
  l8 <- sprintf("The test statistic in this case is %.3f.", test_statistic)
  l9 <- sprintf("The p-value in this case is %.10f.", p_value)
  l10 <- sprintf("If the p-value is less than or equal to 0.05, the null hypothesis is rejected.")
  l11 <- sprintf("In this case, the result based on the p-value is: %s.", pval.rejection_result)
  l12 <- sprintf("The 95%% confidence interval in this case is [ %.3f, %.3f ].", lowerbound, upperbound)
  
  if (alternative_indic == "1") { # two-tailed
    
    l1 <- sprintf("Two-tailed z-test was conducted at 0.05 significance level, by using two numeric vectors.")
    l4 <- sprintf("The null hypothesis is H0: mu1-mu2 = %.3f and the alternative hypothesis is H1: mu1-mu2 != %.3f.", null_val, null_val)
    
  } else if (alternative_indic == "2") { # left-tailed
    
    l1 <- sprintf("Left-tailed z-test was conducted at 0.05 significance level, by using two numeric vectors.")
    l4 <- sprintf("The null hypothesis is H0: mu1-mu2 = %.3f and the alternative hypothesis is H1: mu1-mu2 < %.3f.", null_val, null_val)
    
  } else { # alternative_indic == "3" # right-tailed
    
    l1 <- sprintf("Right-tailed z-test was conducted at 0.05 significance level, by using two numeric vectors.")
    l4 <- sprintf("The null hypothesis is H0: mu1-mu2 = %.3f and the alternative hypothesis is H1: mu1-mu2 > %.3f.", null_val, null_val)

  }
  
  # concatenate
  cat(title, l1, l2, sep = "\n")
  print(vec1)
  cat(l3, sep = "\n")
  print(vec2)
  cat(l4, l5, l6, l7, l8, l9, l10, l11, l12, sep = "\n")
  
  # end of the process
  
}



###4th### two samples, t-test, two-tailed
twosamples_twotailed_t.func <- function(vec1, vec2, null_val, alternative_indic){
  
  cat("Enter the following values indicating the equality assumption of the two population variance.","T: Equal", "F: Unequal", sep = "\n")
  var.equal <- readline(":")
  
  if(var.equal == "F"){
    var.func_result <- unequal_var.func(vec1, vec2, null_val)
  } else if (var.equal == "T") {
    var.func_result <- equal_var.func(vec1, vec2, null_val)
  } else { # wrong input value
    return(cat("wrong input value\n")) #termination
  }
  
  #var.func_result <- c(sample_mean1, sample_mean2, sample_var1, sample_var2, test_statistic, df)
  sample_mean1 <- var.func_result[1]
  sample_mean2 <- var.func_result[2]
  sample_var1 <- var.func_result[3]
  sample_var2 <- var.func_result[4]
  test_statistic <- var.func_result[5]
  df <- var.func_result[6]
  
  p_value <- 2*(1-pt(abs(test_statistic),df))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (sample_mean1 - sample_mean2) - qt(1-0.025,df)*sqrt((sample_var1/length(vec1)) + (sample_var2/length(vec2)))
  upperbound <- (sample_mean1 - sample_mean2) + qt(1-0.025,df)*sqrt((sample_var1/length(vec1)) + (sample_var2/length(vec2)))

  twosamples_outputs_t.func(test_statistic, vec1, vec2, sample_mean1, sample_mean2, sample_var1, sample_var2, null_val, df, alternative_indic, p_value, upperbound, lowerbound, pval.rejection_result)
    
}


###4th### two samples, t-test, left-tailed
twosamples_lefttailed_t.func <- function(vec1, vec2, null_val, alternative_indic){
  
  cat("Enter the following values indicating the equality assumption of the two population variance.","T: Equal", "F: Unequal", sep = "\n")
  var.equal <- readline(":")
  
  if(var.equal == "F"){
    var.func_result <- unequal_var.func(vec1, vec2, null_val)
  } else if (var.equal == "T") {
    var.func_result <- equal_var.func(vec1, vec2, null_val)
  } else { # wrong input value
    return(cat("wrong input value\n")) #termination
  }
  
  #var.func_result <- c(sample_mean1, sample_mean2, sample_var1, sample_var2, test_statistic, df)
  sample_mean1 <- var.func_result[1]
  sample_mean2 <- var.func_result[2]
  sample_var1 <- var.func_result[3]
  sample_var2 <- var.func_result[4]
  test_statistic <- var.func_result[5]
  df <- var.func_result[6]
  
  p_value <- pt(test_statistic,df)
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (-Inf)
  upperbound <- (sample_mean1 - sample_mean2) + qt(1-0.05,df)*sqrt((sample_var1/length(vec1)) + (sample_var2/length(vec2)))

  twosamples_outputs_t.func(test_statistic, vec1, vec2, sample_mean1, sample_mean2, sample_var1, sample_var2, null_val, df, alternative_indic, p_value, upperbound, lowerbound, pval.rejection_result)
    
}


###4th### two samples, t-test, right-tailed
twosamples_righttailed_t.func <- function(vec1, vec2, null_val, alternative_indic){
  
  cat("Enter the following values indicating the equality assumption of the two population variance.","T: Equal", "F: Unequal", sep = "\n")
  var.equal <- readline(":")
  
  if(var.equal == "F"){
    var.func_result <- unequal_var.func(vec1, vec2, null_val)
  } else if (var.equal == "T") {
    var.func_result <- equal_var.func(vec1, vec2, null_val)
  } else { # wrong input value
    return(cat("wrong input value\n")) #termination
  }
  
  #var.func_result <- c(sample_mean1, sample_mean2, sample_var1, sample_var2, test_statistic, df)
  sample_mean1 <- var.func_result[1]
  sample_mean2 <- var.func_result[2]
  sample_var1 <- var.func_result[3]
  sample_var2 <- var.func_result[4]
  test_statistic <- var.func_result[5]
  df <- var.func_result[6]
  
  p_value <- (1 - pt(test_statistic,df))
  if (p_value <= 0.05){
    pval.rejection_result <- "reject null hypothesis" # result from the p-value
  } else {
    pval.rejection_result <- "cannot reject null hypothesis" # result from the p-value
  }
  
  # confidence interval
  lowerbound <- (sample_mean1 - sample_mean2) - qt(1-0.05,df)*sqrt((sample_var1/length(vec1)) + (sample_var2/length(vec2)))
  upperbound <- Inf
  
  twosamples_outputs_t.func(test_statistic, vec1, vec2, sample_mean1, sample_mean2, sample_var1, sample_var2, null_val, df, alternative_indic, p_value, upperbound, lowerbound, pval.rejection_result)

}

###5th### ###end of the process### two samples, t-test, outputs
twosamples_outputs_t.func <- function(test_statistic, vec1, vec2, sample_mean1, sample_mean2, sample_var1, sample_var2, null_val, df, alternative_indic, p_value, upperbound, lowerbound, pval.rejection_result){
  
  # common format of outputs (should be concatenated at the end)
  title <- "<HYPOTHESIS TESTING RESULT>"
  # l1
  l2 <- sprintf("The first numeric vector is:")
  # vec1
  l3 <- sprintf("The second numeric vector is:")
  # vec2
  # l4
  l5 <- sprintf("As for the first vector, the sample size is %d,the sample mean is %.3f and the sample variance is %.3f.", length(vec1), sample_mean1, sample_var1)
  l6 <- sprintf("As for the second vector, the sample size is %d,the sample mean is %.3f and the sample variance is %.3f.", length(vec2), sample_mean2, sample_var2)
  l7 <- sprintf("The test statistic in this case is %.3f.", test_statistic)
  l8 <- sprintf("The degrees of freedom in this case is %.3f.", df)
  l9 <- sprintf("The p-value in this case is %.10f.", p_value)
  l10 <- sprintf("If the p-value is less than or equal to 0.05, the null hypothesis is rejected.")
  l11 <- sprintf("In this case, the result based on the p-value is: %s at 0.05 significance level.", pval.rejection_result);
  l12 <- sprintf("The 95%% confidence interval in this case is [ %.3f, %.3f ].", lowerbound, upperbound)
  
  
  
  if (alternative_indic == "1"){ # two-tailed
    l1 <- sprintf("Two-tailed t-test was conducted at 0.05 significance level, by using two numeric vectors")
    l4 <- sprintf("The null hypothesis is H0: mu1-mu2 = %.3f and the alternative hypothesis is H1: mu1-mu2 != %.3f.", null_val, null_val)
    
  } else if (alternative_indic == "2") { # left-tailed
    
    l1 <- sprintf("Left-tailed t-test was conducted at 0.05 significance level, by using two numeric vectors")
    l4 <- sprintf("The null hypothesis is H0: mu1-mu2 = %.3f and the alternative hypothesis is H1: mu1-mu2 < %.3f.", null_val, null_val)
    
  } else { 
    
    l1 <- sprintf("Right-tailed t-test was conducted at 0.05 significance level, by using two numeric vectors")
    l4 <- sprintf("The null hypothesis is H0: mu1-mu2 = %.3f and the alternative hypothesis is H1: mu1-mu2 > %.3f.", null_val, null_val)
    
  }
  
  # concatenate
  cat(title, l1, l2, sep = "\n")
  print(vec1)
  cat(l3, sep = "\n")
  print(vec2)
  cat(l4, l5, l6, l7, l8, l9, l10, l11, l12, sep = "\n")
  
  # end of the process
  
}



###extra functions for t-test with two samples###

unequal_var.func <- function(vec1, vec2, null_val){
  
  sample_mean1 <- mean(vec1)
  sample_mean2 <- mean(vec2)
  sample_var1 <- var(vec1)
  sample_var2 <- var(vec2)
  
  test_stat_num <- (sample_mean1 - sample_mean2) - null_val
  test_stat_denom <- sqrt((sample_var1 / length(vec1)) + (sample_var2 / length(vec2)))
  test_statistic <- test_stat_num / test_stat_denom
  
  df_num <- ((sample_var1 / length(vec1)) + (sample_var2 / length(vec2)))^2
  df_denom1 <- ((sample_var1 / length(vec1))^2) / (length(vec1)-1)
  df_denom2 <- ((sample_var2 / length(vec2))^2) / (length(vec2)-1)
  df <- df_num / (df_denom1 + df_denom2)
  
  var.func_result <- c(sample_mean1, sample_mean2, sample_var1, sample_var2, test_statistic, df)
  return(var.func_result)
  
}


equal_var.func <- function(vec1, vec2, null_val){
  
  sample_mean1 <- mean(vec1)
  sample_mean2 <- mean(vec2)
  
  pooled_sd_num <- (length(vec1)-1)*var(vec1) + (length(vec2)-1)*var(vec2)
  pooled_sd_denom <- length(vec1) + length(vec2) - 2
  pooled_sd <- sqrt(pooled_sd_num / pooled_sd_denom)
  
  test_stat_num <- (sample_mean1 - sample_mean2) - null_val
  test_stat_denom <- pooled_sd*((1/length(vec1)) + (1/length(vec2)))
  test_statistic <- test_stat_num / test_stat_denom
  
  df <- length(vec1) + length(vec2) - 2
  
  sample_var1 <- (pooled_sd)^2
  sample_var2 <- (pooled_sd)^2
  
  var.func_result <- c(sample_mean1, sample_mean2, sample_var1, sample_var2, test_statistic, df)
  return(var.func_result)

}
  
  
  