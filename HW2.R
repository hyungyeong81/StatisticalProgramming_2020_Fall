#HW2_2019313106

#Codes for the first question
LCS <- LifeCycleSavings ;LCS

#1-(1)
LCSmean <- lapply(LCS,mean) ;LCSmean #mean for each variable
LCSsd <- lapply(LCS,sd) ;LCSsd #standard deviation for each variable

#1-(2)
#generating the vectors that represent standardized values for each variable
sr_sv <- (LCS$sr - LCSmean$sr) / LCSsd$sr #standardized values for the variable sr
pop15_sv <- (LCS$pop15 - LCSmean$pop15) / LCSsd$pop15 #standardized values for the variable pop15 
pop75_sv <- (LCS$pop75 - LCSmean$pop75) / LCSsd$pop75 #standardized values for the variable pop75
dpi_sv <- (LCS$dpi - LCSmean$dpi) / LCSsd$dpi #standardized values for the variable dpi
ddpi_sv <- (LCS$ddpi - LCSmean$ddpi) / LCSsd$ddpi #standardized values for the variable ddpi
#binding the vectors into a data frame object
LCS.new <- data.frame(sr_sv,pop15_sv,pop75_sv,dpi_sv,ddpi_sv) ;LCS.new

#1-(3)
rownames(LCS[LCS$dpi >= 1000 & LCS$dpi <= 1800, ]) #filtering the data frame

#1-(4)
#creating a new binary variable
ddpi.binary <- ifelse(LCS$ddpi >= 4, 1, 0)
#adding the binary variable to LCS.new
LCS.new <- cbind(LCS.new, ddpi.binary) ;LCS.new

#1-(5)
#creating a new factor variable
pop15breaks <- c(0,30,40,100) #since pop15 is the numeric	% of population under 15
size <- cut(LCS$pop15, pop15breaks, right=F) #generating a factor variable
levels(size) <- c("small", "medium", "large") #setting the name of the levels
#adding size(the factor variable) to LCS.new
LCS.new <- cbind(LCS.new, size) ;LCS.new

#1-(6)
table(ddpi.binary, size) #creating a frequency table

####################

#Codes for the second question
dat1 <- read.table('clinical_data1.csv', sep=',', header=T)
dat2 <- read.table('clinical_data2.csv', sep=',', header=T)

#2-(1)
str(dat1) #data structure of dat1
str(dat2) #data structure of dat2

#2-(2)
dat_merged <- merge(dat1, dat2, by="sampleID", all=T) ;dat_merged #merging dat1 and dat2

#2-(3)
dat_complete <- dat_merged[complete.cases(dat_merged),];dat_complete #discarding observations with at least one NA

#2-(4)
E <- ifelse((dat_complete$ER == 0) & (dat_complete$PGR == 0) & (dat_complete$HER2 == 1), 1, 0); E #creating the variable E

#2-(5)
tb <- table(E, dat_complete$OS.event) ;tb #creating a contingency table for E and os event

#2-(6)
tb_margin <- addmargins(tb) ;tb_margin
average_0 <- sum(dat_complete$OS.m) / tb_margin[1,3] #average of OS.m for E=0
average_1 <- sum(dat_complete$OS.m) / tb_margin[2,3] #average of OS.m for E=1
average_v <- c(average_0, average_1) # putting the average together (just for showing the result)
names(average_v) <- c("for E=0", "for E=1") ;average_v #assigning a name to each element

#2-(7)
dat_split <- split(dat_merged, dat_merged$metastasis) #splitting the data by metastasis
dat_split$"0" #creating the dataset of all observations with metastasis=0
