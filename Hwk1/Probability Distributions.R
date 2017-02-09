#population distribution (approximation)
population <- rnorm(100000)  ##rnorm makes random sampling from a standard normal 
mean(population)
sd(population)
hist(population)
?hist
hist(population,prob=TRUE,col=10)
x<-seq(0,100,0.001) 
curve(dnorm(x, mean=mean(population), sd=sd(population)), add=TRUE)
?curve
curve(dnorm(x, mean=mean(population), sd=sd(population)), add=TRUE, lwd=10)


#sample distribution 
sample <- rnorm(10)
mean(sample)
sd(sample)
hist(sample)
hist(sample,breaks = c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2),prob=TRUE)
curve(dnorm(x, mean=mean(sample), sd=sd(sample)), add=TRUE)


#sampling distribution of mean for n=10 
size <- 10   #sample size
rep <- 10    #number of replication 
record.sample <-NULL #storage# 
record.means <-  NULL 
record.sd <-NULL 

for (i in 1:rep){    
sample <- rnorm(size)
record.sample <- cbind(record.sample,sample)
record.means <- rbind(record.means,mean(sample))
record.sd <- rbind(record.sd,sd(sample))
}

#sampling distribution of mean
record.sample
record.means 
record.sd
hist(record.means, prob=TRUE)
curve(dnorm(x, mean=mean(record.means ), sd=sd(record.means)), add=TRUE)


#sampling distribution of mean for n=20 
size <- 20   #sample size
rep <- 500    #number of replication 
record.sample <-NULL #storage# 
record.means <-  NULL 
record.sd <-NULL 

for (i in 1:rep){    
sample <- rnorm(size)
record.sample <- cbind(record.sample,sample)
record.means <- rbind(record.means,mean(sample))
record.sd <- rbind(record.sd,sd(sample))
}

#sampling distribution of mean 
record.sample
record.means 
record.sd
hist(record.means, prob=TRUE)
curve(dnorm(x, mean=mean(record.means ), sd=sd(record.means)), add=TRUE)


#sampling distribution of mean for n=20 
size <- 20   #sample size
rep <- 100    #number of replication 
record.sample <-NULL #storage# 
record.means <-  NULL 
record.sd <-NULL 

for (i in 1:rep){    
sample <- rnorm(size)
record.sample <- cbind(record.sample,sample)
record.means <- rbind(record.means,mean(sample))
record.sd <- rbind(record.sd,sd(sample))
}

#sampling distribution of mean 
record.sample
record.means 
record.sd
hist(record.means, prob=TRUE)
curve(dnorm(x, mean=mean(record.means ), sd=sd(record.means)), add=TRUE)




#sampling distribution of mean for n=50 
size <- 50   #sample size
rep <- 1000    #number of replication 
record.sample <-NULL #storage# 
record.means <-  NULL 
record.sd <-NULL 

for (i in 1:rep){    
sample <- rnorm(size)
record.sample <- cbind(record.sample,sample)
record.means <- rbind(record.means,mean(sample))
record.sd <- rbind(record.sd,sd(sample))
}

#sampling distribution of mean 
record.sample
record.means 
record.sd
hist(record.means, prob=TRUE)
curve(dnorm(x, mean=mean(record.means ), sd=sd(record.means)), add=TRUE)


####How to copy and paste the graph from R to word#### 
### right click then choose "copy as metafile" and paste in word #### 


################################################

######Let's play with an example data set ###########

#################################################

# get the working directory 
# set the working directory 
getwd()
setwd("C:\\EDMS646\\Data")

#Install a package and load 
install.packages("psych", dependencies=TRUE)
library(lattice)

#Read data as a.csv file 

census <-read.csv("census.csv", header=TRUE, na.strings="-99", sep=",") 

#I. Data
#We will be using a common data set for all lab sessions throughout the quarter. The data set comes from the Census Bureau and uses data collected by the American Community Survey. The unit of analysis is metropolitan cities throughout the United States. There are 517 such observations in the data set. The variables in the data set include: 
#GEO_NAME	Metropolitan and Micropolitan Statistical Area
#areatype	Metropolitan and Micropolitan
#pop_size	Total Estimated Population
#division	US Division
#region	Region
#commute	Mean travel time to work
#hh_chil	Percent households with children <18
#edu_hs	Percentage adults with High School Diploma
#edu_coll	Percentage adults with Bachelor's Degree
#income	Median household income
#income2	Median household income (in thousands)
#no_car	Percentage of households without vehicle
#female	Percent population - female
#age	Median age
#senior	Percent population 65 years or older
#white	Percent Population - White
#black	Percent Population - Black
#indian	Percent Population - Indian
#asian	Percent Population - Asian
#hawaiian	Percent Population - Hawaiian
#other	Percent Population - Other
#hispanic	Percent Population - Hispanic

#The variables for division and region are defined by the map included on the last page. The variable areatype takes on two values ? micro and metro ? depending on the following definitions:
#	A metro area contains a core urban area of 50,000 or more population
#	A micro area contains an urban core of at least 10,000 (but less than 50,000) population
#During the semester we will be answering the research question:
#What best predicts average commute times of US cities?
#There is a mix of qualitative and quantitative predictor variables that we will use to answer the question, using regression based approaches.

#Viewing the data and getting summary statistics

str(census)
head(census)
tail(census)
summary(census)

########Univariate########

#to get univariate descriptive statistics (numeric description) 
library(psych)
# descriptive statistics
describe(census$commute)
# frequency table
table(census$commute)

#for more information 
?describe
?table

#Univariate plots
#Conventional histogram (graphic description) 

par(ask=TRUE)
with(census, {hist(commute, col="red", main="")
           hist(commute, breaks=15, col="blue", main="")
           hist(commute, breaks="FD", col="orange", main="")
})


#standardizing and plotting# 
install.packages("pls", dependencies=TRUE)
library(pls)
st.commute <-stdize(as.matrix(census$commute))
hist(st.commute)
summary(st.commute)


# for more information about histogram 
?hist
?par

#Create boxplot of "commute" variable 
with(census, boxplot(commute, col="gold",notch=TRUE, 
pars=list(boxwex=.5,staplewex=.25,outwex=3,outpch=19),ylab="commute"))


#####################################################################################################################
# Z distribution and its use: inferences for one sample mean with known sigma #
#####################################################################################################################

?rnorm 
#dnorm(x, mean = 0, sd = 1, log = FALSE)
#pnorm(q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#qnorm(p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
#rnorm(n, mean = 0, sd = 1)

#x, q	: vector of quantiles.
#p	: vector of probabilities.
#n	: number of observations. If length(n) > 1, the length is taken to be the number required.
#mean	: vector of means.
#sd	: vector of standard deviations.
#log, log.p	: logical; if TRUE, probabilities p are given as log(p).
#lower.tail	: logical; if TRUE (default), probabilities are P[X ¡Â x] otherwise, P[X > x].

dnorm(0,0,1)   #density = height# 
pnorm(0,0,1)   #input: z value, output: probability (area)
qnorm(0.5,0,1) #input: probabiliy, output: z value 
rnorm(10,0,1)  #random sampling 

pnorm(1.96,0,1) #What does this mean?
qnorm(0.95,0,1) #What does this mean?

#If your observed z-value is 3.75, what is p-value?

##### Online sources that you can refer to for z-test### 
http://www.inside-r.org/packages/cran/BSDA/docs/z.test
http://www.endmemo.com/program/R/ztest.php


###if we know the population variance 10## 
x.bar <- mean(census$commute) 
sigma <- 10 
install.packages("BSDA", dependencies=TRUE)
library(BSDA)
z.test(census$commute, mu = 0, sigma.x = sigma, conf.level = 0.95)  #Null hypothesis? 
z.test(census$commute, mu = 20, sigma.x = sigma, conf.level = 0.95) #Null hypothesis? 
z.test(census$commute, mu = 22, sigma.x = sigma, conf.level = 0.95) #Null hypothesis? 

#####################################################################################################################
# t distribution and its uses : inferences for means: one-sample t-test, independent t-test and paird sample t-test and more# 
#####################################################################################################################
install.packages("stats", dependencies=TRUE)
library(stats) 

#####one-sample ttest######
t.test(census$commute, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = 
FALSE, var.equal = FALSE, conf.level = 0.95) 

t.test(census$commute, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 20, paired = 
FALSE, var.equal = FALSE, conf.level = 0.95) 

t.test(census$commute, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 22, paired = 
FALSE, var.equal = FALSE, conf.level = 0.95) 

#####independent sample t test###### 
census$areatype

#### assumption check first#### equal variance or not equal variance 

install.packages("lawstat", dependencies=TRUE)
library(lawstat)
 
levene.test(census$commute, census$areatype, location=c("median", "mean", "trim.mean"), trim.alpha=0.25,
bootstrap = FALSE, num.bootstrap=1000, kruskal.test=FALSE, 
correction.method=c("none","correction.factor","zero.removal","zero.correction"))

###what is your conclusion about equal variance assumption? 

t.test(census$commute~census$areatype, alternative = c("two.sided", "less", "greater"), mu = 0, paired = 
FALSE, var.equal = TRUE, conf.level = 0.95)


###if equal variance is not assumed ####
t.test(census$commute~census$areatype, alternative = c("two.sided", "less", "greater"), mu = 0, paired = 
FALSE, var.equal = FALSE, conf.level = 0.95)

by(census$commute,census$areatyp,mean)     
by(census$commute,census$areatyp,sd)  

plot(census$commute~census$areatype) 
boxplot(census$commute~census$areatype) 

commute.macro <- subset(census, census$areatyp == 1)
commute.micro <- subset(census, census$areatyp == 2)

hist(commute.macro$commute)
hist(commute.micro$commute)

commute.macro.no.outlier <- subset(census, census$areatyp == 1 & census$commute < 35)
hist(commute.macro.no.outlier$commute)


####paird sample t-test example##### 
pre = c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
post = c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(pre,post,alternative="greater", paired=TRUE)   #one-sided 
t.test(pre,post,alternative="two.sided", paired=TRUE) #two-sided

t.test(pre,post,alternative="two.sided", paired=TRUE, conf.level=0.8) #two-sided

t.test(pre,post,alternative="two.sided", paired=TRUE, conf.level=0.8 alpha=0.05)

pairwise.t.test(pre,post,alpha=0.05)
boxplot(pre,post) 

##############################################################################################################################
#  Chi-square distribution and its uses: inferences for one sample variance and association between two categorical variables #
############################################################################################################################## 

######## Here, Chi-square test is to test one-sample standard deviation ####### 

## Create function to perform chi-square test.
var.interval = function(data,sigma0,conf.level = 0.95) {
  df = length(data) - 1
  chilower = qchisq((1 - conf.level)/2, df)
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  v = var(data)
  testchi = df*v/(sigma0^2)
  alpha = 1-conf.level

  print(paste("Standard deviation = ", round(sqrt(v),4)),quote=FALSE)
  print(paste("Test statistic = ", round(testchi,4)),quote=FALSE)
  print(paste("Degrees of freedom = ", round(df,0)),quote=FALSE)
  print(" ",quote=FALSE)
  print("Two-tailed test critical values, alpha=0.05",quote=FALSE)
  print(paste("Lower = ", round(qchisq(alpha/2,df),4)),quote=FALSE)
  print(paste("Upper = ", round(qchisq(1-alpha/2,df),4)),quote=FALSE)
  print(" ",quote=FALSE)
  print("95% Confidence Interval for Standard Deviation",quote=FALSE)
  print(c(round(sqrt(df * v/chiupper),4), 
         round(sqrt(df * v/chilower),4)),quote=FALSE)
}

## Perform chi-square test. The null hypothesis is that sigma(sd in population) = 4 or not 
sd(pre)
var.interval(pre,4)  # what is your conclusion? # 
var.interval(pre,10)  # what is your conclusion? # 


#####################################################################################################################
#  F distribution and its uses: inferences for two sample variances & means for multiple groups (ANOVA)             # 
#####################################################################################################################

library(stats)
var.test(pre, post, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)


var.test(census$commute~census$areatype, ratio = 1,
         alternative = c("two.sided", "less", "greater"),
         conf.level = 0.95)


######### more about distributions ###### 
http://socr.ucla.edu/htmls/SOCR_Distributions.html



