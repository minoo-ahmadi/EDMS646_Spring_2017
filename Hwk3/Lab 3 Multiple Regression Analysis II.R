#Lab 3 Mutiple Regression Analysis with Qualitative Predictors 

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



# get the working directory 
# set the working directory 
getwd()
setwd("C:\\EDMS651\\Data")

#Install a package and load 
install.packages("psych", dependencies=TRUE)
library(lattice)

#Read data as a.csv file 

census <-read.csv("census.csv", header=TRUE, na.strings="-99", sep=",") 


#Viewing the data and getting summary statistics

str(census)
head(census)
tail(census)
summary(census)


####Multilpe Regression#### 

###Dummay coding### 
#Recall that our qualitative variable region is actually kept as a numeric variable in our data set with the following value labels:
#1 = West
#2 = Midwest
#3 = Northeast
#4 = South 
#5 = Other

## funtion factor and C creats dummy coded variables with default "treatement" 
## By default the first group is the reference group 

## Make factor variables first 
census$region.f = factor(census$region, labels=c("West", "Midwest", "Northeast", "South", "Other"))

## Using the factor variable, four dummay variables are created by the following command 
census$region.ct <- C(census$region.f, treatment)
attributes(census$region.ct)

## Running ANOVA using regression analysis
census.lm <- lm(commute~region.ct, data=census)
summary(census.lm)

## (Unadjusted) Group means 
tapply(census$commute, census$region.f, mean)

## Having Northeast as the reference group 
census$region.ct <- C(census$region.f, contr.treatment, base=3)
census$region.ct

## Running ANOVA using regression analysis 
census.lm <- lm(commute~region.ct, data=census)
summary(census.lm)

####More recoding options are avaiable and the link will be helpful 
####(http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm)

## visualization ## 
install.packages("car", dependencies=TRUE)
library(car)
Boxplot(commute~region, data=census, ylab="Commuting Time")
plot(commute~region, data=census, ylab="Commuting Time",xlab="Regions")


# Back to the West reference group coded variables
census$region.ct <- C(census$region.f, treatment)
attributes(census$region.ct)

##ANCOVA using MRA ### 
census.lm <- lm(commute~region.ct + income2, data=census)
summary(census.lm)

#Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        16.28762    0.83326  19.547  < 2e-16 ***
#region.ctMidwest   -0.76891    0.41226  -1.865 0.062739 .  
#region.ctNortheast  1.48020    0.48150   3.074 0.002224 ** 
#region.ctSouth      1.77654    0.39357   4.514 7.91e-06 ***
#region.ctOther      3.57062    0.91482   3.903 0.000108 ***
#census$income2      0.10955    0.01569   6.981 9.13e-12 ***

##Getting adjusted means## 
##Recall lecture slides ### 
#Adjusted Y = Original Y - common slope *(group mean of X - Grand mean of X) 

# original Ys and group mean of X
tapply(census$commute, census$region.f, mean)
tapply(census$income2, census$region.f, mean)
# common slope = 0.10955
# grand mean of X 
mean(census$income2)

adjWest <- 21.69100 - 0.10955* (49.32238  - mean(census$income2))
adjMidwest <- 20.53609 - 0.10955* (45.79897   - mean(census$income2))
adjNortheast <-23.32286  - 0.10955* (50.70673  - mean(census$income2))
adjSouth <- 22.70300  - 0.10955* (42.34363    - mean(census$income2))
adjOther <-  23.53571  - 0.10955* (33.56821   - mean(census$income2))

computed.adjusted.means <- cbind(adjWest, adjMidwest, adjNortheast, adjSouth, adjOther)
adjOther - adjWest 

##Getting adjusted means using effects package## 
install.packages("effects", dependencies=TRUE)
library(effects)
effect("region.ct", census.lm)
computed.adjusted.means

# These should be the same (with some rounding errors) 

## plotting##
range(census$income2)
range(census$commut)

plot(census$income2[census$region == 1], census$commute[census$region  == 1], xlab='Income',
ylab='Commute', xlim=c(10,100), ylim=c(10,40), pch=15, col='green')
points(census$income2[census$region == 2], census$commute[census$region  ==2], pch=15, col='red')
points(census$income2[census$region == 3], census$commute[census$region  ==3], pch=15, col='blue')
points(census$income2[census$region == 4], census$commute[census$region  ==4], pch=15, col='purple')
points(census$income2[census$region == 5], census$commute[census$region  ==5], pch=15, col='pink')

###Testing homogeneous slopes = Testing interactions### 
census.lm <- lm(commute~region.ct + census$income2 + region.ct*census$income2, data=census)
summary(census.lm)

#Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                       12.37472    1.57567   7.854 2.43e-14 ***
#region.ctMidwest                   6.83083    2.56819   2.660  0.00807 ** 
#region.ctNortheast                 0.04979    2.46528   0.020  0.98389    
#region.ctSouth                     4.63407    2.01036   2.305  0.02156 *  
#region.ctOther                    12.90934    2.08779   6.183 1.29e-09 ***
#census$income2                     0.18889    0.03137   6.021 3.33e-09 ***
#region.ctMidwest:census$income2   -0.15983    0.05397  -2.961  0.00321 ** 
#region.ctNortheast:census$income2  0.02604    0.04830   0.539  0.59000    
#region.ctSouth:census$income2     -0.05441    0.04276  -1.272  0.20385    
#region.ctOther:census$income2     -0.24097    0.04569  -5.274 1.98e-07 ***

# for West Yhat (expected mean of commuting time)     = 12.37472 +  0.18889*income2 
# for Midwest Yhat (expected mean of commuting time)  = 12.37472 + 6.83083+ (0.18889-0.15983)*income2 
# for Northeast Yhat (expected mean of commuting time)= 12.37472  +  0.04979 + (0.18889+ 0.02604)*income2 
# for Soutn Yhat (expected mean of commuting time)    = 12.37472 + 4.63407 + (0.18889 - 0.05441)*income2
# for Other Yhat (expected mean of commuting time)    = 12.37472 + 12.90934 + (  0.18889 -0.24097)*income2 

Y1 <- 12.37472 +  0.18889*census$income2 
Y2 <- 12.37472 + 6.83083+ (0.18889-0.15983)*census$income2 
Y3 <- 12.37472  +  0.04979 + (0.18889+ 0.02604)*census$income2 
Y4 <- 12.37472 + 4.63407 + (0.18889 - 0.05441)*census$income2
Y5 <- 12.37472 + 12.90934 + (0.18889 -0.24097)*census$income2 

plot(census$income2,Y1,type="n",bty="l",xlab="Income",ylab="Commuting Time")
lines(census$income2,Y1,lty=1,lwd=2,col='green')
lines(census$income2,Y2,lty=2,lwd=2,col='red')
lines(census$income2,Y3,lty=6,lwd=2,col='blue')
lines(census$income2,Y4,lty=9,lwd=2,col='purple')
lines(census$income2,Y5,lty=11,lwd=2,col='pink')

#Exercise 1: Interpret each regression coefficient regardless of is significance. 
#Constant: 
#Median household income: 
#Midwest: 
#Northeast:  
#South:  
#Other-region:  
#Midwest_int: 
#Northeast_int: 
#South_int:  
#Other-int:  

#Exercise 2: Write down a prediction equation for each region in terms of income predictor (You should end up with five prediction equations in this case)  
#hint) 
#Y1 <- 12.37472 +  0.18889*census$income2 
#Y2 <- 12.37472 + 6.83083+ (0.18889-0.15983)*census$income2 
#Y3 <- 12.37472  +  0.04979 + (0.18889+ 0.02604)*census$income2 
#Y4 <- 12.37472 + 4.63407 + (0.18889 - 0.05441)*census$income2
#Y5 <- 12.37472 + 12.90934 + (0.18889 -0.24097)*census$income2 



