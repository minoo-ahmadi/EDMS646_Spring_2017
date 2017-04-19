# Lab 5 ONE-WAY ANOVA 

# get the working directory 
# set the working directory 
getwd()
setwd("C:\\EDMS646\\Data")

#Install a package and load 
install.packages("psych", dependencies=TRUE)
library(lattice)

#Read data as a.csv file 

census <-read.csv("Lab 5 -Data census_partial.csv", header=TRUE, na.strings="-99", sep=",") 
str(census)


##note that region is not a factor in the data frame. So we need to change

census$region <-as.factor(census$region)
levels(census$region) <- c("R1", "R2", "R3", "R4", "R5")

mean(census$commute[census$region=="1"])
mean(census$commute[census$region=="2"])
mean(census$commute[census$region=="3"])
mean(census$commute[census$region=="4"])
mean(census$commute[census$region=="5"])

# or 

tapply(census$commute, census$region, mean)
tapply(census$commute, census$region, var)

tapply(census$commute, census$region, length) #smaple size

## bosplot 

boxplot (census$commute ~ census$region)


## Oneway ANOVA - too simple 
oneway.test(census$commute ~ census$region)
oneway.test(census$commute ~ census$region, var.equal=TRUE) ## to use this, test homogeneiry of variances first!

## USE AOV

aov.out <- aov(census$commute ~ census$region)
summary(aov.out)

## Post-Hoc
TukeyHSD(aov.out)

## Contrast  ### Multiple Regression!!! 
summary.lm(aov.out)


### Test assumptions 

## Homogeneiry of variance. (http://www.itl.nist.gov/div898/handbook/eda/section3/eda357.htm) 

bartlett.test(census$commute ~ census$region)

library(lawstat)
levene.test(census$commute, census$region, location=c("median", "mean", "trim.mean"), trim.alpha=0.25,
bootstrap = FALSE, num.bootstrap=1000, kruskal.test=FALSE, 
correction.method=c("none","correction.factor","zero.removal","zero.correction"))


## Model checking  ## four plots are generated to see, 
## 1) equal variance
## 2) Normality 
## 3) This is like the first plot but now to specifically test if the residuals increase with the fitted values, which they do.
## 4) Outliers 

plot(aov.out)

###Non-parametric ANOVA
kruskal.test(census$commute ~ census$region)

As for the Wilcoxon test (or Mann-Whitney test) with two samples, this test converts the
response values to ranks, and tests whether the ranks are distributed equally across the
conditions, as would be expected under the null hypothesis.
