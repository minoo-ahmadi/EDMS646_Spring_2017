***Method1***

ONEWAY commute BY region
  /STATISTICS DESCRIPTIVES HOMOGENEITY BROWNFORSYTHE WELCH
  /PLOT MEANS
  /MISSING ANALYSIS.

****Method2****

UNIANOVA commute BY region 
  /METHOD=SSTYPE(3) 
  /INTERCEPT=INCLUDE 
  /SAVE=RESID ZRESID 
  /PLOT=PROFILE(region) 
  /EMMEANS=TABLES(OVERALL) 
  /EMMEANS=TABLES(region) 
  /PRINT=OPOWER ETASQ HOMOGENEITY DESCRIPTIVE  
  /PLOT=RESIDUALS 
  /CRITERIA=ALPHA(.05) 
  /DESIGN=region.

****Check Normality Assumption****

EXAMINE VARIABLES=ZRE_1
  /PLOT BOXPLOT STEMLEAF HISTOGRAM NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

***Check Independence Assumption****

GRAPH
  /SCATTERPLOT(BIVAR)=region WITH ZRE_1
  /MISSING=LISTWISE.

*** Nonparametric****


*** 1.  K-W  (When normality and homogenerity of variance are violated)  

NPAR TESTS
  /K-W=commute BY region(1 5)
  /MISSING ANALYSIS.


**** 2. Welch and Brwon-Forsyteh (When homogenerity of variance is violated with different ns) 

ONEWAY commute BY region
  /STATISTICS DESCRIPTIVES HOMOGENEITY BROWNFORSYTHE WELCH
  /PLOT MEANS
  /MISSING ANALYSIS.



