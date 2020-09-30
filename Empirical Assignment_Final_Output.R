> library(haven)
> library(clubSandwich)
> mydata <- read_dta("/Users/kacperszostakow/Desktop/data.dta")
> attach(mydata)
> 
> #create variables
> mydata$dt <- dltt + dlc
> attr(mydata$dt, 'label') <- 'Debt - Total'
> mydata$leverage <- mydata$dt/at
> attr(mydata$leverage, 'label') <- 'Leverage - Variable'
> mydata$tangibility <- ppent/at
> attr(mydata$tangibility, 'label') <- 'Tangibility - Variable'
> mydata$mkvalt <- prcc_f * csho
> attr(mydata$mkvalt, 'label') <- 'Market Value'
> mydata$bvoe <- at - mydata$dt
> attr(mydata$bvoe, 'label') <- 'Book Value of Equity'
> mydata$market_to_book <- (at - ceq + mydata$mkvalt)/at
> attr(mydata$market_to_book, 'label') <- 'Market-to-Book'
> mydata$logsale <- log(sale) # In log(sale) : NaNs produced. Replace with 0
> mydata$logsale[!is.finite(mydata$logsale)] <- 0
> mydata$logsale[is.nan(mydata$logsale)] <- 0
> attr(mydata$logsale, 'label') <- 'Log of Sale'
> mydata$profitability <- ebitda/at
> attr(mydata$profitability, 'label') <- 'Profitability'
> detach(mydata)
> 
> #apply filters
> library(dplyr)
> mydataclean <- mydata[mydata$fic == "USA" & mydata$at > 0,]
> attach(mydataclean)
> #### a) 
> length(unique(gvkey)) # we have 1354 obs & 677 firms
[1] 1988
> summary(mydataclean) # find MIN, MAX and AVERAGE
    gvkey               fyear         indfmt             consol            datafmt             curcd                fyr               at           
 Length:7153        Min.   :2005   Length:7153        Length:7153        Length:7153        Length:7153        Min.   : 1.000   Min.   :     0.00  
 Class :character   1st Qu.:2008   Class :character   Class :character   Class :character   Class :character   1st Qu.: 6.000   1st Qu.:     7.62  
 Mode  :character   Median :2011   Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :11.000   Median :    37.55  
                    Mean   :2011                                                                               Mean   : 9.017   Mean   :   315.63  
                    3rd Qu.:2013                                                                               3rd Qu.:12.000   3rd Qu.:   142.10  
                    Max.   :2017                                                                               Max.   :12.000   Max.   :106199.00  
                    NA's   :1                                                                                  NA's   :1        NA's   :1          
      capx                ceq                  ch                 che                 csho              dlc                dlcch          
 Min.   :   0.0000   Min.   :-2564.691   Min.   :   -7.417   Min.   :    0.000   Min.   :  0.000   Min.   :    0.000   Min.   :-4380.000  
 1st Qu.:   0.2308   1st Qu.:    2.008   1st Qu.:    0.322   1st Qu.:    0.460   1st Qu.:  3.069   1st Qu.:    0.082   1st Qu.:   -0.340  
 Median :   1.7170   Median :   13.424   Median :    1.809   Median :    2.541   Median :  6.000   Median :    0.855   Median :    0.000  
 Mean   :  17.4914   Mean   :   80.877   Mean   :   18.325   Mean   :   37.028   Mean   : 13.544   Mean   :   29.413   Mean   :   11.477  
 3rd Qu.:   9.1315   3rd Qu.:   52.322   3rd Qu.:    7.336   3rd Qu.:   11.217   3rd Qu.: 11.356   3rd Qu.:    4.644   3rd Qu.:    0.516  
 Max.   :2273.5190   Max.   : 6599.000   Max.   :11442.000   Max.   :28593.000   Max.   :819.631   Max.   :19959.000   Max.   :26097.000  
 NA's   :1           NA's   :13          NA's   :580         NA's   :3           NA's   :91        NA's   :6           NA's   :3207       
     dltis                dltr               dltt                dt                ebitda             intan              ppenb         
 Min.   : -139.814   Min.   :  -6.990   Min.   :    0.00   Min.   :    0.000   Min.   :-779.000   Min.   :   0.000   Min.   :   0.000  
 1st Qu.:    0.000   1st Qu.:   0.031   1st Qu.:    0.10   1st Qu.:    0.544   1st Qu.:  -0.220   1st Qu.:   0.000   1st Qu.:   0.000  
 Median :    0.203   Median :   0.749   Median :    2.68   Median :    4.959   Median :   4.009   Median :   0.000   Median :   0.018  
 Mean   :   38.977   Mean   :  27.636   Mean   :  104.24   Mean   :  113.565   Mean   :  38.813   Mean   :  19.704   Mean   :  24.870  
 3rd Qu.:    6.961   3rd Qu.:   7.621   3rd Qu.:   30.15   3rd Qu.:   41.120   3rd Qu.:  20.149   3rd Qu.:   2.351   3rd Qu.:   8.967  
 Max.   :24784.000   Max.   :9924.000   Max.   :93279.00   Max.   :30941.000   Max.   :5745.000   Max.   :1697.380   Max.   :2081.000  
 NA's   :204         NA's   :121        NA's   :3          NA's   :8           NA's   :46         NA's   :1287       NA's   :1         
     ppent               sale                teq              txpd               txt                xint              xintd         
 Min.   :   0.000   Min.   :  -21.814   Min.   :-64.53   Min.   :-153.700   Min.   :-122.332   Min.   :   0.000   Min.   :   0.000  
 1st Qu.:   0.893   1st Qu.:    5.449   1st Qu.: 11.79   1st Qu.:   0.001   1st Qu.:   0.000   1st Qu.:   0.088   1st Qu.:   0.000  
 Median :   6.631   Median :   36.261   Median : 95.38   Median :   0.498   Median :   0.165   Median :   0.570   Median :   0.000  
 Mean   :  89.812   Mean   :  284.388   Mean   :112.21   Mean   :   8.312   Mean   :   6.899   Mean   :  12.390   Mean   :   7.216  
 3rd Qu.:  44.145   3rd Qu.:  157.199   3rd Qu.:189.22   3rd Qu.:   3.358   3rd Qu.:   2.747   3rd Qu.:   4.236   3rd Qu.:   0.324  
 Max.   :6763.770   Max.   :17782.353   Max.   :528.38   Max.   :1493.500   Max.   : 825.000   Max.   :4263.000   Max.   :1106.000  
 NA's   :4          NA's   :1           NA's   :7096     NA's   :2493       NA's   :2          NA's   :248        NA's   :5582      
    costat              fic                prcc_c            mkvalt             prcc_f             fyrc            sic               ipodate          
 Length:7153        Length:7153        Min.   :  0.001   Min.   :    0.00   Min.   :  0.010   Min.   : 1.000   Length:7153        Min.   :1968-01-02  
 Class :character   Class :character   1st Qu.:  2.750   1st Qu.:   12.98   1st Qu.:  2.875   1st Qu.: 6.000   Class :character   1st Qu.:1987-01-01  
 Mode  :character   Mode  :character   Median :  7.500   Median :   47.24   Median :  7.750   Median :12.000   Mode  :character   Median :1988-07-14  
                                       Mean   : 11.372   Mean   :  224.59   Mean   : 11.364   Mean   : 9.278                      Mean   :1989-07-10  
                                       3rd Qu.: 15.750   3rd Qu.:  150.10   3rd Qu.: 15.750   3rd Qu.:12.000                      3rd Qu.:1992-03-31  
                                       Max.   :425.000   Max.   :23352.87   Max.   :425.000   Max.   :12.000                      Max.   :2007-05-31  
                                       NA's   :1730      NA's   :1826       NA's   :1825      NA's   :1                           NA's   :1           
    leverage         tangibility          bvoe           market_to_book         logsale       profitability       
 Min.   :  0.0000   Min.   :0.0000   Min.   : -696.177   Min.   :   0.1994   Min.   :-6.908   Min.   :-154.00000  
 1st Qu.:  0.0567   1st Qu.:0.1212   1st Qu.:    4.997   1st Qu.:   1.1388   1st Qu.: 1.695   1st Qu.:  -0.03869  
 Median :  0.2528   Median :0.2891   Median :   24.613   Median :   1.5957   Median : 3.591   Median :   0.10717  
 Mean   :  0.4958   Mean   :0.3161   Mean   :  175.345   Mean   :   2.9342   Mean   : 3.336   Mean   :  -0.12846  
 3rd Qu.:  0.4833   3rd Qu.:0.4637   3rd Qu.:   85.069   3rd Qu.:   2.6794   3rd Qu.: 5.058   3rd Qu.:   0.18165  
 Max.   :881.0000   Max.   :0.9919   Max.   :29871.000   Max.   :1673.3610   Max.   : 9.786   Max.   :  13.52381  
 NA's   :8          NA's   :4        NA's   :8           NA's   :1829        NA's   :1        NA's   :46          
> sapply(mydataclean %>% select_if(is.numeric), sd, na.rm = TRUE) # find standard deviations
         fyear            fyr             at           capx            ceq             ch            che           csho            dlc          dlcch 
      2.819811       3.528615    2191.606746      79.564780     350.470242     234.781022     619.897072      36.766458     494.945234     462.688449 
         dltis           dltr           dltt             dt         ebitda          intan          ppenb          ppent           sale            teq 
    450.425646     197.600658    1330.418862     888.141226     189.871737      93.994706     111.555069     348.508065    1034.112019     117.951605 
          txpd            txt           xint          xintd         prcc_c         mkvalt         prcc_f           fyrc       leverage    tangibility 
     48.575981      39.518817      95.124674      56.040508      14.998897     923.600917      14.734855       3.518903      10.483213       0.231666 
          bvoe market_to_book        logsale  profitability 
    807.302085      23.647861       2.460297       2.495258 
> #### c)
> m <- lm(leverage ~ tangibility) # run regression of leverage on tangibility
> summary(m)

Call:
lm(formula = leverage ~ tangibility)

Residuals:
   Min     1Q Median     3Q    Max 
 -0.62  -0.45  -0.24   0.03 880.38 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept)   0.6219     0.2100   2.961  0.00308 **
tangibility  -0.3983     0.5358  -0.743  0.45722   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 10.49 on 7140 degrees of freedom
  (11 observations deleted due to missingness)
Multiple R-squared:  7.741e-05,	Adjusted R-squared:  -6.263e-05 
F-statistic: 0.5528 on 1 and 7140 DF,  p-value: 0.4572

> 
> #Manual t test for beta tangibility = 1, used 1349 DF
> betatan <- coef(m)[[2]]
> sdtan <- summary(m)$coefficients[2,2]
> t <- (betatan - 1)/sdtan
> t #tsatistic
[1] -2.609885
> p1 <- pt(t,1349)
> p1  #p-value
[1] 0.004578761
> #With linearHypotheis function, reports F statistic, have to install package "car
> library(car) 
> linearHypothesis(m,"tangibility=1")
Linear hypothesis test

Hypothesis:
tangibility = 1

Model 1: restricted model
Model 2: leverage ~ tangibility

  Res.Df    RSS Df Sum of Sq      F   Pr(>F)   
1   7141 785798                                
2   7140 785049  1    748.93 6.8115 0.009076 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> #### d)
> n <- lm(leverage ~ tangibility + market_to_book + logsale + profitability) # add other variables to the model
> summary(n)

Call:
lm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability)

Residuals:
     Min       1Q   Median       3Q      Max 
-108.725   -0.318    0.242    0.751   45.263 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -2.11149    0.07785 -27.122  < 2e-16 ***
tangibility     0.46825    0.16513   2.836  0.00459 ** 
market_to_book  0.49851    0.00170 293.306  < 2e-16 ***
logsale         0.27048    0.01717  15.755  < 2e-16 ***
profitability  -0.15913    0.05727  -2.779  0.00548 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.639 on 5271 degrees of freedom
  (1877 observations deleted due to missingness)
Multiple R-squared:  0.9529,	Adjusted R-squared:  0.9528 
F-statistic: 2.664e+04 on 4 and 5271 DF,  p-value: < 2.2e-16

> #Manual t test for beta tangiblity = -2*beta profitability
> nvcov <- vcov(n)
> nvcov
                 (Intercept)   tangibility market_to_book       logsale profitability
(Intercept)     6.060944e-03 -6.129065e-03  -1.332688e-06 -8.022622e-04  9.898138e-04
tangibility    -6.129065e-03  2.726862e-02   7.115191e-06 -7.067729e-04  1.739085e-04
market_to_book -1.332688e-06  7.115191e-06   2.888734e-06 -2.467325e-06  4.193243e-05
logsale        -8.022622e-04 -7.067729e-04  -2.467325e-06  2.947397e-04 -3.169635e-04
profitability   9.898138e-04  1.739085e-04   4.193243e-05 -3.169635e-04  3.279267e-03
> betatan2 <- coef(n)[[2]]
> betaprof <- coef(n)[[5]]
> sdtp <- sqrt(2.486722e-03 + 4 * 2.363806e-04 + 4 * -9.986766e-06 )
> t2 <- (betatan2 + 2 * betaprof)/sdtp
> t2 #t statistic
[1] 2.575262
> p2 <- (1-pt(t2,5276)) #I used 671 degrees of freedom because regression says 668 for 5 coefficients 
> p2 #p-value
[1] 0.005021701
> # using linearHypothesis function, but gives F statistics, package car
> linearHypothesis(n,"tangibility=-2*profitability")
Linear hypothesis test

Hypothesis:
tangibility  + 2 profitability = 0

Model 1: restricted model
Model 2: leverage ~ tangibility + market_to_book + logsale + profitability

  Res.Df   RSS Df Sum of Sq      F Pr(>F)
1   5272 36714                           
2   5271 36710  1     3.814 0.5476 0.4593
> #### e)
> levels(factor(fyear)) # first level is 2005
 [1] "2005" "2006" "2007" "2008" "2009" "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017"
> levels(factor(substr(sic, 1, 2)))
 [1] "01" "07" "10" "13" "14" "15" "16" "17" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "41"
[30] "42" "44" "45" "47" "48" "49" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "60" "61" "62" "63" "64" "65" "67" "70" "72" "73" "75" "76" "78"
[59] "79" "80" "81" "82" "83" "87" "89" "99"
> d <- lm(leverage ~ tangibility 
+         + market_to_book 
+         + logsale 
+         + profitability 
+         + factor(fyear) 
+         + factor(substr(sic, 1, 2)))
> summary(d) # we get too many independent variables that are not significant

Call:
lm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability + factor(fyear) + factor(substr(sic, 1, 2)))

Residuals:
     Min       1Q   Median       3Q      Max 
-103.529   -0.307    0.191    0.668   42.971 

Coefficients:
                             Estimate Std. Error t value Pr(>|t|)    
(Intercept)                 -1.962680   0.459022  -4.276 1.94e-05 ***
tangibility                  0.780663   0.209680   3.723 0.000199 ***
market_to_book               0.494397   0.001795 275.445  < 2e-16 ***
logsale                      0.261115   0.018740  13.934  < 2e-16 ***
profitability               -0.183972   0.057073  -3.223 0.001274 ** 
factor(fyear)2006            0.205465   0.271090   0.758 0.448532    
factor(fyear)2007            0.265684   0.251808   1.055 0.291426    
factor(fyear)2008            0.176986   0.248957   0.711 0.477172    
factor(fyear)2009            0.095746   0.248099   0.386 0.699573    
factor(fyear)2010            0.255144   0.247721   1.030 0.303075    
factor(fyear)2011            0.004318   0.242764   0.018 0.985810    
factor(fyear)2012           -0.084665   0.239984  -0.353 0.724259    
factor(fyear)2013           -0.064394   0.237031  -0.272 0.785886    
factor(fyear)2014            0.047411   0.254404   0.186 0.852168    
factor(fyear)2015           -0.343200   0.270045  -1.271 0.203822    
factor(fyear)2016           -0.451707   0.366939  -1.231 0.218373    
factor(fyear)2017            0.291301   1.008588   0.289 0.772730    
factor(substr(sic, 1, 2))07  0.337724   2.618614   0.129 0.897386    
factor(substr(sic, 1, 2))10 -4.055294   1.892697  -2.143 0.032192 *  
factor(substr(sic, 1, 2))13  0.027587   0.574241   0.048 0.961686    
factor(substr(sic, 1, 2))14 -0.123424   1.053297  -0.117 0.906723    
factor(substr(sic, 1, 2))15  0.968095   0.742969   1.303 0.192630    
factor(substr(sic, 1, 2))16 -0.350988   0.626368  -0.560 0.575262    
factor(substr(sic, 1, 2))17 -0.204574   0.686552  -0.298 0.765735    
factor(substr(sic, 1, 2))20 -0.226247   0.438576  -0.516 0.605970    
factor(substr(sic, 1, 2))21 -3.299938   2.623629  -1.258 0.208529    
factor(substr(sic, 1, 2))22  0.157087   0.497945   0.315 0.752417    
factor(substr(sic, 1, 2))23  0.073333   0.909397   0.081 0.935732    
factor(substr(sic, 1, 2))24  0.222234   0.619788   0.359 0.719935    
factor(substr(sic, 1, 2))25  0.450036   0.620118   0.726 0.468039    
factor(substr(sic, 1, 2))26 -0.607344   0.519380  -1.169 0.242311    
factor(substr(sic, 1, 2))27 -0.409262   0.485147  -0.844 0.398941    
factor(substr(sic, 1, 2))28 -0.816020   0.411462  -1.983 0.047395 *  
factor(substr(sic, 1, 2))29 -0.276580   0.948301  -0.292 0.770560    
factor(substr(sic, 1, 2))30  0.054434   0.476087   0.114 0.908975    
factor(substr(sic, 1, 2))31  0.151332   1.222496   0.124 0.901487    
factor(substr(sic, 1, 2))32 -0.159173   0.666871  -0.239 0.811359    
factor(substr(sic, 1, 2))33 -0.120907   0.459991  -0.263 0.792679    
factor(substr(sic, 1, 2))34  0.010480   0.511338   0.020 0.983649    
factor(substr(sic, 1, 2))35 -0.208267   0.423579  -0.492 0.622963    
factor(substr(sic, 1, 2))36 -0.340910   0.411032  -0.829 0.406916    
factor(substr(sic, 1, 2))37 -0.240448   0.469211  -0.512 0.608356    
factor(substr(sic, 1, 2))38 -0.852362   0.415121  -2.053 0.040095 *  
factor(substr(sic, 1, 2))39 -0.039166   0.532515  -0.074 0.941373    
factor(substr(sic, 1, 2))41  0.361479   2.618476   0.138 0.890207    
factor(substr(sic, 1, 2))42  0.021420   0.524627   0.041 0.967434    
factor(substr(sic, 1, 2))44 -0.133812   0.702010  -0.191 0.848837    
factor(substr(sic, 1, 2))45  0.152823   0.626136   0.244 0.807184    
factor(substr(sic, 1, 2))47  8.799343   1.215382   7.240 5.14e-13 ***
factor(substr(sic, 1, 2))48 -0.047969   0.447153  -0.107 0.914574    
factor(substr(sic, 1, 2))49 -0.166944   0.478259  -0.349 0.727054    
factor(substr(sic, 1, 2))50  0.286467   0.473540   0.605 0.545240    
factor(substr(sic, 1, 2))51  0.159683   0.491241   0.325 0.745148    
factor(substr(sic, 1, 2))52 -0.036168   0.726179  -0.050 0.960279    
factor(substr(sic, 1, 2))53 -0.431208   0.505304  -0.853 0.393497    
factor(substr(sic, 1, 2))54 -0.679682   0.492630  -1.380 0.167738    
factor(substr(sic, 1, 2))55 -0.120552   0.601497  -0.200 0.841160    
factor(substr(sic, 1, 2))56 -0.478137   0.519090  -0.921 0.357038    
factor(substr(sic, 1, 2))57 -0.239934   0.540196  -0.444 0.656945    
factor(substr(sic, 1, 2))58 -0.768568   0.436033  -1.763 0.078020 .  
factor(substr(sic, 1, 2))59 -0.720271   0.483137  -1.491 0.136069    
factor(substr(sic, 1, 2))60  0.293487   1.353129   0.217 0.828299    
factor(substr(sic, 1, 2))61  1.065367   0.575641   1.851 0.064262 .  
factor(substr(sic, 1, 2))62 -0.073776   0.744016  -0.099 0.921015    
factor(substr(sic, 1, 2))63 -0.113718   0.506343  -0.225 0.822309    
factor(substr(sic, 1, 2))64 -1.043422   0.620751  -1.681 0.092842 .  
factor(substr(sic, 1, 2))65  0.499398   0.543035   0.920 0.357803    
factor(substr(sic, 1, 2))67  0.901287   0.428846   2.102 0.035631 *  
factor(substr(sic, 1, 2))70 -3.790221   0.646063  -5.867 4.72e-09 ***
factor(substr(sic, 1, 2))72 -0.886540   0.687483  -1.290 0.197266    
factor(substr(sic, 1, 2))73 -0.349727   0.409018  -0.855 0.392568    
factor(substr(sic, 1, 2))75  0.229349   0.757077   0.303 0.761948    
factor(substr(sic, 1, 2))76  0.158445   1.053071   0.150 0.880407    
factor(substr(sic, 1, 2))78 -0.128406   0.504152  -0.255 0.798967    
factor(substr(sic, 1, 2))79 -0.044401   0.517245  -0.086 0.931596    
factor(substr(sic, 1, 2))80  0.060300   0.425858   0.142 0.887404    
factor(substr(sic, 1, 2))81  1.326767   2.620276   0.506 0.612635    
factor(substr(sic, 1, 2))82 -0.404595   0.640478  -0.632 0.527606    
factor(substr(sic, 1, 2))83  0.205856   0.740741   0.278 0.781096    
factor(substr(sic, 1, 2))87  0.090137   0.460366   0.196 0.844779    
factor(substr(sic, 1, 2))99 -0.230608   0.471851  -0.489 0.625054    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.586 on 5195 degrees of freedom
  (1877 observations deleted due to missingness)
Multiple R-squared:  0.9554,	Adjusted R-squared:  0.9547 
F-statistic:  1391 on 80 and 5195 DF,  p-value: < 2.2e-16

> # we should group year into after 2009 and other. We should also group SIC codes for relevance
> 
> mydataclean$dummyYear <- as.numeric(fyear > 2009) # variable has value 1 for years > 2009 and 0 for <= 2009
> temp1 <- levels(factor(substr(sic, 1, 2)))
> temp1
 [1] "01" "07" "10" "13" "14" "15" "16" "17" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "41"
[30] "42" "44" "45" "47" "48" "49" "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "60" "61" "62" "63" "64" "65" "67" "70" "72" "73" "75" "76" "78"
[59] "79" "80" "81" "82" "83" "87" "89" "99"
> temp1 <- temp1[-c(3, 4, 5, 6, 7, 8, 46, 47, 48, 49, 50, 51, 52)]
> temp1
 [1] "01" "07" "20" "21" "22" "23" "24" "25" "26" "27" "28" "29" "30" "31" "32" "33" "34" "35" "36" "37" "38" "39" "41" "42" "44" "45" "47" "48" "49"
[30] "50" "51" "52" "53" "54" "55" "56" "57" "58" "59" "70" "72" "73" "75" "76" "78" "79" "80" "81" "82" "83" "87" "89" "99"
> mydataclean$dummySic <- as.numeric(substr(sic, 1, 2) %in% temp1) # this gives us 0 for firms that belong to finance, insurance, real estate, construction and mining industries
> detach(mydataclean) # this is to update DataFrame that is attached
> attach(mydataclean)
> d2 <- lm(leverage ~ tangibility 
+          + market_to_book 
+          + logsale 
+          + profitability 
+          + dummyYear 
+          + dummySic)
> summary(d2) # now we have more significant results

Call:
lm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability + dummyYear + dummySic)

Residuals:
     Min       1Q   Median       3Q      Max 
-108.377   -0.314    0.255    0.749   44.927 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)    -1.377706   0.132165 -10.424  < 2e-16 ***
tangibility     0.655246   0.170111   3.852 0.000119 ***
market_to_book  0.498643   0.001693 294.617  < 2e-16 ***
logsale         0.275204   0.017186  16.014  < 2e-16 ***
profitability  -0.174299   0.057064  -3.054 0.002266 ** 
dummyYear      -0.221927   0.076853  -2.888 0.003897 ** 
dummySic       -0.745291   0.119738  -6.224 5.21e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.628 on 5269 degrees of freedom
  (1877 observations deleted due to missingness)
Multiple R-squared:  0.9533,	Adjusted R-squared:  0.9532 
F-statistic: 1.792e+04 on 6 and 5269 DF,  p-value: < 2.2e-16

> #### f)
> coef_test(d, vcov = "CR1", 
+           cluster = gvkey, test = "naive-t")[1:2,]
        Coef. Estimate    SE t-stat p-val (naive-t) Sig.
1 (Intercept)   -1.963 0.474  -4.14          <0.001  ***
2 tangibility    0.781 0.409   1.91          0.0565    .
> coef_test(d, vcov = "CR2", 
+           cluster = gvkey, test = "Satterthwaite")[1:2,] # test for d from e)
        Coef. Estimate    SE t-stat  d.f. p-val (Satt) Sig.
1 (Intercept)   -1.963 0.691  -2.84  14.5       0.0127    *
2 tangibility    0.781 0.451   1.73 304.2       0.0848    .
> coef_test(d2, vcov = "CR1", 
+           cluster = gvkey, test = "naive-t")[1:2,]
        Coef. Estimate    SE t-stat p-val (naive-t) Sig.
1 (Intercept)   -1.378 0.223  -6.18          <0.001  ***
2 tangibility    0.655 0.147   4.45          <0.001  ***
> coef_test(d2, vcov = "CR2", 
+           cluster = gvkey, test = "Satterthwaite")[1:2,] # test for d2 from e)
        Coef. Estimate    SE t-stat d.f. p-val (Satt) Sig.
1 (Intercept)   -1.378 0.425  -3.24  172      0.00142   **
2 tangibility    0.655 0.172   3.80  378      < 0.001  ***
> #### g)   -   we will first take d from e)
> library(sandwich)
> library(lmtest)
> library(plm)
> mydataclean.p <- pdata.frame(mydataclean, index = c("gvkey", "fyear"))
at least one couple (id-time) has NA in at least one index dimensionin resulting pdata.frame
 to find out which, use e.g.table(index(your_pdataframe), useNA = "ifany")
> # estimate a combined time and entity fixed effects regression model via lm()
> g <- plm(leverage ~ tangibility 
+          + market_to_book 
+          + logsale 
+          + profitability 
+          + factor(fyear) 
+          + factor(substr(sic, 1, 2)), data = mydataclean.p, index = "gvkey", model = "within")
> summary(g)
Oneway (individual) effect Within Model

Call:
plm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability + factor(fyear) + factor(substr(sic, 1, 2)), 
    data = mydataclean.p, model = "within", index = "gvkey")

Unbalanced Panel: n = 1670, T = 1-12, N = 5276

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-12.623931  -0.068987   0.000000   0.070655  25.316318 

Coefficients:
                   Estimate Std. Error t-value  Pr(>|t|)    
tangibility       0.4263654  0.1131771  3.7672 0.0001677 ***
market_to_book    0.0854968  0.0017815 47.9909 < 2.2e-16 ***
logsale           0.0328096  0.0166473  1.9709 0.0488159 *  
profitability     0.3557380  0.0189463 18.7761 < 2.2e-16 ***
factor(fyear)2006 0.0571537  0.0673841  0.8482 0.3963953    
factor(fyear)2007 0.1305046  0.0654000  1.9955 0.0460655 *  
factor(fyear)2008 0.1776076  0.0653571  2.7175 0.0066094 ** 
factor(fyear)2009 0.2515915  0.0656431  3.8327 0.0001289 ***
factor(fyear)2010 0.2441269  0.0662588  3.6844 0.0002326 ***
factor(fyear)2011 0.1601493  0.0658201  2.4331 0.0150171 *  
factor(fyear)2012 0.1951250  0.0661627  2.9492 0.0032069 ** 
factor(fyear)2013 0.1770152  0.0670164  2.6414 0.0082930 ** 
factor(fyear)2014 0.2130230  0.0733137  2.9056 0.0036874 ** 
factor(fyear)2015 0.0599414  0.0854959  0.7011 0.4832839    
factor(fyear)2016 0.1842331  0.1299570  1.4176 0.1563807    
factor(fyear)2017 0.1098101  0.3275413  0.3353 0.7374517    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    2409.7
Residual Sum of Squares: 1287.8
R-Squared:      0.46559
Adj. R-Squared: 0.21477
F-statistic: 195.484 on 16 and 3590 DF, p-value: < 2.22e-16

> coef_test(g, vcov = "CR1", 
+           cluster = mydataclean.p$gvkey, test = "naive-t")[1,]
        Coef. Estimate    SE t-stat p-val (naive-t) Sig.
1 tangibility    0.426 0.287   1.48           0.138     

> coef_test(g, vcov = "CR2", 
+           cluster = mydataclean.p$gvkey, test = "Satterthwaite")[1,]
        Coef. Estimate    SE t-stat d.f. p-val (Satt) Sig.
1 tangibility    0.426 0.315   1.35  241        0.178     
> mem.maxVSize()
[1] Inf

> # obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
> coeftest(g, vcov = vcovHC, type = "HC1")

t test of coefficients:

                  Estimate Std. Error t value Pr(>|t|)   
tangibility       0.426365   0.260831  1.6346 0.102212   
market_to_book    0.085497   0.026928  3.1750 0.001511 **
logsale           0.032810   0.022746  1.4424 0.149263   
profitability     0.355738   0.252466  1.4091 0.158907   
factor(fyear)2006 0.057154   0.038970  1.4666 0.142571   
factor(fyear)2007 0.130505   0.066089  1.9747 0.048380 * 
factor(fyear)2008 0.177608   0.075916  2.3395 0.019363 * 
factor(fyear)2009 0.251592   0.138564  1.8157 0.069498 . 
factor(fyear)2010 0.244127   0.099494  2.4537 0.014187 * 
factor(fyear)2011 0.160149   0.083916  1.9085 0.056412 . 
factor(fyear)2012 0.195125   0.086166  2.2645 0.023602 * 
factor(fyear)2013 0.177015   0.095592  1.8518 0.064139 . 
factor(fyear)2014 0.213023   0.113623  1.8748 0.060899 . 
factor(fyear)2015 0.059941   0.132094  0.4538 0.650016   
factor(fyear)2016 0.184233   0.119053  1.5475 0.121834   
factor(fyear)2017 0.109810   0.116489  0.9427 0.345914   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> g2 <- plm(leverage ~ tangibility 
+           + market_to_book 
+           + logsale 
+           + profitability 
+           + dummyYear 
+           + dummySic, data = mydataclean.p, index = "gvkey", model = "within")
> summary(g2)
Oneway (individual) effect Within Model

Call:
plm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability + dummyYear + dummySic, data = mydataclean.p, 
    model = "within", index = "gvkey")

Unbalanced Panel: n = 1670, T = 1-12, N = 5276

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-12.684658  -0.061958   0.000000   0.066135  25.480326 

Coefficients:
                Estimate Std. Error t-value  Pr(>|t|)    
tangibility    0.4282800  0.1131993  3.7834 0.0001572 ***
market_to_book 0.0852214  0.0017822 47.8193 < 2.2e-16 ***
logsale        0.0437812  0.0161243  2.7152 0.0066547 ** 
profitability  0.3525714  0.0189483 18.6071 < 2.2e-16 ***
dummyYear      0.0193142  0.0244640  0.7895 0.4298753    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    2409.7
Residual Sum of Squares: 1299.8
R-Squared:      0.46059
Adj. R-Squared: 0.20984
F-statistic: 614.972 on 5 and 3601 DF, p-value: < 2.22e-16

> coef_test(g2, vcov = "CR1", 
+           cluster = mydataclean.p$gvkey, test = "naive-t")[1,]
        Coef. Estimate    SE t-stat p-val (naive-t) Sig.
1 tangibility    0.428 0.292   1.47           0.143     
> 
> coef_test(g2, vcov = "CR2", 
+           cluster = mydataclean.p$gvkey, test = "Satterthwaite")[1,]
        Coef. Estimate    SE t-stat d.f. p-val (Satt) Sig.
1 tangibility    0.428 0.319   1.34  240         0.18     
> coef_test(g2, vcov = "CR2", 
+           cluster = mydataclean.p$gvkey, test = "Satterthwaite")[1,]

> # obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
> coeftest(g2, vcov = vcovHC, type = "HC1")

t test of coefficients:

               Estimate Std. Error t value Pr(>|t|)   
tangibility    0.428280   0.261616  1.6371 0.101706   
market_to_book 0.085221   0.027137  3.1404 0.001701 **
logsale        0.043781   0.020456  2.1402 0.032404 * 
profitability  0.352571   0.251830  1.4000 0.161588   
dummyYear      0.019314   0.019645  0.9832 0.325598   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> #### h)
> h <- plm(leverage ~ tangibility 
+          + market_to_book 
+          + logsale 
+          + profitability 
+          + factor(fyear), data = mydataclean.p, index = "gvkey", model = "within")
> summary(h)
Oneway (individual) effect Within Model

Call:
plm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability + factor(fyear), data = mydataclean.p, model = "within", 
    index = "gvkey")

Unbalanced Panel: n = 1670, T = 1-12, N = 5276

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-12.623931  -0.068987   0.000000   0.070655  25.316318 

Coefficients:
                   Estimate Std. Error t-value  Pr(>|t|)    
tangibility       0.4263654  0.1131771  3.7672 0.0001677 ***
market_to_book    0.0854968  0.0017815 47.9909 < 2.2e-16 ***
logsale           0.0328096  0.0166473  1.9709 0.0488159 *  
profitability     0.3557380  0.0189463 18.7761 < 2.2e-16 ***
factor(fyear)2006 0.0571537  0.0673841  0.8482 0.3963953    
factor(fyear)2007 0.1305046  0.0654000  1.9955 0.0460655 *  
factor(fyear)2008 0.1776076  0.0653571  2.7175 0.0066094 ** 
factor(fyear)2009 0.2515915  0.0656431  3.8327 0.0001289 ***
factor(fyear)2010 0.2441269  0.0662588  3.6844 0.0002326 ***
factor(fyear)2011 0.1601493  0.0658201  2.4331 0.0150171 *  
factor(fyear)2012 0.1951250  0.0661627  2.9492 0.0032069 ** 
factor(fyear)2013 0.1770152  0.0670164  2.6414 0.0082930 ** 
factor(fyear)2014 0.2130230  0.0733137  2.9056 0.0036874 ** 
factor(fyear)2015 0.0599414  0.0854959  0.7011 0.4832839    
factor(fyear)2016 0.1842331  0.1299570  1.4176 0.1563807    
factor(fyear)2017 0.1098101  0.3275413  0.3353 0.7374517    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    2409.7
Residual Sum of Squares: 1287.8
R-Squared:      0.46559
Adj. R-Squared: 0.21477
F-statistic: 195.484 on 16 and 3590 DF, p-value: < 2.22e-16
> coef_test(h, vcov = "CR1", 
+           cluster = mydataclean.p$gvkey, test = "naive-t")[1,]
        Coef. Estimate    SE t-stat p-val (naive-t) Sig.
1 tangibility    0.426 0.287   1.48           0.138     
> 
> coef_test(h, vcov = "CR2", 
+           cluster = mydataclean.p$gvkey, test = "Satterthwaite")[1,]
        Coef. Estimate    SE t-stat d.f. p-val (Satt) Sig.
1 tangibility    0.426 0.315   1.35  241        0.178     
> # obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
> coeftest(h, vcov = vcovHC, type = "HC1")

t test of coefficients:

                  Estimate Std. Error t value Pr(>|t|)   
tangibility       0.426365   0.260831  1.6346 0.102212   
market_to_book    0.085497   0.026928  3.1750 0.001511 **
logsale           0.032810   0.022746  1.4424 0.149263   
profitability     0.355738   0.252466  1.4091 0.158907   
factor(fyear)2006 0.057154   0.038970  1.4666 0.142571   
factor(fyear)2007 0.130505   0.066089  1.9747 0.048380 * 
factor(fyear)2008 0.177608   0.075916  2.3395 0.019363 * 
factor(fyear)2009 0.251592   0.138564  1.8157 0.069498 . 
factor(fyear)2010 0.244127   0.099494  2.4537 0.014187 * 
factor(fyear)2011 0.160149   0.083916  1.9085 0.056412 . 
factor(fyear)2012 0.195125   0.086166  2.2645 0.023602 * 
factor(fyear)2013 0.177015   0.095592  1.8518 0.064139 . 
factor(fyear)2014 0.213023   0.113623  1.8748 0.060899 . 
factor(fyear)2015 0.059941   0.132094  0.4538 0.650016   
factor(fyear)2016 0.184233   0.119053  1.5475 0.121834   
factor(fyear)2017 0.109810   0.116489  0.9427 0.345914   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

> h2 <- plm(leverage ~ tangibility 
+           + market_to_book 
+           + logsale 
+           + profitability 
+           + dummyYear, data = mydataclean.p, index = "gvkey", model = "within")
> summary(h2)
Oneway (individual) effect Within Model

Call:
plm(formula = leverage ~ tangibility + market_to_book + logsale + 
    profitability + dummyYear, data = mydataclean.p, model = "within", 
    index = "gvkey")

Unbalanced Panel: n = 1670, T = 1-12, N = 5276

Residuals:
      Min.    1st Qu.     Median    3rd Qu.       Max. 
-12.684658  -0.061958   0.000000   0.066135  25.480326 

Coefficients:
                Estimate Std. Error t-value  Pr(>|t|)    
tangibility    0.4282800  0.1131993  3.7834 0.0001572 ***
market_to_book 0.0852214  0.0017822 47.8193 < 2.2e-16 ***
logsale        0.0437812  0.0161243  2.7152 0.0066547 ** 
profitability  0.3525714  0.0189483 18.6071 < 2.2e-16 ***
dummyYear      0.0193142  0.0244640  0.7895 0.4298753    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Total Sum of Squares:    2409.7
Residual Sum of Squares: 1299.8
R-Squared:      0.46059
Adj. R-Squared: 0.20984
F-statistic: 614.972 on 5 and 3601 DF, p-value: < 2.22e-16
> coef_test(h2, vcov = "CR1", 
+           cluster = mydataclean.p$gvkey, test = "naive-t")[1,]
        Coef. Estimate    SE t-stat p-val (naive-t) Sig.
1 tangibility    0.428 0.292   1.47           0.143     
> 
> coef_test(h2, vcov = "CR2", 
+           cluster = mydataclean.p$gvkey, test = "Satterthwaite")[1,]
        Coef. Estimate    SE t-stat d.f. p-val (Satt) Sig.
1 tangibility    0.428 0.319   1.34  240         0.18     
> # obtain a summary based on clusterd standard errors (adjustment for autocorrelation + heteroskedasticity)
> coeftest(h2, vcov = vcovHC, type = "HC1")

t test of coefficients:

               Estimate Std. Error t value Pr(>|t|)   
tangibility    0.428280   0.261616  1.6371 0.101706   
market_to_book 0.085221   0.027137  3.1404 0.001701 **
logsale        0.043781   0.020456  2.1402 0.032404 * 
profitability  0.352571   0.251830  1.4000 0.161588   
dummyYear      0.019314   0.019645  0.9832 0.325598   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
