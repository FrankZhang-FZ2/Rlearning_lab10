Lab 10 - Grading the professor, Pt. 2
================
Qilin Zhang
3/28/23

### Load packages and data

``` r
library(tidyverse) 
library(tidymodels)
library(openintro)
library(psych)
library(infer)
```

### Exercise 1

y = 0.06664\*x + 3.88034 R2 = 0.03502 Adj. R2 = 0.03293

``` r
evals <- evals

m_bty <- lm(
  score ~ bty_avg,
  data = evals
)
summary(m_bty)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.9246 -0.3690  0.1420  0.3977  0.9309 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.88034    0.07614   50.96  < 2e-16 ***
    ## bty_avg      0.06664    0.01629    4.09 5.08e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5348 on 461 degrees of freedom
    ## Multiple R-squared:  0.03502,    Adjusted R-squared:  0.03293 
    ## F-statistic: 16.73 on 1 and 461 DF,  p-value: 5.083e-05

### Exercise 2 - 9

\#2 y = 0.07416*x1 + 0.17239X2 + 3.74734 x1= bty_avg x2 = gender R2 =
0.05912 Adj. R2 = 0.05503 \#3 When we hold beauty score constant, male
score higher on average than female by 0.17239. When we hold gender
constant, every 1 point increase in beauty score is associated with
0.07416 in eval scores. \#4 About 5.9% of variability in eval scores is
explained by m-bty-gen. \#5 y = 0.07416*x1 + 3.91973 \#6 Male tend to
have higher rating when scored the same in beauty scores \#7 From the
previous model, it seems like intercept is the only thing that changes
when shifting from male to female \#8 The adjusted r2 increase by around
2% after adding gender as an additional predictor. It shows that gender
add some predictability above and beyond beauty scores. \#9 It seems
like introducing gender does shift the parameter estimate for beauty
score (from 0.06664 to 0.07416).

``` r
m_bty_gen <- lm(
  score ~ bty_avg + gender,
  data = evals
)
summary(m_bty_gen)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + gender, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8305 -0.3625  0.1055  0.4213  0.9314 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.74734    0.08466  44.266  < 2e-16 ***
    ## bty_avg      0.07416    0.01625   4.563 6.48e-06 ***
    ## gendermale   0.17239    0.05022   3.433 0.000652 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5287 on 460 degrees of freedom
    ## Multiple R-squared:  0.05912,    Adjusted R-squared:  0.05503 
    ## F-statistic: 14.45 on 2 and 460 DF,  p-value: 8.177e-07

\###exercise 10

Y = 0.06783x1 + 3.98155 - 0.1607 x2 - 0.12623 x3 x1 = bty_avg x2 =
tenure track or not x3 = tenured When we hold beauty score constant,
shifting in rank associates with change in eval scores as well.
Especially, tenure track professor tend to have 0.1607 less than
teaching professors and tenured tend to have 0.12623 less than teaching
professors. \#R2 seems to show only weak to no impact from rank.

``` r
m_bty_rank <- lm(
  score ~ bty_avg + rank,
  data = evals
)
summary(m_bty_rank)
```

    ## 
    ## Call:
    ## lm(formula = score ~ bty_avg + rank, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8713 -0.3642  0.1489  0.4103  0.9525 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       3.98155    0.09078  43.860  < 2e-16 ***
    ## bty_avg           0.06783    0.01655   4.098 4.92e-05 ***
    ## ranktenure track -0.16070    0.07395  -2.173   0.0303 *  
    ## ranktenured      -0.12623    0.06266  -2.014   0.0445 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5328 on 459 degrees of freedom
    ## Multiple R-squared:  0.04652,    Adjusted R-squared:  0.04029 
    ## F-statistic: 7.465 on 3 and 459 DF,  p-value: 6.88e-05

\###exercise 11+12

I think language might be unrelated to eval scores unless there is bias
against that. Also, the number of evaluation in class should have little
predictability for eval scores.

``` r
# language
m_language <- lm(
  score ~ language,
  data = evals
)
summary(m_language)
```

    ## 
    ## Call:
    ## lm(formula = score ~ language, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8897 -0.3429  0.1103  0.4103  0.8571 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          4.18966    0.02595 161.444   <2e-16 ***
    ## languagenon-english -0.24680    0.10553  -2.339   0.0198 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5413 on 461 degrees of freedom
    ## Multiple R-squared:  0.01173,    Adjusted R-squared:  0.009582 
    ## F-statistic:  5.47 on 1 and 461 DF,  p-value: 0.01978

``` r
#cls_did_eval
m_eval <- lm(
  score ~ cls_did_eval,
  data = evals
)
summary(m_eval)
```

    ## 
    ## Call:
    ## lm(formula = score ~ cls_did_eval, data = evals)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1.8545 -0.3595  0.1303  0.4269  0.8485 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  4.1469347  0.0325682 127.331   <2e-16 ***
    ## cls_did_eval 0.0007589  0.0005616   1.351    0.177    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5434 on 461 degrees of freedom
    ## Multiple R-squared:  0.003946,   Adjusted R-squared:  0.001786 
    ## F-statistic: 1.827 on 1 and 461 DF,  p-value: 0.1772

\###exercise 13 +14 I will not include cls_perc_eval because it is
essentially the same thing as cls_did_eval

``` r
m_complex <- lm(
  score ~
    rank +
    ethnicity +
    gender + 
    language +
    age +
    cls_did_eval + 
    cls_students + 
    cls_level + 
    cls_profs +
    cls_credits +
    bty_avg,
  data = evals
)
summary(m_complex)
```

    ## 
    ## Call:
    ## lm(formula = score ~ rank + ethnicity + gender + language + age + 
    ##     cls_did_eval + cls_students + cls_level + cls_profs + cls_credits + 
    ##     bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.81081 -0.31219  0.08483  0.37165  0.95661 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.957168   0.208299  18.998  < 2e-16 ***
    ## ranktenure track      -0.097165   0.082662  -1.175 0.240439    
    ## ranktenured           -0.041744   0.065533  -0.637 0.524454    
    ## ethnicitynot minority  0.167748   0.077699   2.159 0.031381 *  
    ## gendermale             0.185994   0.051880   3.585 0.000374 ***
    ## languagenon-english   -0.140321   0.108539  -1.293 0.196737    
    ## age                   -0.006753   0.003099  -2.179 0.029842 *  
    ## cls_did_eval           0.006908   0.002304   2.998 0.002864 ** 
    ## cls_students          -0.004059   0.001395  -2.910 0.003793 ** 
    ## cls_levelupper        -0.005692   0.056590  -0.101 0.919922    
    ## cls_profssingle        0.009144   0.051449   0.178 0.859015    
    ## cls_creditsone credit  0.517234   0.117531   4.401 1.35e-05 ***
    ## bty_avg                0.065052   0.016660   3.905 0.000109 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5065 on 450 degrees of freedom
    ## Multiple R-squared:  0.1553, Adjusted R-squared:  0.1328 
    ## F-statistic: 6.895 on 12 and 450 DF,  p-value: 1.688e-11

\###exercise 15 + 16

\#16 When holding all other variables constant, one point increase in
age is associate with 0.005222 point decrease in eval score. When
holding all other variables constant, shifting from male to female is
associated with 0.186761 decrease in eval score. \#17 Seem like
professors that are male, younger, and with relatively high beauty score
in classes with one credit, less students, and lower students who submit
course eval will have higer eval scores. \#18 I would not be comfortable
generalizing my findings to professors all over the world considering
the low sample size and the homogenity of the sample size (they are all
from the same univeristy I assume).

``` r
m_best <- lm(
  score ~
    ethnicity +
    gender + 
    language +
    age +
    cls_did_eval + 
    cls_students + 
    cls_credits +
    bty_avg,
  data = evals
)
summary(m_best)
```

    ## 
    ## Call:
    ## lm(formula = score ~ ethnicity + gender + language + age + cls_did_eval + 
    ##     cls_students + cls_credits + bty_avg, data = evals)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.84932 -0.31610  0.06814  0.37118  0.94225 
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            3.827108   0.172412  22.197  < 2e-16 ***
    ## ethnicitynot minority  0.174664   0.074734   2.337 0.019866 *  
    ## gendermale             0.186761   0.050638   3.688 0.000253 ***
    ## languagenon-english   -0.169595   0.103940  -1.632 0.103442    
    ## age                   -0.005222   0.002616  -1.996 0.046521 *  
    ## cls_did_eval           0.007057   0.002257   3.127 0.001878 ** 
    ## cls_students          -0.004164   0.001353  -3.078 0.002212 ** 
    ## cls_creditsone credit  0.540358   0.105292   5.132 4.26e-07 ***
    ## bty_avg                0.066005   0.016483   4.004 7.26e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.505 on 454 degrees of freedom
    ## Multiple R-squared:  0.1526, Adjusted R-squared:  0.1377 
    ## F-statistic: 10.22 on 8 and 454 DF,  p-value: 3.622e-13

Add exercise headings as needed.
