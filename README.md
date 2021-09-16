
# ssclip

<!-- badges: start -->
<!-- badges: end -->

The goal of "ssclip" is to calculate minimum sample size for external validation of clinical prediction models to target precise estimates of predictive performance.

There are 6 functions to target different criteria of predictive performance:

 1. R2 :The proportion of variance explained.
 2. Calibration-in-the-large(CITL) :  Agreement between predicted and observed outcome values on average.
 3. Calibration slope : agreement between predicted and observed values across the range of predicted values.
 4. Residual variances :To target variance of observed outcome values.
 5. Sensitivity : To target model's ability to predict true positives in the dichotomous outcome
 6. Specificity : To target model's ability to predict true negatives in the dichotomous outcome

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Ayushi-712/ssclip")
```
## Example
1.Precise estimate of R2val

```{r example }
library(ssclip)
##1. R2 :The proportion of variance explained

#Eg.To target a 95% confidence interval for R2val that has a narrow width of about 0.1
#i.e SE of R2val will be 0.0255 and assuming R2val is 0.5.
ss_R2val( R2val= 0.5,width=0.1,alpha=0.05 )

#for different combinations of parameters one can pass a vector instead of single value.
ss_R2val( R2val= c(0.6,0.9),width=c( 0.1),alpha=c( 0.01,0.05) )

```
2.  Precise estimate of CITL(calibration-in-the-large)

```{r example }

# Eg. To target SE of CITL model of 2.55 (width = 10)at 95%
#confidence interval , Assuming R2 CITL = R2val = 0.5 and
# variance of the observed Yi is 400
ss_citl( R2= 0.5,width = 10,alpha = 0.05, varY = 400)

```
3. Precise estimate of calibration slope
```

**# Eg. Sample size to target a 95% confidence interval for 𝜆cal
# that has a narrow width ≤ 0.2 (eg, if the calibration slope was 1,
# the confidence interval would be 0.9 to 1.1 assuming confidence
# intervals derived by 𝜆̂cal ± 1.96SE𝜆̂cal) and assuming R2val = 0.5 .**
ss_cal_slope( R2= 0.5,width = 0.2,alpha = 0.05, lambda = 1)

```
