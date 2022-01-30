
# Sample Size for CLInical Prediction models (ssclip)

<!-- badges: start -->
<!-- badges: end -->

The goal of "ssclip" is to calculate minimum sample size for external validation of clinical prediction models to target precise estimates of predictive performance.

There are 6 functions to target different criteria of predictive performance:

 1. R2 :The proportion of variance explained.
 2. Calibration-in-the-large(CITL) :  Agreement between predicted and observed outcome values on average.
 3. Calibration slope : agreement between predicted and observed values across the range of predicted values.
 4. Residual variances(small multiplicative margin of error (MMOE) around the true value) :To target variance of observed outcome values.
 5. Sensitivity : To target model's ability to predict true positives in the dichotomous outcome
 6. Specificity : To target model's ability to predict true negatives in the dichotomous outcome

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("Ayushi-712/ssclip")
```
### Step-by-step guide
![Alt desc](https://github.com/Ayushi-712/ssclip/blob/master/Data/sim8766-fig-0002-m.jpg)
```
```
## Examples (For continuous outcome)
### 1.Precise estimate of R2val
#### For Eg.Sample size to target a 95% confidence interval for R2val that has a narrow width of about 0.1 i.e SE of R2val = 0.0255 and expected R2val = 0.5.One can give parameters as R2val= 0.5,width=0.1,alpha=0.05 in the ss_R2val function.
```{r example }
library(ssclip)
ss_R2val( R2val= 0.5,width=0.1,alpha=0.05 )
```
#### Output
![Alt desc](https://github.com/Ayushi-712/ssclip/blob/master/Data/ss_R2val.png)
#### for different combinations of parameters one can pass a vector instead of single value.
```
ss_R2val( R2val= c(0.6,0.9),width=c( 0.1),alpha=c( 0.01,0.05) )
```
#### Output
![Alt desc](https://github.com/Ayushi-712/ssclip/blob/master/Data/ss_R2val_diff_comb.png)

### 2.Precise estimate of CITL(calibration-in-the-large)
#### For Eg. Sample size to target SE of CITL model of 2.55 (width = 10) at 95% confidence interval , R2 CITL = R2val = 0.5 and variance of the observed Yi = 400.
```{r example }
ss_citl( R2= 0.5,width = 10,alpha = 0.05, varY = 400)
```
### 3. Precise estimate of calibration slope
#### Eg. Sample size to target a 95% confidence interval for 𝜆cal that has a narrow width ≤ 0.2 (eg, if the calibration slope was 1, the confidence interval would be 0.9 to 1.1, confidence intervals derived by 𝜆̂cal ± 1.96SE𝜆̂cal) and expected R2val = 0.5

```
ss_cal_slope( R2= 0.5,width = 0.2,alpha = 0.05, lambda = 1)
```
### 4. Precise estimates of residual variances (small multiplicative margin of error (MMOE) around the true value)
#### Eg.Sample size for margin of error of within 10% (1.0 <=MMOE <=1.1) of the true value at 95% confidence level.
```
ss_res_var( max_MOE=1.1, alpha=0.05)
```
## Examples (For dichotomous outcome)
### 5. To target sensitivity
#### Eg .Sample size to target 80% sensitivity at 95% confidence level, maximum margin of error 5% for a precision of 95% and 30% of the population with particular disease.

```
ss_sens( alpha=0.05, se= 0.8, d=0.05,prev=0.3)
```
### 6.To target specificity
#### Eg .Sample size to target 50% specificity at 95% confidence level, maximum margin of error 5% for a precision of 95% and assuming 30% of the population with particular disease.
```
ss_spec( alpha=0.05, sp= 0.5, d=0.05,prev=0.3)
```
## References
1. Archer L, Snell KIE, Ensor J, Hudda MT, Collins GS, Riley RD. Minimum sample size for external validation of a clinical prediction model with a continuous outcome. Stat Med. 2020;40:133-46.

