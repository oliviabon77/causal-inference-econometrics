Instrumental Variables: Immigration and Economic Output
================
Olivia Bonnette
2026-05-10

- [Overview](#overview)
- [Setup](#setup)
- [Data](#data)
- [Step 1: OLS Benchmark](#step-1-ols-benchmark)
- [Step 2: First Stage](#step-2-first-stage)
- [Step 3: IV Regression (2SLS)](#step-3-iv-regression-2sls)
- [Step 4: OLS vs. IV Comparison](#step-4-ols-vs-iv-comparison)
- [Instrument Validity Assessment](#instrument-validity-assessment)
- [Packages Used](#packages-used)

## Overview

This lab applies **Instrumental Variables (IV) regression** to estimate
the causal effect of immigration on economic output. We use the **Card
(2001) enclave instrument** — predicting current immigrant inflows from
historical settlement patterns — applied to state-level data from a
paper by Giovanni Peri.

The core identification challenge: immigrants do not move randomly. They
tend to relocate to places with strong labor demand, which also drives
up economic output independently. Simple OLS conflates the causal effect
of immigration with these underlying economic conditions. IV corrects
for this endogeneity.

------------------------------------------------------------------------

## Setup

``` r
library(AER)       # ivreg() for IV regression
library(lmtest)    # coeftest() for hypothesis testing
library(sandwich)  # vcovCL() and vcovHC() for robust standard errors
library(haven)     # read_dta() for Stata files
library(tidyverse) # data manipulation and visualization
```

------------------------------------------------------------------------

## Data

``` r
# Load Peri state-panel data
# Ensure peri_iv_data.dta is in your working directory
peri_iv_data <- read_dta("peri_iv_data (1).dta")

head(peri_iv_data)
```

    ## # A tibble: 6 × 8
    ##    year statefip foreign_pop foreign_imputed  us_pop state_code border_distance
    ##   <dbl>    <dbl>       <dbl>           <dbl>   <dbl> <chr>                <dbl>
    ## 1  1960       35       19025           19025  516678 NM                    352.
    ## 2  1960       24       86065           86065 1832621 MD                   2437.
    ## 3  1960       55      158797          158797 2305508 WI                   1911.
    ## 4  1960        9      261192          261192 1383818 CT                   2838.
    ## 5  1960       37       15554           15554 2716344 NC                   2017.
    ## 6  1960       53      161339          161339 1619639 WA                   1702.
    ## # ℹ 1 more variable: gsp_worker <dbl>

``` r
summary(peri_iv_data)
```

    ##       year         statefip      foreign_pop      foreign_imputed  
    ##  Min.   :1960   Min.   : 1.00   Min.   :   3488   Min.   :   4185  
    ##  1st Qu.:1970   1st Qu.:16.00   1st Qu.:  26014   1st Qu.:  24582  
    ##  Median :1985   Median :29.00   Median :  70454   Median :  64621  
    ##  Mean   :1984   Mean   :28.96   Mean   : 355276   Mean   : 355276  
    ##  3rd Qu.:2000   3rd Qu.:42.00   3rd Qu.: 275541   3rd Qu.: 252395  
    ##  Max.   :2006   Max.   :56.00   Max.   :9160466   Max.   :8768017  
    ##      us_pop          state_code        border_distance    gsp_worker    
    ##  Min.   :  109569   Length:306         Min.   : 214.2   Min.   : 32285  
    ##  1st Qu.:  721633   Class :character   1st Qu.:1243.1   1st Qu.: 60822  
    ##  Median : 2043850   Mode  :character   Median :1706.2   Median : 70912  
    ##  Mean   : 2935922                      Mean   :1803.7   Mean   : 76765  
    ##  3rd Qu.: 3694009                      3rd Qu.:2436.7   3rd Qu.: 85832  
    ##  Max.   :17432292                      Max.   :4177.2   Max.   :327228

**Dataset structure:**

| Variable | Description |
|----|----|
| `year` | Survey year |
| `statefip` | State FIPS code |
| `foreign_pop` | Actual foreign-born population |
| `foreign_imputed` | Card-style instrument: predicted immigrant share based on historical enclaves |
| `us_pop` | US-born population |
| `state_code` | State abbreviation |
| `border_distance` | Distance from the border (miles) |
| `gsp_worker` | Gross State Product per worker (outcome variable) |

------------------------------------------------------------------------

## Step 1: OLS Benchmark

Before instrumenting, we estimate a naive OLS regression of GSP per
worker on foreign and US-born population. This provides a benchmark —
but it is likely biased because immigration is endogenous.

``` r
gspreg_ols <- lm(gsp_worker ~ us_pop + foreign_pop, data = peri_iv_data)

# Cluster standard errors by state to account for within-state correlation
gspreg_ols_cluster <- coeftest(
  gspreg_ols,
  vcov = vcovCL(gspreg_ols, cluster = ~ state_code)
)

gspreg_ols_cluster
```

    ## 
    ## t test of coefficients:
    ## 
    ##                Estimate  Std. Error t value  Pr(>|t|)    
    ## (Intercept)  7.9346e+04  5.9663e+03 13.2989 < 2.2e-16 ***
    ## us_pop      -2.1921e-03  1.4492e-03 -1.5127  0.131409    
    ## foreign_pop  1.0850e-02  4.0562e-03  2.6750  0.007879 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Interpretation:**

The OLS coefficient on `foreign_pop` is approximately **0.01085**,
suggesting each additional 10,000 immigrants is associated with roughly
\$108.50 higher GSP per worker, holding the US-born population constant.

**Is this causal?** No. Immigrants tend to move to economically stronger
states. Those positive demand shocks simultaneously attract immigrants
and raise GSP per worker, creating upward omitted variable bias. The OLS
estimate cannot be interpreted causally.

**Why cluster by state?** Observations within the same state share
common shocks (policy changes, regional economic cycles). Failing to
cluster would understate standard errors and produce misleadingly
precise inference.

------------------------------------------------------------------------

## Step 2: First Stage

The Card instrument (`foreign_imputed`) predicts how many immigrants a
state should receive based on: 1. Where immigrants from each origin
country historically settled 2. How much national immigration from each
origin country grew over time

This generates variation in immigrant inflows driven by national supply
shocks and historical geography — plausibly exogenous to local economic
conditions.

``` r
first_stage <- lm(foreign_pop ~ foreign_imputed + us_pop, data = peri_iv_data)

first_stage_cluster <- coeftest(
  first_stage,
  vcov = vcovCL(first_stage, cluster = ~ state_code)
)

first_stage_cluster
```

    ## 
    ## t test of coefficients:
    ## 
    ##                    Estimate  Std. Error t value  Pr(>|t|)    
    ## (Intercept)     -8.3537e+04  4.3726e+04 -1.9105   0.05702 .  
    ## foreign_imputed  7.8091e-01  1.7934e-01  4.3543 1.828e-05 ***
    ## us_pop           5.4966e-02  2.7340e-02  2.0105   0.04527 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Interpretation:** The instrument is a **strong and statistically
significant predictor** of actual foreign population (coefficient ≈
0.78, p \< 0.001). A one-unit increase in the imputed immigrant share
translates into a roughly 78-cent increase in realized foreign
population after controlling for the US-born population. Instrument
strength will be confirmed formally via the weak instrument diagnostic
in the IV stage.

------------------------------------------------------------------------

## Step 3: IV Regression (2SLS)

``` r
gspreg_iv <- ivreg(
  gsp_worker ~ us_pop + foreign_pop | us_pop + foreign_imputed,
  data = peri_iv_data
)

# HC3 robust standard errors clustered by state
coeftest(gspreg_iv, vcov = vcovHC(gspreg_iv, type = "HC3", cluster = "state_code"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                Estimate  Std. Error t value  Pr(>|t|)    
    ## (Intercept)  8.0660e+04  3.7870e+03 21.2990 < 2.2e-16 ***
    ## us_pop      -3.0874e-03  1.2379e-03 -2.4942  0.013159 *  
    ## foreign_pop  1.4549e-02  5.1959e-03  2.8002  0.005436 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Formal diagnostics including weak instrument F-test and Wu-Hausman endogeneity test
summary(gspreg_iv, vcov = sandwich, diagnostics = TRUE)
```

    ## 
    ## Call:
    ## ivreg(formula = gsp_worker ~ us_pop + foreign_pop | us_pop + 
    ##     foreign_imputed, data = peri_iv_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -48197 -16110  -3976  10567 246797 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.066e+04  3.736e+03  21.589  < 2e-16 ***
    ## us_pop      -3.087e-03  1.140e-03  -2.709 0.007134 ** 
    ## foreign_pop  1.455e-02  4.018e-03   3.621 0.000344 ***
    ## 
    ## Diagnostic tests:
    ##                  df1 df2 statistic  p-value    
    ## Weak instruments   1 303     43.08 2.27e-10 ***
    ## Wu-Hausman         1 302     10.19  0.00156 ** 
    ## Sargan             0  NA        NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 30690 on 303 degrees of freedom
    ## Multiple R-Squared: 0.03677, Adjusted R-squared: 0.03042 
    ## Wald test: 6.833 on 2 and 303 DF,  p-value: 0.001251

**Key diagnostics:**

| Test | Statistic | Interpretation |
|----|----|----|
| Weak instruments F-stat | **43.08** (p \< 0.001) | Instrument is strong; well above the conventional F \> 10 threshold |
| Wu-Hausman | 10.19 (p = 0.0016) | Statistically significant endogeneity — OLS is biased; IV is preferred |
| Sargan | Not applicable | Single instrument, exactly identified — overidentification test not available |

------------------------------------------------------------------------

## Step 4: OLS vs. IV Comparison

``` r
comparison <- data.frame(
  Specification = c("OLS (biased)", "IV / 2SLS (causal)"),
  Coefficient = c(0.01085, 0.01455),
  Interpretation = c(
    "Likely upward biased due to endogeneity",
    "Causal estimate after instrumenting"
  )
)

knitr::kable(comparison, caption = "Effect of foreign population on GSP per worker")
```

| Specification      | Coefficient | Interpretation                          |
|:-------------------|------------:|:----------------------------------------|
| OLS (biased)       |     0.01085 | Likely upward biased due to endogeneity |
| IV / 2SLS (causal) |     0.01455 | Causal estimate after instrumenting     |

Effect of foreign population on GSP per worker

**Discussion:**

The IV estimate (0.01455) is **larger** than the OLS estimate (0.01085).
This direction of change suggests OLS may have been *downward* biased —
perhaps because immigrants also move to economically weaker states with
lower wages, partially offsetting the positive economic shock bias.
After removing this endogeneity via the Card instrument, the estimated
causal effect increases.

The Wu-Hausman test confirms that the difference between OLS and IV is
statistically significant (p = 0.0016), validating the use of IV over
OLS.

------------------------------------------------------------------------

## Instrument Validity Assessment

The Card enclave instrument satisfies:

- **Relevance:** F-stat = 43 confirms the instrument is a strong
  predictor of immigrant inflows (far above the conventional F \> 10
  threshold for weak instrument concerns)
- **Exclusion restriction:** The instrument affects GSP only through its
  effect on immigration — plausible, since historical settlement
  patterns are determined decades before the outcome period

**Potential concern:** Long-run city characteristics that originally
attracted immigrants may also independently affect current wages. This
would violate the exclusion restriction if those underlying conditions
persist and correlate with both the instrument and the outcome
independently of immigration. This caveat should be acknowledged in any
policy application.

------------------------------------------------------------------------

## Packages Used

- `AER`: Instrumental variable regression via `ivreg()`
- `lmtest`: Coefficient testing via `coeftest()`
- `sandwich`: Clustered and heteroskedasticity-robust standard errors
- `haven`: Import Stata `.dta` files
- `tidyverse`: Data manipulation and visualization
