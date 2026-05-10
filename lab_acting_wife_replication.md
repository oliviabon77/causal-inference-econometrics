Acting Wife: Replicating Bursztyn, Fujiwara & Pallais (2017)
================
Olivia Bonnette
2026-05-10

- [Overview](#overview)
- [Setup](#setup)
- [Data](#data)
- [Step 1: Replicate Panel A of Table
  4](#step-1-replicate-panel-a-of-table-4)
- [Step 2: Interaction Specification (Women
  Only)](#step-2-interaction-specification-women-only)
- [Step 3: Last Row of Table 4 — Full Sample
  Interaction](#step-3-last-row-of-table-4--full-sample-interaction)
- [Step 4: Sensitivity Analysis — Alternative Coding of
  “Single”](#step-4-sensitivity-analysis--alternative-coding-of-single)
- [Packages Used](#packages-used)

## Overview

This lab replicates Panel A of Table 4 from **“Acting Wife: Marriage
Market Incentives and Labor Market Investments”** (Bursztyn, Fujiwara &
Pallais, 2017, *American Economic Review*).

The paper uses a randomized experiment among MBA students to test
whether single women **strategically conceal ambition** when their
answers are observable to male peers. Participants were randomized into
a public condition (answers visible to classmates) or a private
condition (answers confidential). The authors document that single women
— but not partnered women or men — significantly downplay career
preferences in the public condition.

**Key causal identification:** Random assignment to public vs. private
conditions eliminates selection bias. The `public` treatment dummy is
exogenous by design.

------------------------------------------------------------------------

## Setup

``` r
library(lmtest)    # coeftest() for hypothesis testing
library(sandwich)  # vcovHC() for heteroskedasticity-robust standard errors
library(haven)     # read_dta() for Stata files
library(jtools)    # export_summs() for formatted regression tables
library(tidyverse) # data manipulation and visualization
```

------------------------------------------------------------------------

## Data

``` r
# Load Acting Wife experimental data
# Ensure acting_wife_data_set.dta is in your working directory
acting_wife <- read_dta("acting_wife_data_set (1).dta")

# Create analysis variables
acting_wife <- acting_wife %>%
  mutate(
    female        = 1 - male,
    single        = ifelse(maritalstatus == 0, 1, 0),
    single_female = ifelse(female == 1 & single == 1, 1, 0)
  )

glimpse(acting_wife)
```

    ## Rows: 355
    ## Columns: 16
    ## $ male                <dbl> 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1,…
    ## $ maritalstatus       <dbl+lbl>  4,  0,  4,  3,  0,  1,  0,  3, NA,  1,  0,  4…
    ## $ desiredcompensation <chr> "150-175", ">250", "125-150", "100-150", "200-225"…
    ## $ travel              <dbl+lbl> 1, 4, 1, 2, 1, 2, 2, 4, 4, 1, 3, 2, 4, 4, 3, 2…
    ## $ hourswork           <chr> "51-60", "41-50", "41-50", "41-50", "41-50", "40",…
    ## $ tendtolead          <dbl+lbl> 3, 4, 5, 4, 4, 4, 2, 4, 5, 4, 4, 5, 4, 4, 4, 4…
    ## $ moreambitious       <dbl+lbl> 2, 5, 4, 4, 4, 4, 3, 4, 5, 5, 3, 4, 5, 3, 5, 4…
    ## $ competitive         <dbl+lbl> 5, 5, 5, 3, 4, 4, 3, 5, 5, 3, 3, 3, 4, 3, 3, 4…
    ## $ goodwriter          <dbl+lbl> 3, 4, 4, 4, 4, 4, 4, 5, 3, 2, 4, 4, 5, 5, 4, 5…
    ## $ public              <dbl> 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 0,…
    ## $ travelmonth         <dbl> 3, 30, 3, 6, 3, 6, 6, 30, 30, 3, 18, 6, 30, 30, 18…
    ## $ salary              <dbl> 162.5, 262.5, 137.5, 125.0, 212.5, 112.5, 100.0, 2…
    ## $ hours_desired       <dbl> 55.5, 45.5, 45.5, 45.5, 45.5, 40.0, 50.0, 55.5, 45…
    ## $ female              <dbl> 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…
    ## $ single              <dbl> 0, 1, 0, 0, 1, 0, 1, 0, NA, 0, 1, 0, 0, 0, 1, 0, 1…
    ## $ single_female       <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,…

**Key variables:**

| Variable | Description |
|----|----|
| `public` | Treatment indicator: 1 = answers visible to peers, 0 = private |
| `single_female` | 1 = unmarried female respondent |
| `salary` | Desired annual salary (thousands) |
| `travelmonth` | Desired travel days per month |
| `hours_desired` | Desired weekly hours of work |
| `tendtolead` | Self-reported leadership tendency (1–5 Likert scale) |
| `moreambitious` | Self-reported professional ambition (1–5 Likert scale) |
| `competitive` | Self-reported competitiveness (1–5 Likert scale) — placebo |
| `goodwriter` | Self-reported writing ability (1–5 Likert scale) — placebo |

------------------------------------------------------------------------

## Step 1: Replicate Panel A of Table 4

The paper estimates bivariate regressions of career preference outcomes
on the public treatment dummy, **restricted to single women only**. HC1
heteroskedasticity-robust standard errors are used throughout.

``` r
sf <- subset(acting_wife, single_female == 1)
```

``` r
# Convert Likert-scale outcomes to numeric for regression
sf <- sf %>%
  mutate(
    tendtolead_num     = as.numeric(tendtolead),
    moreambitious_num  = as.numeric(moreambitious),
    competitive_num    = as.numeric(competitive),
    goodwriter_num     = as.numeric(goodwriter)
  )
```

``` r
# Desired salary
m_salary <- lm(salary ~ public, data = sf)
coeftest(m_salary, vcov = vcovHC(m_salary, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 131.0484     6.9326 18.9032  < 2e-16 ***
    ## public      -18.1174     8.1669 -2.2184  0.03046 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Days per month of travel
m_travel <- lm(travelmonth ~ public, data = sf)
coeftest(m_travel, vcov = vcovHC(m_travel, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  13.5484     2.1693  6.2454 5.349e-08 ***
    ## public       -6.9277     2.3476 -2.9509  0.004565 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Desired weekly hours
m_hours <- lm(hours_desired ~ public, data = sf)
coeftest(m_hours, vcov = vcovHC(m_hours, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  52.2097     1.9053 27.4021  < 2e-16 ***
    ## public       -3.8882     2.1116 -1.8414  0.07077 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Self-reported leadership tendency
m_lead <- lm(tendtolead_num ~ public, data = sf)
coeftest(m_lead, vcov = vcovHC(m_lead, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.87097    0.13719 28.2163  < 2e-16 ***
    ## public      -0.38821    0.18737 -2.0719  0.04274 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Professional ambition
m_amb <- lm(moreambitious_num ~ public, data = sf)
coeftest(m_amb, vcov = vcovHC(m_amb, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  4.12903    0.15207 27.1514 < 2.2e-16 ***
    ## public      -0.74972    0.17755 -4.2225 8.624e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Competitiveness (placebo — should be near zero)
m_comp <- lm(competitive_num ~ public, data = sf)
coeftest(m_comp, vcov = vcovHC(m_comp, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.29032    0.15524 21.1953   <2e-16 ***
    ## public       0.12347    0.20638  0.5983    0.552    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Writing ability (placebo — should be near zero)
m_write <- lm(goodwriter_num ~ public, data = sf)
coeftest(m_write, vcov = vcovHC(m_write, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.83871    0.17421 22.0349   <2e-16 ***
    ## public       0.12681    0.23169  0.5473   0.5863    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Summary of replicated coefficients (single women, public treatment
effect):**

| Outcome                   | Coefficient | p-value  | Matches Paper?       |
|---------------------------|-------------|----------|----------------------|
| Desired salary            | −18.1       | 0.030    | ✓                    |
| Travel days/month         | −6.9        | 0.005    | ✓                    |
| Desired weekly hours      | −3.9        | 0.071    | ✓ (marginal)         |
| Leadership tendency       | −0.39       | 0.043    | ✓                    |
| Professional ambition     | −0.75       | \< 0.001 | ✓                    |
| Competitiveness (placebo) | +0.12       | 0.552    | ✓ (null as expected) |
| Writing ability (placebo) | +0.13       | 0.586    | ✓ (null as expected) |

The replication is successful. Single women in the public condition
reported substantially lower desired salaries, travel willingness,
leadership, and ambition. The placebo outcomes (competitiveness, writing
ability) are near zero and statistically insignificant, confirming the
treatment effects are not driven by general social desirability bias.

------------------------------------------------------------------------

## Step 2: Interaction Specification (Women Only)

To test whether the public treatment differentially affects single
vs. partnered women, we add an interaction term using the female-only
subsample.

``` r
women <- subset(acting_wife, female == 1) %>%
  mutate(single = ifelse(maritalstatus == 0, 1, 0))

# Salary
m_salary_int <- lm(salary ~ public * single, data = women)
coeftest(m_salary_int, vcov = vcovHC(m_salary_int, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   134.7222     6.3370 21.2597   <2e-16 ***
    ## public         -1.2222     7.7611 -0.1575   0.8752    
    ## single         -3.6738     9.3988 -0.3909   0.6967    
    ## public:single -16.8951    11.2737 -1.4986   0.1369    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Desired hours
m_hours_int <- lm(hours_desired ~ public * single, data = women)
coeftest(m_hours_int, vcov = vcovHC(m_hours_int, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   52.53704    1.60620 32.7088  < 2e-16 ***
    ## public        -4.05704    1.86812 -2.1717  0.03208 *  
    ## single        -0.32736    2.49362 -0.1313  0.89580    
    ## public:single  0.16879    2.82110  0.0598  0.95240    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Note on results:** The interaction term `public:single` is not
statistically significant in this subsample specification. This does not
necessarily contradict the paper — the authors use a different
comparison group (all respondents vs. single women only) and the
experiment may be underpowered for this particular specification. See
the full-sample interaction below.

------------------------------------------------------------------------

## Step 3: Last Row of Table 4 — Full Sample Interaction

The paper’s last row compares the public treatment effect for single
women against everyone else in the full sample.

``` r
# Desired hours: full sample interaction
m_hours_lastrow <- lm(hours_desired ~ public * single_female, data = acting_wife)
coeftest(m_hours_lastrow, vcov = vcovHC(m_hours_lastrow, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          51.68276    0.73858 69.9756  < 2e-16 ***
    ## public                1.17873    1.16791  1.0093  0.31355    
    ## single_female         0.52692    2.02311  0.2604  0.79467    
    ## public:single_female -5.06698    2.39191 -2.1184  0.03485 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Desired salary: full sample interaction
m_salary_lastrow <- lm(salary ~ public * single_female, data = acting_wife)
coeftest(m_salary_lastrow, vcov = vcovHC(m_salary_lastrow, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          142.0690     3.0477 46.6156   <2e-16 ***
    ## public                -3.6336     4.1016 -0.8859   0.3763    
    ## single_female        -11.0206     7.5021 -1.4690   0.1427    
    ## public:single_female -14.4838     9.0575 -1.5991   0.1107    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Results:**

- **Hours interaction** (`public:single_female`): coefficient = −5.07, p
  = 0.035. Single women differentially reduce desired hours when answers
  are public, consistent with the paper.
- **Salary interaction** (`public:single_female`): coefficient = −14.48,
  p = 0.111. The salary effect is in the expected direction but does not
  reach conventional significance in this replication.

------------------------------------------------------------------------

## Step 4: Sensitivity Analysis — Alternative Coding of “Single”

Following Gelman’s critique of researcher degrees of freedom, we test
whether results are sensitive to a reasonable alternative coding of
single status. Here, we include respondents in serious relationships
(marital status = 1) alongside unmarried respondents (marital status =
0).

``` r
acting_wife <- acting_wife %>%
  mutate(
    single_alt        = ifelse(maritalstatus %in% c(0, 1), 1, 0),
    single_female_alt = ifelse(female == 1 & single_alt == 1, 1, 0)
  )

sf_alt <- subset(acting_wife, single_female_alt == 1)

# Salary under alternative coding
m_salary_alt <- lm(salary ~ public, data = sf_alt)
coeftest(m_salary_alt, vcov = vcovHC(m_salary_alt, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 131.5341     5.3383 24.6397  < 2e-16 ***
    ## public      -14.6736     6.3978 -2.2935  0.02429 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Hours under alternative coding
m_hours_alt <- lm(hours_desired ~ public, data = sf_alt)
coeftest(m_hours_alt, vcov = vcovHC(m_hours_alt, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  51.7045     1.3983 36.9775  < 2e-16 ***
    ## public       -3.4903     1.5677 -2.2263  0.02867 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# Last-row interaction under alternative coding
m_hours_last_alt <- lm(hours_desired ~ public * single_female_alt, data = acting_wife)
coeftest(m_hours_last_alt, vcov = vcovHC(m_hours_last_alt, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              51.799242   0.799767 64.7679  < 2e-16 ***
    ## public                    1.564728   1.257810  1.2440  0.21433    
    ## single_female_alt        -0.094697   1.603478 -0.0591  0.95294    
    ## public:single_female_alt -5.054988   2.002544 -2.5243  0.01204 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
m_salary_last_alt <- lm(salary ~ public * single_female_alt, data = acting_wife)
coeftest(m_salary_last_alt, vcov = vcovHC(m_salary_last_alt, type = "HC1"))
```

    ## 
    ## t test of coefficients:
    ## 
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)              142.9924     3.2555 43.9230  < 2e-16 ***
    ## public                    -3.4554     4.3743 -0.7899  0.43011    
    ## single_female_alt        -11.4583     6.2257 -1.8405  0.06654 .  
    ## public:single_female_alt -11.2182     7.7190 -1.4533  0.14703    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

**Sensitivity conclusion:**

Key results remain directionally consistent and statistically
significant under the alternative coding, but some effect sizes shrink.
This illustrates Gelman’s point about researcher degrees of freedom:
small, defensible analytic choices can meaningfully affect which results
clear significance thresholds, even when the underlying patterns are
stable. Transparency about these choices is essential for credible
inference.

------------------------------------------------------------------------

## Packages Used

- `lmtest`: Coefficient testing via `coeftest()`
- `sandwich`: Heteroskedasticity-robust standard errors via `vcovHC()`
- `haven`: Import Stata `.dta` files
- `jtools`: Formatted regression tables
- `tidyverse`: Data manipulation and visualization
