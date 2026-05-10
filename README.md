
# Causal Inference & Econometrics — Applied Replication Studies

Replication studies of landmark economics papers using real government microdata. Each project applies a distinct causal inference method to answer a high-stakes policy question, builds the full analytical dataset from raw sources, and validates outputs against published benchmarks.

These are not tutorial exercises. The papers replicated here are among the most cited and contested in labor economics. Replicating them from scratch — constructing datasets from raw IPUMS CPS microdata, implementing top-code corrections, applying CPI deflation, running weak instrument diagnostics, and evaluating sensitivity to researcher analytic choices — is the kind of work that separates analysts who understand causality from those who run regressions.

---

## Why Causal Inference

Correlation is easy. Causality is hard. Most business and policy decisions require knowing whether X *caused* Y, not just whether they moved together. The methods in this repo are the standard toolkit for answering causal questions with observational data:

- **IV/2SLS** isolates exogenous variation to remove omitted variable bias
- **Difference-in-Differences** uses treatment and control groups across time to identify policy effects
- **Experimental OLS** with interaction terms identifies heterogeneous treatment effects in randomized settings
- **Sensitivity analysis** tests whether conclusions hold under defensible alternative assumptions

---

## Projects

### 1. Instrumental Variables — Does Immigration Raise or Lower Native Wages?
**File:** `lab_iv_instrumental_variables.Rmd`

**The question:** Does immigration increase economic output for native workers, or does labor market competition depress wages?

**The identification problem:** Immigrants do not move randomly. They concentrate in economically strong cities, which independently raises wages. A naive OLS regression conflates the causal effect of immigration with the underlying conditions that attracted immigrants in the first place.

**The solution:** The Card (2001) enclave instrument predicts immigrant inflows using historical settlement patterns interacted with national immigration growth by country of origin. This generates variation driven by historical geography and global supply shocks, plausibly independent of current local economic conditions.

**Key results:**
- First stage F-statistic = **43** — well above the conventional F > 10 threshold
- Wu-Hausman p-value = **0.0016** — OLS is statistically confirmed to be biased; IV is preferred
- IV estimate (0.01455) exceeds OLS (0.01085), suggesting OLS was *downward* biased — immigrants may also sort to economically weaker states, partially offsetting the upward endogeneity from strong-city sorting

**Methods:** OLS, IV/2SLS, clustered standard errors, weak instrument F-test, Wu-Hausman test
**Packages:** `AER`, `lmtest`, `sandwich`, `haven`, `tidyverse`

---

### 2. Experimental OLS — Do Women Hide Ambition in the Marriage Market?
**File:** `lab_acting_wife_replication.Rmd`

**The question:** Do single women strategically conceal career ambition when their preferences are observable to male peers?

**The paper:** Bursztyn, Fujiwara & Pallais (2017), *American Economic Review*. MBA students were randomly assigned to report career preferences either privately or publicly. The randomization makes the treatment exogenous — there is no selection bias.

**Key results (Panel A of Table 4 replicated):**
- Single women in the public condition reported **$18,100 lower** desired annual salaries (p = 0.030)
- **6.9 fewer** desired travel days per month (p = 0.005)
- **0.75 lower** self-reported professional ambition on a 1–5 scale (p < 0.001)
- Placebo outcomes (competitiveness, writing ability) are near zero and insignificant — confirming the effects are not driven by general social desirability bias
- Effects are specific to single women; partnered women and men show no significant changes

**Sensitivity analysis:** Following Gelman's critique of researcher degrees of freedom, an alternative coding of single status is tested. Key effects remain directionally consistent but some shrink — illustrating how defensible analytic choices can determine which results clear significance thresholds.

**Methods:** Bivariate OLS, HC1 robust standard errors, interaction terms, placebo testing, sensitivity analysis
**Packages:** `lmtest`, `sandwich`, `haven`, `jtools`, `tidyverse`

---

### 3. Survey-Weighted Microdata — Health, Employment, and Household Characteristics
**File:** `lab_ipums_cps_analysis.Rmd`

**The question:** How do health status, disability, and household characteristics relate to labor market outcomes and intergenerational wellbeing?

**The data challenge:** Working with IPUMS CPS ASEC microdata requires careful attention to survey weight application, universe restrictions, labeled variable handling, and the distinction between person-level and household-level weights. Failing to account for these produces meaningless population estimates.

**Key results:**
- Employment rates decline monotonically from **63.8%** (excellent health) to **11.5%** (poor health) — a 52-percentage-point gap
- Workers in poor health average **32 hours per week** versus 38–39 for excellent health workers, compounding the employment gap
- Among deaf respondents who are married, **96.8%** have a deaf spouse — striking evidence of assortative mating on disability status
- Children of mothers in excellent health are overwhelmingly rated excellent health themselves; the correlation is visually monotonic but partially attributable to reporting bias since the same respondent rates both

**Methods:** Survey-weighted means, grouped summarize, ggplot2 visualization, intergenerational correlation analysis
**Packages:** `dplyr`, `haven`, `ipumsr`, `ggplot2`

---

### 4. Difference-in-Differences — Did the Mariel Boatlift Depress Wages?
**File:** `lab_mariel_boatlift_did.Rmd`

**The question:** Did the sudden arrival of 125,000 Cuban immigrants in Miami in 1980 depress wages for low-skilled native workers?

**The controversy:** Borjas (2017) finds significant wage depression using a narrow sample of non-Hispanic men aged 25–59 without a high school diploma. Critics (Peri and Yasenov) argue the results disappear under alternative sample choices and may be driven by changes in Miami's racial workforce composition rather than the boatlift itself — a textbook omitted variable problem in a DiD design.

**What this replication does:**
- Constructs the Borjas sample from raw IPUMS CPS microdata (1976–1989) with full top-code correction and CPI deflation to 1980 dollars
- Replicates wage trend plots for Miami vs. comparison cities
- Tests the racial composition critique directly by plotting the white worker share in Miami vs. non-Miami over time
- Runs an alternative sample (ages 19–65, both sexes) to evaluate sensitivity to demographic restrictions
- Implements a formal DiD regression with pre/post and treatment/control indicators

**Key takeaway:** The Mariel Boatlift controversy is a master class in how sample construction and researcher degrees of freedom drive empirical results in contested policy debates. Small, defensible analytic choices determine whether the boatlift appears to have harmed native wages or had no detectable effect.

**Methods:** Difference-in-Differences, CPI deflation, top-code correction, raw IPUMS microdata construction, racial composition analysis, sensitivity analysis
**Packages:** `ipumsr`, `tidyverse`, `jtools`, `huxtable`

---

## Data Sources

All analyses use publicly available data:

| Dataset | Source | Access |
|---|---|---|
| IPUMS CPS ASEC (1976–1989) | [cps.ipums.org](https://cps.ipums.org/cps/) | Free account required |
| Peri state-panel IV data | Giovanni Peri, UC Davis | Available from author's faculty page |
| Acting Wife experimental data | Bursztyn, Fujiwara & Pallais (2017) | AER data repository |

Data files are not included in this repository. Download each dataset from the sources above and place files in your working directory before running.

---

## How to Run

```r
install.packages(c(
  "AER", "lmtest", "sandwich", "haven", "ipumsr",
  "jtools", "huxtable", "tidyverse", "table1", "rddtools"
))
```

1. Clone this repository
2. Download datasets from the sources above
3. Open any `.Rmd` file in RStudio
4. Click **Knit** to render as HTML

---

## Skills Demonstrated

| Category | Specifics |
|---|---|
| Causal inference | IV/2SLS, DiD, experimental OLS, interaction terms |
| Data construction | Raw IPUMS microdata, top-code correction, CPI deflation, variable derivation |
| Statistical rigor | Clustered SEs, HC1/HC3 robust SEs, weak instrument F-tests, Wu-Hausman test |
| Sensitivity analysis | Alternative coding, sample restriction sensitivity, researcher degrees of freedom |
| Visualization | ggplot2 with survey-weighted grouped summaries |
| Reproducibility | R Markdown, documented data sources, annotated code throughout |

---

## References

- Card, D. (2001). *Immigrant Inflows, Native Outflows, and the Local Labor Market Impacts of Higher Immigration.* Journal of Labor Economics.
- Borjas, G. (2017). *The Wage Impact of the Marielitos: A Reappraisal.* ILR Review.
- Bursztyn, L., Fujiwara, T., & Pallais, A. (2017). *Acting Wife: Marriage Market Incentives and Labor Market Investments.* American Economic Review.
- Peri, G. (2007). *Immigrants' Complementarities and Native Wages: Evidence from California.* NBER Working Paper.
- Gelman, A. & Loken, E. (2014). *The Statistical Crisis in Science.* American Scientist.

---

## Author

**Olivia Bonnette**
B.S. Biomedical Engineering & Business Management, Case Western Reserve University (May 2026)
[linkedin.com/in/oliviabonnette](https://linkedin.com/in/oliviabonnette) | [GitHub](https://github.com/oliviabon77)
