# Causal Inference & Policy Analysis in R

This repository contains several projects exploring how causal inference methods can be applied to real-world economic, behavioral, and policy questions using R.

Across these projects, I worked with both observational and experimental datasets while practicing data cleaning, statistical modeling, visualization, and causal reasoning. A major focus of the analyses was understanding how different research designs try to separate correlation from possible causal effects, while also showing how modeling choices and sample construction can change conclusions.

The goal of this repository was both to strengthen my technical skills in R and to better understand how researchers work through difficult questions using imperfect real-world data. These kinds of methods are also commonly used in healthcare, policy, and business settings when randomized experiments are difficult or unrealistic.

---

## Methods Covered

The projects in this repository include several commonly used causal inference approaches, including:

- Instrumental Variables (IV / 2SLS)
- Difference-in-Differences (DiD)
- Experimental OLS regression
- Interaction effects
- Sensitivity analysis
- Survey-weighted analysis

These methods are widely used in economics, healthcare, public policy, and social science research when analysts are trying to estimate the effects of interventions or behaviors using observational data.

---

# Projects

## 1. Instrumental Variables — Immigration and Native Wages

**File:** `lab_iv_instrumental_variables.Rmd`

### Research Question
How does immigration affect wages for native workers?

### Background
One of the biggest challenges in studying immigration is that immigrants do not move randomly. They are often drawn to areas with stronger labor markets and better economic opportunities, making it difficult to separate the effects of immigration itself from the conditions that already existed in those areas.

### Approach
This project uses an instrumental variables framework based on historical immigrant settlement patterns to estimate immigration effects while trying to reduce endogeneity bias.

The analysis compares standard OLS estimates with IV estimates and also evaluates instrument strength using common diagnostic tests.

### Key Findings
- The first-stage F-statistic was above the conventional weak instrument threshold
- Results showed noticeable differences between OLS and IV estimates
- The comparison helped illustrate how omitted variable bias and sorting effects can influence labor market analyses

### Skills & Methods
- OLS regression
- IV / 2SLS estimation
- Clustered standard errors
- Weak instrument testing
- Wu-Hausman testing

### Packages
`AER`, `lmtest`, `sandwich`, `haven`, `tidyverse`

---

## 2. Experimental OLS — Ambition and Social Signaling

**File:** `lab_acting_wife_replication.Rmd`

### Research Question
Do people change how they present career ambition when their responses are publicly observable?

### Background
This project replicates portions of the paper *Acting Wife* by Bursztyn, Fujiwara, and Pallais (2017), which studied how MBA students responded differently when career preferences were shared publicly versus privately.

Because treatment assignment was randomized, the study design helps reduce selection bias and makes it easier to interpret differences between groups.

### Approach
The analysis uses OLS regression with interaction terms and robust standard errors to examine differences across treatment conditions and demographic groups.

Additional sensitivity checks were included to look at how alternative coding decisions affected results.

### Key Findings
- Public observability influenced reported career preferences among some groups
- Several placebo outcomes showed minimal effects, helping evaluate whether changes reflected broader response bias
- Sensitivity analyses showed that some estimates changed depending on modeling assumptions and variable definitions

### Skills & Methods
- Experimental OLS regression
- Interaction terms
- HC1 robust standard errors
- Placebo testing
- Sensitivity analysis

### Packages
`lmtest`, `sandwich`, `haven`, `jtools`, `tidyverse`

---

## 3. Survey-Weighted Microdata Analysis

**File:** `lab_ipums_cps_analysis.Rmd`

### Research Question
How are health status, disability, and household characteristics associated with employment outcomes and household wellbeing?

### Background
This project uses IPUMS CPS ASEC microdata and focuses heavily on data preparation and survey methodology. Large survey datasets require careful handling of weights, universe restrictions, and labeled variables in order to produce interpretable estimates.

### Approach
The analysis uses survey-weighted grouped summaries and visualizations to examine relationships between health, labor force participation, working hours, and household characteristics.

### Key Findings
- Employment rates differed meaningfully across self-reported health categories
- Poor health was associated with lower average working hours
- Household-level patterns showed clustering across some demographic and health characteristics

### Skills & Methods
- Survey-weighted analysis
- Data cleaning and preprocessing
- Grouped summaries
- ggplot2 visualization
- Microdata handling

### Packages
`dplyr`, `haven`, `ipumsr`, `ggplot2`

---

## 4. Difference-in-Differences — The Mariel Boatlift

**File:** `lab_mariel_boatlift_did.Rmd`

### Research Question
Did the Mariel Boatlift affect wages for low-skilled native workers in Miami?

### Background
The Mariel Boatlift remains one of the most discussed case studies in labor economics because researchers have reached different conclusions depending on sample construction and modeling decisions.

This project explores some of those differences by replicating parts of the analysis using IPUMS CPS microdata.

### Approach
The project constructs samples from raw CPS data, applies inflation adjustments, and compares wage trends between Miami and comparison cities before and after the boatlift.

Additional analyses explore how demographic restrictions and sample definitions affect estimated results.

### Key Takeaways
- Results were sensitive to sample construction choices
- Small differences in demographic restrictions changed estimated effects noticeably
- The project highlighted why transparency and robustness checks matter in empirical research

### Skills & Methods
- Difference-in-Differences
- CPI adjustment
- Top-code correction
- Time-series comparison
- Sensitivity analysis

### Packages
`ipumsr`, `tidyverse`, `jtools`, `huxtable`

---

# Data Sources
The Acting Wife and Peri IV datasets are included in this repository. The Mariel Boatlift analysis uses a mock CPS dataset (cps_mock.csv) provided for demonstration. IPUMS CPS microdata requires a free account at cps.ipums.org

---

# How to Run

```r
install.packages(c(
  "AER", "lmtest", "sandwich", "haven", "ipumsr",
  "jtools", "huxtable", "tidyverse", "table1", "rddtools"
))
```

1. Clone this repository
2. Download the datasets listed above
3. Open any `.Rmd` file in RStudio
4. Knit the file to render the analysis

---

# Skills Demonstrated

Throughout these projects, I worked with:
- Instrumental Variables (IV / 2SLS)
- Difference-in-Differences
- Experimental regression analysis
- Robust standard errors and interaction effects
- Survey-weighted microdata
- Data cleaning and preprocessing in R
- ggplot2 visualizations
- R Markdown workflows and reproducible analysis
- CPI adjustment and variable construction
- Sensitivity analysis and model comparison

---

# Future Improvements

Some possible future additions include:
- Additional robustness checks
- Expanded visualizations
- Alternative model specifications
- Larger comparative policy analyses
- Interactive reporting outputs

---

## Author

**Olivia Bonnette**  
B.S. Biomedical Engineering & Business Management  
Case Western Reserve University (May 2026)

[LinkedIn](https://linkedin.com/in/oliviabonnette) | [GitHub](https://github.com/oliviabon77)
