# Regional Well-Being in Europe: Statistical Analysis in R

Group project for the course *Laboratório de Estatística e Ciência de Dados*
(Statistics and Data Science Laboratory), Applied Mathematics and Computation (LMAC),
Instituto Superior Técnico, April 2025. The report is written in Portuguese.

The project has two parts: a statistical analysis of regional quality-of-life indicators
across Europe, and a study of two methods for generating random values from the Rayleigh
distribution. Everything is done in **R**.

## Part 1: Regional well-being in Europe

### Data

OECD regional data with 13 variables per region: education, employment, unemployment,
income, homicides, mortality, life expectancy, air pollution, voter turnout, internet
access, rooms per person, social support and life satisfaction.
[Add the data source link and the number of regions and countries.]

### Exploratory analysis

Custom R functions were written for the analysis:

- Summary statistics for all variables (`summary`).
- A chart per variable showing regional values, the mean of each country, the overall mean
  and the maximum and minimum regions.
- Tables of the 3 highest and 3 lowest regions for each variable, and outlier detection
  with `boxplot.stats`.
- Bar charts of the mean of each variable by European sub-region.
- A Pearson correlation matrix and a choropleth map of voter turnout in Europe.

Some of the correlations found: life expectancy and mortality (-0.99), employment and
unemployment (-0.72), income and life satisfaction (0.72), rooms per person and life
satisfaction (0.49), pollution and homicides (0.58).

### Hypothesis tests

Tests were chosen to fit the data: many variables are not normal (Shapiro-Wilk), and the
samples are not paired, so the chi-square test, the paired t-test and the paired Wilcoxon
test were ruled out.

| # | Question | Test | Result |
|---|----------|------|--------|
| 1 | Education and unemployment | Spearman correlation | ρ = -0.51, p = 5.66e-16, reject H0 |
| 2 | Income and life satisfaction (high vs low income) | Unpaired t-test, confirmed with Mann-Whitney-Wilcoxon | p < 2.2e-16, reject H0 |
| 3 | Air pollution and life expectancy (high vs low pollution) | Mann-Whitney-Wilcoxon | p = 0.059, reject H0 at α = 0.10 |
| 4 | Life satisfaction in Southern Europe vs other regions | Mann-Whitney-Wilcoxon | p = 0.9994, do not reject H0 |
| 5 | Is the third quartile of income below 20000? | Sign test | p = 0.998, do not reject H0 |
| 6 | Social support in Central, Southern and Western Europe | Kruskal-Wallis | p = 0.18, do not reject H0 at α = 0.10 |
| 7 | Education and voter turnout, per European sub-region | Spearman correlation | No strong association only in Scandinavia and Southeast Europe |

### Bootstrap confidence intervals

The percentile-t bootstrap was implemented from scratch (with a nested bootstrap to
estimate the standard error) and compared with the `boot` R package. It was applied to the
5% trimmed mean and to the median of **income** and **unemployment** (95% confidence,
B = 1000 resamples, b = 200 for the standard error; seeds 123 and 321).

| Variable | Statistic | `boot` package | Own implementation |
|----------|-----------|----------------|--------------------|
| Income | 5% trimmed mean | (17082, 18315) | θ₀ = 17692, (17037, 18290) |
| Income | Median | (16901, 18118) | θ₀ = 17787, (17114, 18101) |
| Unemployment | 5% trimmed mean | (7.50, 9.23) | θ₀ = 8.33, (7.55, 9.29) |
| Unemployment | Median | (6.16, 7.74) | θ₀ = 6.95, (6.33, 7.86) |

The two implementations give very similar intervals. For unemployment, the trimmed mean is
clearly above the median, which reflects regions with very high unemployment that still
affect the mean after trimming.

## Part 2: Random variate generation (Rayleigh distribution)

Two methods to generate values from the Rayleigh(β) distribution, derived and proved in the
report, compared with the `VGAM` function `rrayleigh`:

1. **Inverse transform:** `X = β·sqrt(-2·ln(1 - U))`, with `U ~ Uniform(0, 1)`.
2. **Box-Muller:** `X = β·sqrt(Z1² + Z2²)`, built from two independent standard normal
   values generated with Box-Muller.

Comparison with β = 2 and seed 123:

| | Mean | Median | Std. dev. | KS test p-value |
|---|------|--------|-----------|-----------------|
| Inverse transform | 2.49 | 2.32 | 1.30 | 0.989 |
| Box-Muller | 2.51 | 2.36 | 1.30 | 0.751 |
| VGAM | 2.50 | 2.34 | 1.34 | 0.42 |
| Theoretical | 2.51 | 2.35 | 1.31 | |

The study was repeated for β from 0.5 to 5 (step 0.1) and for sample sizes from 200 to
5000 (step 200), with two seeds. Main findings:

- The Kolmogorov-Smirnov p-value does not depend on β for a fixed seed, but it changes with
  the seed and with the sample size. The "best" method is therefore very volatile.
- Using `β·sqrt(-2·ln(U))` in the inverse transform method and the same seed gives the same
  samples as the other methods, because the three methods are essentially equivalent.

## Requirements

- R ([version])
- Packages: [e.g. `ggplot2`, `boot`, `VGAM`, `corrplot`, `sf`, `rnaturalearth`. Check the
  ones used in your scripts]

## How to run

1. Put the data file ([file name]) in `[folder]`.
2. Run `[script name]` for Part 1 and `[script name]` for Part 2.

## Repository structure

```
[file]     [what it contains]
[file]     [what it contains]
report/    Project report (PDF, in Portuguese)
```
Catarina Andrade, Guilherme Barrela, Leonor Castelo, Martim Pinto.

[Optional: one line on your own contribution.]
