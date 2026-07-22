# Reproducible Research: Peer Assessment 1

This repository contains the complete analysis of personal activity-monitoring data recorded at five-minute intervals during October and November 2012.

## Submission files

- `PA1_template.Rmd` — source R Markdown document containing all analysis code and narrative.
- `PA1_template.md` — knitted Markdown report.
- `PA1_template.html` — knitted HTML report.
- `figure/` — figures generated while knitting the report.
- `activity.csv` — supplied source dataset.

## Reproducing the report

Open R in the repository root and run:

```r
knitr::knit2html("PA1_template.Rmd")
```

Every executable code chunk uses `echo = TRUE`. The analysis uses base R for data processing, imputation, summaries, and plots.

## Principal findings

- Mean observed daily total: 10,766.19 steps.
- Median observed daily total: 10,765 steps.
- Peak average five-minute interval: 835.
- Missing step observations: 2,304.
- After interval-mean imputation, both the mean and median daily totals are 10,766.19 steps.
- Weekday activity has a sharper morning peak, while weekend activity is distributed more broadly through the day.
