## Overview

This app supports the design and analysis of **diagnostic accuracy studies** with a 
Bayesian adaptive design.

**Study aim:** to estimate an accuracy measure (sensitivity and/or specificity) 
to a desired degree of precision. The study is deemed successful if the width of 
the central posterior credible interval for the specified measure(s) is below a 
pre-specified target value.

Users can set up a study, calculate suitable sample sizes, and perform interim and 
final analyses on observed data. 
Sample sizes are based on *assurance* -- the prior probability that the study will be successful.

At the interim analyses, the Bayesian design allows 
- sample size re-estimation 
- early termination for efficacy or 
- early termination for futility.

The methods used in this app were developed in (Rachel Paper) and are implemented
in the `safegen` R package.


---

## Workflow

Parameters must be saved before using any other tab. 
After saving, the remaining tabs can be used in any order depending on the stage of the study.


### Parameters

Set your study design and prior parameters, then click **Save Parameters**.

- **Target width** — the maximum acceptable credible interval width for the study to be a success
- **Alpha** — used to define the credible interval level (e.g. alpha = 0.05 gives a 95% CI)
- **Sample size range** — the minimum and maximum number of participants to recruit
- **Target assurance** — the desired probability of achieving the target width
- **Futility assurance** — if assurance falls below this threshold at an interim, stop for futility
- **Number of interims** — how many interim analyses are planned
- **Accuracy measure** — parameter of interest: sensitivity, specificity, or both
- **Priors** — beta hyperparameters (a, b) for sensitivity, specificity, and prevalence

*Updating parameters will reset any analyses previously run.*

*When the accuracy measure is **both** the calculations are notably slower to complete.*


### Start of Study

Either calculate the planned sample size from your parameters or specify it manually.
Click **Start of Study Summary** to see the planned sample size and the timing of the first interim analysis.

### Interim Analyses
Enter observed counts from the 2x2 contingency table for each interim analysis.
For analyses after the first interim, specify whether the data entered contains all patients or only new patients since the previous interim.
Click **Run Interim** to see whether to stop early for efficacy or futility, or continue with
a re-estimated sample size.

The **Final Analysis** tab runs a formal end-of-study success check once recruitment is complete.

### Check Study
Run a one-off check of study success at any point using observed data.

Optionally check for futility, which tells whether it is possible for the study to be 
successful if more patients are recruited but no more than the maximum sample size.

