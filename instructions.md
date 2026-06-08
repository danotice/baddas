## Workflow

Parameters must be saved before using any other tab. 
After saving, the remaining tabs can be used in any order depending on the stage of the study.


### Parameters

Set your study design criteria and prior parameters, then click **Save Parameters**.

- **Accuracy measure** — parameter of interest: sensitivity, specificity, or both
- **Target width** — the maximum acceptable credible interval width for the study to be a success
- **Alpha** — used to define the central two-sided $100(1-\alpha)\%$ credible interval
- **Sample size range** — the minimum and maximum number of participants to recruit
- **Target assurance** — the desired probability of achieving the target width
- **Futility assurance** — if assurance falls below this threshold at an interim, stop for futility
- **Number of interims** — how many interim analyses are planned
- **Priors** — beta shape hyperparameters for sensitivity ($a_\lambda,b_\lambda$), 
specificity ($a_\theta,b_\theta$), and prevalence ($a_\rho,b_\rho$)

*Updating parameters will reset any analyses previously run.*

### Start of Study

The sample size can be calculated using assurance or specified manually.
Click **Start of Study Summary** to see the planned sample size and the timing of the first interim analysis.

### Interim Analyses
Enter the observed counts from the $2\times2$ contingency table for each interim analysis.

For analyses after the first interim, the counts are cumulative (includes all of 
the participants since the beginning of the study). 
The data can either be entered altogether or as two sets of entries -- new 
participants since the previous analysis and the cumulative counts before this analysis.

Click **Run Interim** to see whether to stop early for efficacy or futility, or continue with
a re-estimated sample size.

The **Final Analysis** tab runs a formal end-of-study success check once recruitment is complete.

### Check Study
Run a one-off check of study success at any point using observed data.

Optionally check for futility, which tells whether it is possible for the study to be 
successful if more patients are recruited but no more than the maximum sample size.

