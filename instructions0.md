## Overview

This app supports the design and analysis of **diagnostic accuracy studies** with a 
Bayesian adaptive design.

**Study aim:** to estimate an accuracy measure (sensitivity and/or specificity) 
to a desired degree of precision. The study is deemed successful if the width of 
the central posterior credible interval(s) for the specified measure(s) is below a 
pre-specified target value.

Users can set up a study, calculate suitable sample sizes, and perform interim and 
final analyses on observed data. 
Sample sizes are based on *assurance* -- the prior probability that the study will be successful.

At the interim analyses, the Bayesian design allows 
- sample size re-estimation 
- early termination for efficacy
- early termination for futility.

The methods used in this app were developed in Binks et al.[^1] and are implemented
in the `safegen` R package.

[^1]: Include full reference when available.
