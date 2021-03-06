---
title: "Chlorpyrifos Meta Analysis"
author: "Yoni Ackerman"
output: html_document
---

The Data Set
--------------------------------------------------------------------------------

Data was derived from 159 unique studies.
Each study was comprised of at least one experiment in which some number of
chlorpyrifos formulations were tested against one another and / or some other
active ingredient / formulation. Of a total of 343 experiments, 265 proved
unique and appropriate to our analysis. The criterion by which we selected
experiments were:

* at least 1 chlorpyrifos treatment was tested
* at least 1 alternative treatment was tested
* an untreated control was used
* the reported response had to be at least two numeric measurements of insect
density, sampled on different days
* to maintain stability of sample-variance estimates (equations to follow), we
  retained studies with at least four non-control treatments.

These 265 experiments yielded 2569 data points, each describing the change in
insect density after the application of their respective treatments. In cases
where multiple applications were made before measurements were taken, we summed
the amounts to find a total mass of AI applied. In the case that multiple AI's
were applied, either by using multiple products or a single product containing
multiple AI's, we gave each AI a row in the data set and connected it to all
other other AI's used in the same sample via a unique multi-product key. From
here, we went on to calculate the test statistic for each sample. This first
involved calculating the area below the insect density curve:

$$ \bar{y}_{i}^{T} = \sum_{k = 1}^{N_i}\frac{\text{meas}_{Tk} +
\text{meas}_{T(k+1)}}{2(\text{jd}_{T(k+1)} - \text{jd}_{Tk})} $$

Where $T$ is the index of the treatment in experiment $i$, $N_i$ is the number
of different measurements made in the experiment, $\text{meas}_{Tk}$ is the
$k$th insect density
measurment of that treatment and $\text{jd}_{Tk}$ is the julian date of that
measurement. We then followed th work of Belova et. al. to calculate the effect
size as the standard mean difference from the control by:

$$ d_{i}^{T} = a(n_{i}^{C})\frac{(\bar{y}_{i}^{T} - \bar{y}_{i}^{C})}{\sigma_{i}^{C}} $$

Where $\bar{y}_{i}^{C}$ is the insect day value of the untreated control from the
experiment $i$, $\sigma_{i}^{C}$ is the standard deviation of the control group,
and $a(n_{i}^{C}) is a constant that depends on the sample size within each
experiment. 

In the data collection phase of this project, we did not filter out studies or
experiments based on insect type. We could not assume, however, that experiments
done on different insect species were comparable.
Thus, we had to group our analysis by insect species. However not all species for
which we had data had enough samples for use to do a solid analysis. We
chose to use a ten data-point minimum as our threshold. Before we could begin
our analysis, however, we had one more structural problem to solve. Our analysis
was intended to gauge the efficacy of alternative AI's against Chlorpyrifos.
Every treatment in our data set, however, was not merely definied by the AI
used, but also by the specific formulation of that AI. By looking at the dose-
response curves of the most populous insect-specie/AI sample groups, we concluded
that the effect of increasing dosage on $d_{i}^{T}$ was weak enough that we could
proceed with our analysis without taking it into accound.

Another method to address this problem is layed out in
Belova et al 2013. With this method, each AI is divided into three categories
based on msds recommendations: those applied below the recommended range of
usage (labelled $l$); those within the
recommended range of usage (labelled $s$); and those above the recommended range
($h$). Each category within a single AI group is then treated as a distinct
treatment type. Thus instead of treaments like .01 lbs Chlorpyrifos
used, 2 lbs Chlorpyrifos used, .15 lbs Chlorpyrifos used, etc. One would have only
Chlorpyrifos-l, Chlorpyrifos-s, and Chlorpyrifos-h. It is our goal to apply
these methods thoroughly in the future. We plan to impliment this method in the
future, once satisfactory usage ranges for all pests, crops, and products are
obtained.

Standard Error of Measurements
--------------------------------------------------------------------------------
Calculating the SEM for our data was necessary as numerical measure of error were
not always reported. Instead, many studies employed statistical
groups as a way of representing the statistical difference between measurements.
Using methods from Knapp et. el 2009, we were able to extrapolate upper and
lower bounds on the SEM for the samples of such studies. Furthermore, for studies
for which no error measurement of any kind was reported, we were able to impute
these data using the imputation methods described in Belova et. al. We then went
on to calculate the variance of $d_{i}^{T}$, for which $\sigma{i}^{T}$ is an
essential piece, also according to the equations given in Belova et. al. for both
given and imputed data. These are given as:

$$\text{Var}(d_{i}^{T}) = \big \lbrack a(n_{i}^{C}) \big \rbrack^{2}
\Big (\frac{n_{i}^{C} - 1}{n_{i}^{C} - 3} \Big ) \Big ( \frac{n_{i}^{T} + n_{i}^{C}}{n_{i}^{T} n_{i}^{C}} \Big ) +
(d_{i}^{T})^{2} \Big ( \frac{n_{i}^{C} - 1}{n_{i}^{C} - 3}
\big \lbrack a(n_{i}^{C}) \big \rbrack^{2} - 1 \Big ) $$

$$\text{Var}_{\text{imp}}(d_{i}^{T}) = \big \lbrack a(n_{i}^{C}) \big \rbrack^{2}
\frac{2}{N_{j}}\frac{K_{j} - 1}{K_{j} - 3} + (d_{i}^{T})^{2} \Big ( \frac{K_{j} - 1}{K_{j} - 3} \big \lbrack a(K_{j}) \big \rbrack^{2} - 1 \Big) $$

The Analysis
--------------------------------------------------------------------------------
We made use of two statistical methods in our analysis: Bayesian Monte Carlo
Markov Chain (MCMC) and Linear Mixed Effect Model (LMER) regressions. Both
methods were used to test the following model:

$$ \text{d}_{i}^{T} \sim \text{Normal}(\mu_{i}^{T}, \sigma_{T}) $$

$$ \mu_{i}^{T} = \alpha + \alpha_{i} + \beta\text{x}_{i}^{T} $$

$$ \alpha_{i} \sim \text{Normal}(0, \sigma_{i}) $$

Where $\text{d}_{i}^{T}$ is the test statistic of treatment $T$ of experiment $i$
and $\text{x}_{i}^{T}$ is the categorical variable describing the AI used in
treatment $T$ of experiment $i$. And where $\alpha_{i}$ is a sample drawn from the
estimate of the distribution of the random effect of experiment $i$. This random
effect is a standard way of attempting to statistically account
for the variety of ways studies used in a meta analysis might differ (such as
location, weather patterns, research habits, etc.).

The Results
--------------------------------------------------------------------------------
The following plot displays the effect size and 95% interval of each treatment
with both methods overlayed:

```{r, echo = FALSE, fig.width = 16, fig.height = 12, eval = TRUE}
## do.call(grid.arrange, grobs_weighted)
```

In the following tables, we represent both a numerical version of the above, as
well as the 95% confidence intervals of the difference between the effect size
of $chlorpyrifos$ and 95% confidence interval bounds of the alternative
treatment. If this interval includes or sits above zero, then we cannot reject
the hypothesis that the alternative and $chlorpyrifos$ are statistically the
same at the 5% significance level.

### LMER

### MCMC
