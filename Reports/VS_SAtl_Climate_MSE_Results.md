Climate-readiness of empirical vs. model-based MPs in the US South
Atlantic - Vermilion Snapper
================
Cassidy Peterson
August 2024

# Background

Supplement to Peterson CD. Klibansky N. Vincent MT. Walter III JF.
Climate-readiness of fishery management procedures with application to
the southeast U.S. Atlantic.

This report contains supplementary materials from a South Atlantic MSE
exercise testing the behavior of model-based versus empirical MPs for
black sea bass (BSB), vermilion snapper (VS), and red porgy (RP) using
the openMSE software ([Blue Matter Science](bluematterscience.com)).

This section is focused on results for vermilion snapper.

## Abbreviations

- MSE abbreviations
  - MSE - management strategy evaluation
  - OM - operating model
  - MP - management procedure
  - PM - performance metrics
- Case study species
  - VS - vermilion snapper
  - BSB - black sea bass
  - RP- red porgy
- Management procedures (MP). Refer to [openMSE](openmse.com) and
  [dlmtool](https://dlmtool.openmse.com/reference/index.html)
  documentation for further details on each MP. Note that all non-SCA
  MPs are empirical (indicator-based) MPs
  - ZeroC - zero catch MP, used for demonstration purposes only.
  - SCA*x* - statistical catch at age model that occurs every *x* years;
    may also be specified by a c or p (e.g., SCA5_c) to indicate that
    catches between assessments are constant (c) or specified by
    assessment forecast / projections
  - GBtarg - Gt - Geromont and Butterworth target CPUE and catch MP
  - ICI - index confidence interval MP
  - Irat - Ir - Iratio - mean index ratio
  - IT10 - I10 - iterative index target MP
  - Itarg - It - Itarget - incremental index target MP
  - GBslope - Gs - Geromont and Butterworth index slope MP
  - Islope - Is - index slope tracking MP
- Operating model (OM) scenarios
  - Base - base-case OM with no nonstationarity
  - recdev_hi - Mean recruitment deviations in normal space was linearly
    adjusted to a value twice the size of the historical value from
    years 11-20 and remained at the shifted value until projection year
    50
  - recdev_lo - Mean recruitment deviations in normal space was linearly
    adjusted to a value half the size of the historical value from years
    11-20 and remained at the shifted value until projection year 50
  - age0M_hi - A shift in age-0 natural mortality simulated by linearly
    increasing age-0 M from the historical value to twice the historical
    value from years 11-20 and remaining at the higher value until
    projection year 50
  - age0M_lo - A shift in age-0 natural mortality simulated by linearly
    decreasing age-0 M from the historical value to half the historical
    value from years 11-20 and remaining at the lower value until
    projection year 50
  - recns - Base case recruitment deviations are multiplied by a random
    walk vector with a mean of 1 and standard deviation of 5 (using
    random_walk function in
    [bamExtras](https://github.com/nikolaifish/bamExtras) version 0.0.1
    R package)
  - epiM - A multiplier greater than 1 is applied to historical natural
    mortality (M) in 10% of years; multiplier was defined as
    $1+lognormal(\mu = 0$,$\sigma = 0.2$) with a maximum value of 3 for
    red porgy or 4 for black sea bass and vermilion snapper
  - uobs_hi - The coefficient of variation (CV) of the survey index was
    linearly increased from the value from the last year of the
    historical period to twice that level over years 11-20 and remaining
    at the higher value until projection year 50
  - uobs_lo - The CV of the survey index was linearly decreased from the
    value from the last year of the historical period to half that level
    over years 11-20 and remaining at the higher value until projection
    year 50
- Performance metrics (PMs)
  - $dSSB_{0}$ - dynamic spawning stock biomass that would occur under
    no fishing pressure
  - $F_{MSY}$ - the rate of fishing mortality ($F$) that would produce
    MSY
  - $SSB_{MSY}$ - SSB that would produce MSY if fished at $F_{MSY}$
  - $SSB_{y}$ - SSB in the year $y$
  - SSBratio - The ratio of SSB to dynamic SSB that would be achieved in
    the absence of fishing ($dSSB_{0}$) calculated annually
  - t10SSBratio - Mean terminal 10 years of SSBratio; SSBratio
    calculated annually over the final 10 years of the projection period
    and averaged across years 40-50
  - Fratio - The ratio of realized fishing mortality ($F$) to the rate
    that would produce MSY ($F_{MSY}$) calculated annually
  - PNOF - Probability of not overfishing, calculated as the proportion
    of years that $F/F_{MSY}$ exceeds 1 for the 50-year projection
    period of each projection iteration
  - P100 - Proportion of years in the final 10 years of the simulation
    period (years 40-50) in which SSB\>SSBMSY for each projection
    iteration
  - cyield - Cumulative relative yield, where relative yield is
    calculated as realized yield relative to OM-defined MSY (i.e., if
    yield = MSY, then relative yield = 1 for that year) summed across
    all 50 projected years
  - AAVY - Annual average variability in yield, calculated as the
    average change in yield from year-to-year across all years of the
    projection period:
    $AAVY= \Sigma_{t=0}^{t=50} sqrt( (((yield_{t}-yield_{t-1}) / (yield_{t-1} ))^{2}) )$

# Vermilion Snapper Results

## Median Trajectory Plots

Below plots demonstrate median trajectories across 250 iterations per OM
and MP scenario. Note y-axis limits.

### Median SSBratio trajectory

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-1-1.png"
alt="Figure 1. Median SSBratio (SSB / dSSB_{0}) trajectories for each MP in each OM." />
<figcaption aria-hidden="true">Figure 1. Median SSBratio (<span
class="math inline"><em>S</em><em>S</em><em>B</em>/<em>d</em><em>S</em><em>S</em><em>B</em><sub>0</sub></span>)
trajectories for each MP in each OM.</figcaption>
</figure>

### Median $SSB / SSB_{MSY}$ trajectory

Note that changes to recruitment deviations are not reflected in
MSY-based reference points.

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-2-1.png"
alt="Figure 2. Median SSB / static SSB_{MSY} trajectories for each MP in each OM." />
<figcaption aria-hidden="true">Figure 2. Median <span
class="math inline"><em>S</em><em>S</em><em>B</em>/<em>s</em><em>t</em><em>a</em><em>t</em><em>i</em><em>c</em><em>S</em><em>S</em><em>B</em><sub><em>M</em><em>S</em><em>Y</em></sub></span>
trajectories for each MP in each OM.</figcaption>
</figure>

### Median catch trajectory

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-3-1.png"
alt="Figure 3. Median catch trajectories for each MP in each OM." />
<figcaption aria-hidden="true">Figure 3. Median catch trajectories for
each MP in each OM.</figcaption>
</figure>

### Median SSBratio trajectory with 90% CIs

![Figure 4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP
with 90% confidence intervals. Each MP is plotted in a single coordinate
for each OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-1.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-2.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-3.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-4.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-5.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-6.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-7.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-8.png)![Figure
4. Median SSBratio ($SSB / dSSB_{0}$) trajectories for each MP with 90%
confidence intervals. Each MP is plotted in a single coordinate for each
OM scenario for
simplicity.](VS_SAtl_Climate_MSE_Results_files/figure-gfm/unnamed-chunk-4-9.png)

## Violin plots

Below plots demonstrate distribution of performance metrics across 250
iterations per OM and MP scenario. Note y-axis limits.

### Biomass

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-biomass1-PM-1.png"
alt="Figure 5. Relative SSB/SSB_{MSY} at year 30 by MP for each OM." />
<figcaption aria-hidden="true">Figure 5. Relative <span
class="math inline"><em>S</em><em>S</em><em>B</em>/<em>S</em><em>S</em><em>B</em><sub><em>M</em><em>S</em><em>Y</em></sub></span>
at year 30 by MP for each OM.</figcaption>
</figure>

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-biomass2-PM-1.png"
alt="Figure 6. Relative SSBratio (SSB / dSSB_{0}) at year 30 (reldSSB030) by MP for each OM. Horizontal reference line is indicative of SSB_{MSY}/SSB_{0} calculated at the OM conditioning step; note that this reference value may change over the projection period as population dynamics change, which is not considered for this illustration." />
<figcaption aria-hidden="true">Figure 6. Relative SSBratio (<span
class="math inline"><em>S</em><em>S</em><em>B</em>/<em>d</em><em>S</em><em>S</em><em>B</em><sub>0</sub></span>)
at year 30 (reldSSB030) by MP for each OM. Horizontal reference line is
indicative of <span
class="math inline"><em>S</em><em>S</em><em>B</em><sub><em>M</em><em>S</em><em>Y</em></sub>/<em>S</em><em>S</em><em>B</em><sub>0</sub></span>
calculated at the OM conditioning step; note that this reference value
may change over the projection period as population dynamics change,
which is not considered for this illustration.</figcaption>
</figure>

### Fishing mortality

<figure>
<img src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-F1-PM-1.png"
alt="Figure 7. Relative F/F{MSY} at year 30 by MP for each OM." />
<figcaption aria-hidden="true">Figure 7. Relative <span
class="math inline"><em>F</em>/<em>F</em><em>M</em><em>S</em><em>Y</em></span>
at year 30 by MP for each OM.</figcaption>
</figure>

<figure>
<img src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-F2-PM-1.png"
alt="Figure 8. Probability of not overfishing (PNOF) across the 50-year projection period by MP for each OM." />
<figcaption aria-hidden="true">Figure 8. Probability of not overfishing
(PNOF) across the 50-year projection period by MP for each
OM.</figcaption>
</figure>

### Yield

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-Yield1-PM-1.png"
alt="Figure 9. Relative yield (yield / MSY) at projection year 30 by MP for each OM." />
<figcaption aria-hidden="true">Figure 9. Relative yield (yield / MSY) at
projection year 30 by MP for each OM.</figcaption>
</figure>

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-Yield2-PM-1.png"
alt="Figure 10. Cumulative relative yield (yield / MSY) at projection year 30 by MP for each OM." />
<figcaption aria-hidden="true">Figure 10. Cumulative relative yield
(yield / MSY) at projection year 30 by MP for each OM.</figcaption>
</figure>

## Cumulative Violin Plots (Across all OMs)

Below plots demonstrate distribution of performance metrics across all
iterations, OMs, and MPs. Note y-axis limits.

<figure>
<img
src="VS_SAtl_Climate_MSE_Results_files/figure-gfm/VS-cumulative-PM-plot-1.png"
alt="Figure 11. MP performance across OMs" />
<figcaption aria-hidden="true">Figure 11. MP performance across
OMs</figcaption>
</figure>
