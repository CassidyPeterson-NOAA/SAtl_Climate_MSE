# Climate-readiness of fisheries management procedures with application to the southeast U.S. Atlantic

## Purpose
This [repository](https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE) contains scripts used to test various model-based and empirical management procedures (MPs) across nonstationary projections designed to mimic potential climate change impacts. 

> Peterson et al. (in prep) Climate-readiness of fisheries management procedures with application to the southeast U.S. Atlantic. 

## Overview
* This analysis used a desk MSE to explore the performance of various MPs with nonstationary future projections, using three commercially and recreationally important fish stocks in the southeast U.S. Atlantic as a case study.
* MPs tested include traditional model-based (statistical-catch-at-age stock assessment) and less conventional [empirical (non-model, indicator-based) MPs](https://dlmtool.github.io/DLMtool/reference/index.html). 
* Nonstationary OM scenarios included: 
  * Recdev_hi & Recdev_lo - regime shift in recruitment deviations 
  * Age0M_hi & Age0M_lo - regime shift in young-of-year mortality-based 
  * EpiM - episodic natural mortality
  * Recns - nonstationary recruitment deviations
  * Uobs_hi & Uobs_lo - regime shift in survey index variability
  * Refbias_hi & Refbias_lo - biases in perceived reference points
* Performance metrics included the ability to maintain a healthy stock biomass, avoid overfishing, high catches, and stability in fishery catches. 

## Take-homes
* MP performance was species-specific.
* For RP and VS, empirical MPs were better able to adapt to nonstationarity compared to traditional SCA-based MPs. 
* Empirical MP performance was very similar to that of model-based MPs for BSB. 
* Empirical MPs may serve as a useful fishery management tool in the face of nonstationary future dynamics, and should be included in the fisheries managers' toolbox for continued use and exploration. 
* Future research should focus on defining the ideal tuning strategy for empirical MPs. 

## Software and Packages
This project uses [openMSE](https://openmse.com/) software (developed by [Blue Matter Science](https://www.bluematterscience.com/)) and builds on extensive foundational work led by Nikolai Klibansky (see [bamExtras](https://github.com/nikolaifish/bamExtras), [bamMSE](https://github.com/nikolaifish/bamMSE), and [SEFSCInterimAnalysis](https://github.com/nikolaifish/SEFSCInterimAnalysis)).

## Results
[See All Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/SAtl_ClimateMSE_Results.html)

[See VS Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/VS_SAtl_Climate_MSE_Results.html)

[See BSB Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/BSB_SAtl_Climate_MSE_Results.html)

[See RP Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/RP_SAtl_Climate_MSE_Results.html)


[See All Results with Implementation Overages Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/SAtl_ClimateMSE_Results_Overages.html)

[See VS_Overages Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/VS_O_SAtl_Climate_MSE_Results.html)

[See BSB_Overages Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/BSB_O_SAtl_Climate_MSE_Results.html)

[See RP_Overages Results Here](https://htmlpreview.github.io/?https://github.com/CassidyPeterson-NOAA/SAtl_Climate_MSE/blob/main/RP_O_SAtl_Climate_MSE_Results.html)



## Disclaimer
"This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an 'as is' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government."
