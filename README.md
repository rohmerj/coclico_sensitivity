# coclico_sensitivity

You will find the R scripts for running the global sensitivity analysis (GSA) for the damage cost of future coastal flooding  based on the [coclico](https://coclicoservices.eu/) dataset. Details are available in [Rohmer et al., 2026, submitted].

**How to run**
- unzip the file
- make sure to have the R package [sensitivity](https://cran.r-project.org/web/packages/sensitivity/index.html) installed
- for plotting, make sure to have the R package [ggplot](https://ggplot2.tidyverse.org/) and [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html) installed
- run the R script *run_GSA.R*
- The results can be processed and plotted by running the R script *run_plt.R*

**Files**
- ./data/*.csv: the results of the uncertainty propagation for the five countries considered (France FR, Spain ES, the UK, Italy IT, Finland FI)
- ./data/population.RData: provides the population density across the LAUs of the five countries considered
- ./resu/All: results for the GSA by considering all factors
- ./resu/HE: results for the GSA by considering the high-end scenario
- ./resu/LD: results for the GSA by considering the low defence scenario
- ./resu/sspFixed_LD: results for the GSA by considering the low defence scenario and a fixed SSP scenario
- ./resu/test_proba: results for the GSA by varying the assumptions on the probability weight depending on the population density
 
