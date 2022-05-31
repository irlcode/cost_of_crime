# Data for `Cost of Crime in Russia: compensating variation approach'

## Installation

To replicate the analysis you need to clone this repository to your local machine. Then you need to install the required versions of R dependencies listed in `DEPENDENCIES`. `code/helper_functions/install_dependencies.r` automates this step, but you may still need to install the underlying libraries manually with [Homebrew](https://brew.sh) or `apt-get`, depending on your platform. Finally, you need to declare the environment variable `CRIMECOST_PATH` in bash pointing to the repository. Or, better yet, you can add it in your `.Renviron` with
```console
user:~$ echo 'CRIMECOST_PATH="path_to_cloned_repository"' >> ~/.Renviron
```

The replication on a server-grade 16GB RAM with 2 vCPUs with Ubuntu 20.04 takes approx. 2 hours. Potential issues: you might need to install `cmake`. Also, in R >=4.2.0, stargazer <= 5.2.3 returns an error for objects with multiple models with long names. A quick fix for this is [here](https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53).

## Repository structure

```
/
├─data/ # Data used in this study
| ├ rcvs2021_crimecost.rdata # Russian Crime Victimization Survey - 2021 individual-level data
| ├ rcvs2021_crimecost_imputed.rdata # RCVS-21 data after multiple imputation
| ├ rcvs2021_raking_fit.rdata # RCVS-21 after raking weights
| ├ kouzh_2020_data.rdata # 2020 round of the Comprehensive Monitoring of Living Conditions household survey
| └ yearly_total_crime_cost.rds # Yearly figures of official crime and calculated total costs
|
├─code/
| └─helper_functions/
| | └ install_dependencies.r # Installs R dependencies used in the project 
| |
| ├ 1_rcvs_vs_domestic_household_survey.r # Compare RCVS-21 and KOUZH, compute raking weights
| ├ 2_estimate_regression_models.r # Fit all regression models
| ├ 3_multiple_imputation_regressions.r # Compute multiply imputed models
| └ 4_extract_official_crime_counts # Create figures with official crime counts
|
└─estimates/
  └ costofcrime_estimates_19aug21.csv # Paper estimates under different scenarios
```

We provide an annotated `Makefile` that documents the data analysis in our papers.

To build the paper run `make paper` when in the repository folder.

Please note that those commands will not necessarily produce any publication-ready output files (e.g. tables or figures). Our intention is to make the analysis pipeline transparent to the readers with the aid of `make`.


## Licence
<a rel="license" href="https://creativecommons.org/licenses/by-nc-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-sa/4.0/88x31.png" /></a><br />
Creative Commons License Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0).

Copyright © the respective contributors, as shown by the `AUTHORS` file.

## Contacts
Dmitriy Skougarevskiy, Ph.D.

dskougarevskiy@eu.spb.ru
