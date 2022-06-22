# Alias for running docekr image
RUN = docker run --rm -v $(PWD):/cost_of_crime:Z -u=$(shell id -u) cost_of_crime_image Rscript

# List of R dependencies for the project
DEPENDENCIES:
	# Do nothing, the file is created outside the repo
	noop

# Data from the 2020 round of the Comprehensive Monitoring of Living Conditions household survey
data/kouzh_2020_data.rdata:
	# Do nothing, the file is created outside the repo
	noop
	
# RCVS-2021 data
data/rcvs2021_crimecost.rdata:
	# Do nothing, the file is created outside the repo
	noop

# Install dependencies
code/helper_functions/install_dependencies.r:
	# Do nothing, this code runs at docker build
	noop

# Compute raking weights
data/rcvs2021_raking_fit.rdata: data/rcvs2021_crimecost.rdata
	$(RUN) code/1_rcvs_vs_domestic_household_survey.r

# Estimate summary statistics
tables/survey_summary_stat.tex: data/rcvs2021_crimecost.rdata
	$(RUN) code/1_rcvs_vs_domestic_household_survey.r
	
# Fit the regression models
estimates/costofcrime_estimates_19aug21.csv: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r

# Perform multiple imputation
data/rcvs2021_crimecost_imputed.rdata: data/rcvs2021_crimecost.rdata
	$(RUN) code/3_multiple_imputation_regressions.r
	
# Create a chart with official crime statistics and estimates
media: data/rcvs2021_crimecost.rdata
	$(RUN) code/4_extract_official_crime_counts.r

# Paper
paper: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata \
	estimates/costofcrime_estimates_19aug21.csv data/rcvs2021_crimecost_imputed.rdata media
