# List of R dependencies for the project
DEPENDENCIES:
	# Do nothing, the file is created outside the repo
	noop

# Helper function to install the dependencies
code/helper_functions/install_dependencies.r:
	# Do nothing, the file is created outside the repo
	noop

# GDP deflator data
data/gdp_deflator.csv:
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
install_dependencies:
	Rscript code/helper_functions/install_dependencies.r 
	
# Compute raking weights
data/rcvs2021_raking_fit.rdata: data/rcvs2021_crimecost.rdata
	Rscript code/1_rcvs_vs_domestic_household_survey.r

# Estimate summary statistics
paper: data/rcvs2021_crimecost.rdata
	Rscript code/1_rcvs_vs_domestic_household_survey.r
	
# Fit the regression models
costofcrime_estimates_19aug21.csv: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	Rscript code/2_estimate_regression_models.r

# Perform multiple imputation
data/rcvs2021_crimecost_imputed.rdata: data/rcvs2021_crimecost.rdata
	Rscript code/3_multiple_imputation_regressions.r

# Fit the regression models with multiple imputation
paper: data/rcvs2021_crimecost.rdata data/rcvs2021_crimecost_imputed.rdata
	Rscript code/3_multiple_imputation_regressions.r

# Create a chart with official crime statistics and estimates
paper: data/rcvs2021_crimecost.rdata
	Rscript code/4_extract_official_crime_counts.r

# Paper
paper: install_dependencies data/rcvs2021_crimecost.rdata costofcrime_estimates_19aug21.csv DEPENDENCIES
	Rscript code/helper_functions/install_dependencies.r 
	Rscript code/1_rcvs_vs_domestic_household_survey.r
	Rscript code/2_estimate_regression_models.r
	Rscript code/3_multiple_imputation_regressions.r
	Rscript code/4_extract_official_crime_counts.r
