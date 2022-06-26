# Alias for running docker image
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

# Pre-computed crime cost data
data/yearly_total_crime_cost.rds:
	# Do nothing, the file is created outside the repo
	noop

# Compute raking weights
data/rcvs2021_raking_fit.rdata: data/rcvs2021_crimecost.rdata data/kouzh_2020_data.rdata
	$(RUN) code/1_rcvs_vs_domestic_household_survey.r

# Estimate summary statistics
tables/survey_summary_stat.tex: data/rcvs2021_crimecost.rdata data/kouzh_2020_data.rdata
	$(RUN) code/1_rcvs_vs_domestic_household_survey.r
	
# Fit the regression models
tables/ols_lnlifesatisfaction_victimization.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_victimization_raking.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_income.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_income_raking.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_fear.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_fear_raking.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_crimetype.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/ols_lnlifesatisfaction_crimetype_raking.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
tables/costofcrime_estimates_table_yearly.tex: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
estimates/costofcrime_estimates_19aug21.csv: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r

# Plot income derivative
media/lifesatisfaction_income_deriv_plot.pdf: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r
media/lifesatisfaction_income_deriv_plot_en.pdf: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/2_estimate_regression_models.r

# Perform multiple imputation
data/rcvs2021_crimecost_imputed.rdata: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/3_multiple_imputation_regressions.r

# Plot income densities after imputation
media/density_mean_income_actual_imputed.pdf: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/3_multiple_imputation_regressions.r
media/density_mean_income_actual_imputed_en.pdf: data/rcvs2021_crimecost.rdata data/rcvs2021_raking_fit.rdata
	$(RUN) code/3_multiple_imputation_regressions.r

# Create a chart with official crime statistics and estimates
media/official_crime_costs.pdf: data/rcvs2021_crimecost.rdata
	$(RUN) code/4_extract_official_crime_counts.r
media/official_crime_costs_en.pdf: data/yearly_total_crime_cost.rds
	$(RUN) code/4_extract_official_crime_counts.r

# Data recipe
data: data/kouzh_2020_data.rdata \
	  data/rcvs2021_crimecost.rdata \
	  data/rcvs2021_raking_fit.rdata \
	  data/rcvs2021_crimecost_imputed.rdata

# Tables recipe
tables: estimates/costofcrime_estimates_19aug21.csv \
	    tables/survey_summary_stat.tex \
	    tables/ols_lnlifesatisfaction_victimization.tex \
	    tables/ols_lnlifesatisfaction_victimization.tex \
	    tables/ols_lnlifesatisfaction_income.tex \
	    tables/ols_lnlifesatisfaction_fear.tex \
	    tables/ols_lnlifesatisfaction_crimetype.tex \
	    tables/ols_lnlifesatisfaction_victimization_raking.tex \
	    tables/ols_lnlifesatisfaction_income_raking.tex \
	    tables/ols_lnlifesatisfaction_fear_raking.tex \
	    tables/ols_lnlifesatisfaction_crimetype_raking.tex \
	    tables/costofcrime_estimates_table_yearly.tex \

# Plots recipe
plots: media/lifesatisfaction_income_deriv_plot.pdf \
	   media/lifesatisfaction_income_deriv_plot_en.pdf \
	   media/density_mean_income_actual_imputed.pdf \
	   media/density_mean_income_actual_imputed_en.pdf \
	   media/official_crime_costs.pdf \
	   media/official_crime_costs_en.pdf \

# Paper recipe
paper: data tables plots
