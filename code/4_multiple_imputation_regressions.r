library(data.table)
library(lfe)
library(Amelia)
library(bucky)

options(scipen = 999)
set.seed(42)

# Get project path
setwd(Sys.getenv("CRIMECOST_PATH"))

# Create media directory if not exists
dir.create("media", showWarnings = FALSE)

# Load RCVS-2021 data, procesed in 2_prepare_data.r
load("data/rcvs2021_crimecost_processed.rdata")

# Define various sets of controls
no_controls <- ""
baseline_controls <- paste0(
	c(
		"male", "age", "age_sq", "education_level", "married", "unemployed", "household_size", "household_size_sq",
		"is_rural", "lnlocality_avg_income", "lnlocality_population"
	),
	collapse = " + "
)
baselineregionfe_controls <- paste0(
	c(
		"male", "age", "age_sq", "education_level", "married", "unemployed", "household_size", "household_size_sq",
		"is_rural", "lnlocality_avg_income", "lnlocality_population", "region"
	),
	collapse = " + "
)

# Perform multiple imputation of regressors in life satisfaction regressions
imputation_controls <- c(
	"male", "age", "education_level", "married", "unemployed", "household_size", "is_rural", "locality_avg_income",
	"locality_population", "region", "victimized12m", "victimized5y", "income_level", "mean_household_income",
	"life_satisfaction", "is_unsafe_alone", "is_unsafe_at_home", "fears_becoming_victim"
)
regression_data <- rcvs2021_crimecost[!is.na(region), c(imputation_controls), with = FALSE]

# Perform the imputation
rcvs2021_crimecost_imputed <- amelia(
	regression_data,
	m = 5, p2s = 2, max.resample = 500, tolerance = 1e-6, parallel = "multicore", ncpus = 12, cs = "region",
	logs = c("mean_household_income", "locality_avg_income", "locality_population"),
	ords = c("life_satisfaction", "income_level"),
	noms = c(
		"male", "education_level", "married", "unemployed", "is_rural", "victimized12m", "victimized5y",
		"is_unsafe_alone", "is_unsafe_at_home", "fears_becoming_victim"
	)
)

# Create logs and squares of the variables of interest in the imputed data
rcvs2021_crimecost_imputed <- transform(
	rcvs2021_crimecost_imputed,
	lnmean_household_income = log(mean_household_income)
)
rcvs2021_crimecost_imputed <- transform(rcvs2021_crimecost_imputed, lnlocality_avg_income = log(locality_avg_income))
rcvs2021_crimecost_imputed <- transform(rcvs2021_crimecost_imputed, lnlocality_population = log(locality_population))
rcvs2021_crimecost_imputed <- transform(rcvs2021_crimecost_imputed, lnlife_satisfaction = log(life_satisfaction))
rcvs2021_crimecost_imputed <- transform(
	rcvs2021_crimecost_imputed,
	lnmean_household_income = log(mean_household_income)
)
rcvs2021_crimecost_imputed <- transform(rcvs2021_crimecost_imputed, age_sq = age^2)
rcvs2021_crimecost_imputed <- transform(rcvs2021_crimecost_imputed, household_size_sq = household_size^2)

# Save point
save(rcvs2021_crimecost_imputed, file = "data/rcvs2021_crimecost_imputed.rdata", compress = "gzip")

## Perform overimputation of the household income variable (we ignore the chart)
income_overimputation <- as.data.table(
	overimpute(rcvs2021_crimecost_imputed, var = "mean_household_income", draws = 100)
)
income_overimputation[, names(income_overimputation)[grepl("overimps", names(income_overimputation))] := NULL]

# What is the share of observations with known income where the real value is outside the 90% CI of the imputed values
nrow(income_overimputation[orig > upper.overimputed | orig < lower.overimputed]) / nrow(income_overimputation)
# 0.1134293
# For reference: without income_level: 0.1171463

# What is the distance between mean imputed values and real values
mean(income_overimputation$orig - income_overimputation$mean.overimputed, na.rm = TRUE)
# 0.004312214
# For reference: without income_level: -0.1011772
median(income_overimputation$orig - income_overimputation$mean.overimputed, na.rm = TRUE)
# -3.254755
# For reference: without income_level: -4.065382

## Plot the density comparison
# RU version
cairo_pdf(filename = "media/density_mean_income_actual_imputed.pdf", height = 10, width = 15)
compare.density(
	rcvs2021_crimecost_imputed,
	var = "mean_household_income", main = "",
	xlab = "Ежемесячный доход на члена домохозяйства, тыс. руб.",
	ylab = "Плотность распределения", legend = FALSE, cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5
)
dev.off()

# EN version
cairo_pdf(filename = "media/density_mean_income_actual_imputed_en.pdf", height = 10, width = 15)
compare.density(
	rcvs2021_crimecost_imputed,
	var = "mean_household_income", main = "",
	xlab = "Monthly per capita household income, thou. rubles",
	ylab = "Density distribution", legend = FALSE, cex.lab = 1.5, cex.main = 1.5, cex.axis = 1.5
)
dev.off()

## Fit OLS models to report/use on imputed data
# Define various model scenarios
controls <- c("no", "baseline", "baselineregionfe")
victimization_vars <- c("", "victimized5y", "victimized12m")
income_vars <- c("", "lnmean_household_income", "mean_household_income")
satisfaction_vars <- c("lnlife_satisfaction")
fearofcrime_vars <- c("", "is_unsafe_alone", "is_unsafe_at_home", "fears_becoming_victim")
raking_vars <- c("")

# All possible scenarios
scenarios <- CJ(controls, victimization_vars, income_vars, satisfaction_vars, fearofcrime_vars, raking_vars)

# Remove degenerate combinations
scenarios <- scenarios[!(victimization_vars == "" & income_vars == "" & fearofcrime_vars == "")]

# Do not put victimization and fear of crime in one model
scenarios <- scenarios[!(victimization_vars != "" & fearofcrime_vars != "")]

# Fit the models specified by scenarios
for (i in seq_len(nrow(scenarios))) {
	## Build the regression formula
	# Add controls
	regression_formula <- get(paste0(scenarios$controls[i], "_controls"))

	# Remove region dummies (add them in another part of the formula)
	regression_formula <- gsub(" \\+ region$", "", regression_formula)

	# Add victimization variable
	regression_formula <- paste0(scenarios$victimization_vars[i], " + ", regression_formula)

	# Add income variable
	regression_formula <- paste0(scenarios$income_vars[i], " + ", regression_formula)

	# Add fear of crime variable
	regression_formula <- paste0(scenarios$fearofcrime_vars[i], " + ", regression_formula)

	# Add dependent variable
	regression_formula <- paste0(scenarios$satisfaction_vars[i], " ~ ", regression_formula)

	# Escape one exception at formula end
	regression_formula <- gsub("[ \\+]{1,9} $", "", regression_formula, perl = TRUE)

	# Add error cluster specification
	if (!grepl("regionfe", scenarios$controls[i])) {
		regression_formula <- paste0(regression_formula, " | 0 | 0 | region")
	} else {
		regression_formula <- paste0(regression_formula, " | region | 0 | region")
	}
	regression_formula <- as.formula(regression_formula)

	## Fit the model
	if (!grepl("raking", scenarios$raking_vars[i])) {
		out <- bucky::mi.eval(felm(regression_formula, data = rcvs2021_crimecost_imputed))
	}

	## Save to a designated object
	object_name <- paste(
		"ols", scenarios$satisfaction_vars[i], scenarios$victimization_vars[i], scenarios$fearofcrime_vars[i],
		scenarios$income_vars[i], scenarios$controls[i], scenarios$raking_vars[i],
		sep = "_"
	)
	object_name <- gsub("life_satisfaction", "satisf", object_name)
	object_name <- gsub("mean_household_income", "income", object_name)
	object_name <- gsub("is_unsafe_alone", "unsafealone", object_name)
	object_name <- gsub("is_unsafe_at_home", "unsafeathome", object_name)
	object_name <- gsub("fears_becoming_victim", "fearsvictim", object_name)
	object_name <- gsub("_+", "_", object_name)
	object_name <- gsub("_$", "", object_name)
	assign(object_name, out)
	message(paste0("Fit model ", i, "/", nrow(scenarios)))
}

### Create regression tables with OLS models estimated on imputed data
### Report by hand since stargazer does not support it yet
## Life satisfaction and victimization
# Column 1
summary(ols_lnsatisf_victimized5y_no)

# Column 2
summary(ols_lnsatisf_victimized5y_baseline)

# Column 3
summary(ols_lnsatisf_victimized5y_baselineregionfe)

# Column 4
summary(ols_lnsatisf_victimized12m_no)

# Column 5
summary(ols_lnsatisf_victimized12m_baseline)

# Column 6
summary(ols_lnsatisf_victimized12m_baselineregionfe)


## Life satisfaction and income
# Column 1
summary(ols_lnsatisf_income_no)

# Column 2
summary(ols_lnsatisf_lnincome_no)

# Column 3
summary(ols_lnsatisf_income_baselineregionfe)

# Column 4
summary(ols_lnsatisf_lnincome_baselineregionfe)

# Column 5
summary(ols_lnsatisf_victimized12m_income_baselineregionfe)

# Column 6
summary(ols_lnsatisf_victimized12m_lnincome_baselineregionfe)

## Life satisfaction and fear of crime
# Column 1
summary(ols_lnsatisf_unsafealone_no)

# Column 2
summary(ols_lnsatisf_unsafealone_baselineregionfe)

# Column 3
summary(ols_lnsatisf_unsafeathome_no)

# Column 4
summary(ols_lnsatisf_unsafeathome_baselineregionfe)

# Column 5
summary(ols_lnsatisf_fearsvictim_no)

# Column 6
summary(ols_lnsatisf_fearsvictim_baselineregionfe)
