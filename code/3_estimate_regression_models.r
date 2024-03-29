library(data.table)
library(lfe)
library(mgcv)
library(boot)
library(ggplot2)
library(ggthemes)
library(scales)

options(scipen = 999)
set.seed(42)

# Get project path
setwd(Sys.getenv("CRIMECOST_PATH"))
source("code/helper_functions/utils.r")

# Create directories if not exist
dir.create("media", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("estimates", showWarnings = FALSE)

# Load RCVS-2021 data, procesed in 2_prepare_data.r
load("data/rcvs2021_crimecost_processed.rdata")

# Define various sets of controls
no_controls <- ""
sociodemographic_controls <- paste0(
	c("male", "age", "age_sq", "household_size", "household_size_sq"),
	collapse = " + "
)
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


## Fit OLS models to report/use
# Define various model scenarios
controls <- c("no", "sociodemographic", "baseline", "baselineregionfe")
victimization_vars <- c(
	"", "victimized12m", "victimized5y", "assault", "robbery", "theft", "larceny", "fraud", "remote", "attemptedremote",
	"other"
)
income_vars <- c("", "lnmean_household_income", "mean_household_income")
satisfaction_vars <- c("lnlife_satisfaction", "life_satisfaction")
fearofcrime_vars <- c("", "is_unsafe_alone", "is_unsafe_at_home", "fears_becoming_victim")
raking_vars <- c("", "raking")

# All possible scenarios
scenarios <- CJ(controls, victimization_vars, income_vars, satisfaction_vars, fearofcrime_vars, raking_vars)

# Remove degenerate combinations
scenarios <- scenarios[!(controls == "no" & victimization_vars == "" & income_vars == "" & fearofcrime_vars == "")]

# Do not put victimization and fear of crime in one model
scenarios <- scenarios[!(victimization_vars != "" & fearofcrime_vars != "")]

# Only one set of scenarios with crime type variables
crime_types <- c("assault", "robbery", "theft", "larceny", "fraud", "remote", "attemptedremote", "other")
scenarios <- scenarios[!(controls != "baselineregionfe" & victimization_vars %in% crime_types)]
scenarios <- scenarios[
	!(victimization_vars %in% crime_types & !(income_vars %in% c("mean_household_income", "lnmean_household_income")))
]
scenarios <- scenarios[!(satisfaction_vars != "lnlife_satisfaction" & victimization_vars %in% crime_types)]

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
		out <- felm(regression_formula, data = rcvs2021_crimecost[!is.na(region)])
	} else {
		out <- felm(
			regression_formula,
			data = rcvs2021_crimecost[!is.na(region)],
			weights = rcvs2021_crimecost[!is.na(region)]$raking_weight
		)
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

## Create regression tables with OLS models
# Life satisfaction and victimization
quiet_stargazer(
	ols_lnsatisf_victimized5y_no, ols_lnsatisf_victimized5y_baseline, ols_lnsatisf_victimized5y_baselineregionfe,
	ols_lnsatisf_victimized12m_no, ols_lnsatisf_victimized12m_baseline, ols_lnsatisf_victimized12m_baselineregionfe,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 6),
	out = "tables/ols_lnlifesatisfaction_victimization.tex"
)

# Life satisfaction and income
quiet_stargazer(
	ols_lnsatisf_income_no, ols_lnsatisf_lnincome_no, ols_lnsatisf_income_baselineregionfe,
	ols_lnsatisf_lnincome_baselineregionfe, ols_lnsatisf_victimized12m_income_baselineregionfe,
	ols_lnsatisf_victimized12m_lnincome_baselineregionfe,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 6),
	out = "tables/ols_lnlifesatisfaction_income.tex"
)

# Life satisfaction and fear of crime
quiet_stargazer(
	ols_lnsatisf_unsafealone_no, ols_lnsatisf_unsafealone_baselineregionfe, ols_lnsatisf_unsafeathome_no,
	ols_lnsatisf_unsafeathome_baselineregionfe, ols_lnsatisf_fearsvictim_no, ols_lnsatisf_fearsvictim_baselineregionfe,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 6),
	out = "tables/ols_lnlifesatisfaction_fear.tex"
)

# Life satisfaction, income and different types of crimes
quiet_stargazer(
	ols_lnsatisf_assault_lnincome_baselineregionfe, ols_lnsatisf_robbery_lnincome_baselineregionfe,
	ols_lnsatisf_fraud_lnincome_baselineregionfe, ols_lnsatisf_theft_lnincome_baselineregionfe,
	ols_lnsatisf_larceny_lnincome_baselineregionfe, ols_lnsatisf_remote_lnincome_baselineregionfe,
	ols_lnsatisf_attemptedremote_lnincome_baselineregionfe, ols_lnsatisf_other_lnincome_baselineregionfe,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 7),
	out = "tables/ols_lnlifesatisfaction_crimetype.tex"
)

## Same tables, but with raking weights
# Life satisfaction and victimization
quiet_stargazer(
	ols_lnsatisf_victimized5y_no_raking, ols_lnsatisf_victimized5y_baseline_raking,
	ols_lnsatisf_victimized5y_baselineregionfe_raking, ols_lnsatisf_victimized12m_no_raking,
	ols_lnsatisf_victimized12m_baseline_raking, ols_lnsatisf_victimized12m_baselineregionfe_raking,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 6),
	out = "tables/ols_lnlifesatisfaction_victimization_raking.tex"
)

# Life satisfaction and income
quiet_stargazer(
	ols_lnsatisf_income_no_raking, ols_lnsatisf_lnincome_no_raking, ols_lnsatisf_income_baselineregionfe_raking,
	ols_lnsatisf_lnincome_baselineregionfe_raking, ols_lnsatisf_victimized12m_income_baselineregionfe_raking,
	ols_lnsatisf_victimized12m_lnincome_baselineregionfe_raking,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 6),
	out = "tables/ols_lnlifesatisfaction_income_raking.tex"
)

# Life satisfaction and fear of crime
quiet_stargazer(
	ols_lnsatisf_unsafealone_no_raking, ols_lnsatisf_unsafealone_baselineregionfe_raking,
	ols_lnsatisf_unsafeathome_no_raking, ols_lnsatisf_unsafeathome_baselineregionfe_raking,
	ols_lnsatisf_fearsvictim_no_raking, ols_lnsatisf_fearsvictim_baselineregionfe_raking,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 6),
	out = "tables/ols_lnlifesatisfaction_fear_raking.tex"
)

# Life satisfaction, income and different types of crimes
quiet_stargazer(
	ols_lnsatisf_assault_lnincome_baselineregionfe_raking, ols_lnsatisf_robbery_lnincome_baselineregionfe_raking,
	ols_lnsatisf_fraud_lnincome_baselineregionfe_raking, ols_lnsatisf_theft_lnincome_baselineregionfe_raking,
	ols_lnsatisf_larceny_lnincome_baselineregionfe_raking, ols_lnsatisf_remote_lnincome_baselineregionfe_raking,
	ols_lnsatisf_attemptedremote_lnincome_baselineregionfe_raking, ols_lnsatisf_other_lnincome_baselineregionfe_raking,
	type = "latex", summary = FALSE, float = TRUE, multicolumn = TRUE, dep.var.labels = rep("ln(Life satisfaction)", 7),
	out = "tables/ols_lnlifesatisfaction_crimetype_raking.tex"
)

## Fit semiparametric model of life satisfaction on the spline of income, victimization, and fear of crime
# Reduce the set of scenarios to consider for semiparametric estimation
scenarios_semipar <- scenarios[
	income_vars == "mean_household_income" & satisfaction_vars == "lnlife_satisfaction" & raking_vars == ""
]
scenarios_semipar <- scenarios_semipar[!(victimization_vars == "" & fearofcrime_vars == "")]

# Fit the models specified by scenarios_semipar
for (i in seq_len(nrow(scenarios_semipar))) {
	## Build the regression formula
	# Add controls
	regression_formula <- get(paste0(scenarios_semipar$controls[i], "_controls"))

	# Add victimization variable
	regression_formula <- paste0(scenarios_semipar$victimization_vars[i], " + ", regression_formula)

	# Add income variable (in semiparametric fashion)
	regression_formula <- paste0("s(", scenarios_semipar$income_vars[i], ") + ", regression_formula)

	# Add fear of crime variable
	regression_formula <- paste0(scenarios_semipar$fearofcrime_vars[i], " + ", regression_formula)

	# Add dependent variable
	regression_formula <- paste0(scenarios_semipar$satisfaction_vars[i], " ~ ", regression_formula)

	# Escape one exception at formula end
	regression_formula <- gsub("[ \\+]{1,9} $", "", regression_formula, perl = TRUE)

	regression_formula <- as.formula(regression_formula)

	## Fit the model
	out <- gam(regression_formula, data = rcvs2021_crimecost[!is.na(region)], family = gaussian())

	## Save to a designated object
	object_name <- paste(
		"gam", scenarios_semipar$satisfaction_vars[i], scenarios_semipar$victimization_vars[i],
		scenarios_semipar$fearofcrime_vars[i], scenarios_semipar$income_vars[i], scenarios_semipar$controls[i],
		scenarios_semipar$raking_vars[i],
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
	message(paste0("Fit model ", i, "/", nrow(scenarios_semipar)))
}

## Estimate first derivatives of the estimated models w.r.t income, victimization, and fear of crime as well as their CI
## at average values of all regressors except for income which we vary in the semiparametric models
# Define the set of scenarios to consder for derivative estimation
scenarios_derivatives <- scenarios[
	income_vars %in% c("mean_household_income", "lnmean_household_income") &
		satisfaction_vars == "lnlife_satisfaction" &
		raking_vars == ""
]
scenarios_derivatives <- scenarios_derivatives[!(victimization_vars == "" & fearofcrime_vars == "")]

# Compute the average values of regressors in the data
regressor_averages <- rcvs2021_crimecost[, lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE))]
regressor_averages <- regressor_averages[, lapply(.SD, function(x) mean(as.numeric(x), na.rm = TRUE))]

# Round the factors to modal factors
regressors_factors <- c("education_level", "region")
regressor_averages[,
	c(regressors_factors) := lapply(.SD, function(x) as.factor(round(x, 0))),
	.SDcols = regressors_factors
]

# Remove NA columns
regressor_averages <- Filter(function(x) !all(is.na(x)), regressor_averages)

# Expand to multiple levels of mean per capita household income (from 1K RUB to 150K RUB)
regressor_averages_at_income <- data.table(
	regressor_averages[, -c("lnmean_household_income", "mean_household_income")],
	mean_household_income = 1:150, lnmean_household_income = log(1:150)
)

# Init an object to store derivative values
income_crime_derivatives <- data.table()

# Iterate over the scenarios
for (i in seq_len(nrow(scenarios_derivatives))) {
	for (model in c("ols", "gam")) {
		# Form the object name
		object_name <- paste(
			model, scenarios_derivatives$satisfaction_vars[i], scenarios_derivatives$victimization_vars[i],
			scenarios_derivatives$fearofcrime_vars[i], scenarios_derivatives$income_vars[i],
			scenarios_derivatives$controls[i], scenarios_derivatives$raking_vars[i],
			sep = "_"
		)
		object_name <- gsub("life_satisfaction", "satisf", object_name)
		object_name <- gsub("mean_household_income", "income", object_name)
		object_name <- gsub("is_unsafe_alone", "unsafealone", object_name)
		object_name <- gsub("is_unsafe_at_home", "unsafeathome", object_name)
		object_name <- gsub("fears_becoming_victim", "fearsvictim", object_name)
		object_name <- gsub("_+", "_", object_name)
		object_name <- gsub("_$", "", object_name)

		# Store income regressor name separately
		income_var <- scenarios_derivatives$income_vars[i]

		# Store victimization/fear of crime regressor name separately
		crime_var <- ifelse(
			nchar(scenarios_derivatives$victimization_vars[i]) > 0,
			scenarios_derivatives$victimization_vars[i],
			scenarios_derivatives$fearofcrime_vars[i]
		)

		if (exists(object_name)) {
			if (model == "ols") {

				# For OLS the derivatives are constant at different values of regressors
				out <- data.table(
					model = model,
					scenarios_derivatives[i],
					regressor_averages_at_income[, c("mean_household_income")],
					income_deriv = unname(coef(get(object_name))[income_var]),
					income_se = unname(sqrt(diag(vcov(get(object_name))))[income_var]),
					crime_deriv = unname(coef(get(object_name))[crime_var]),
					crime_se = unname(sqrt(diag(vcov(get(object_name))))[crime_var])
				)
			} else if (model == "gam") {
				smooth_deriv <- gratia::derivatives(
					get(object_name),
					term = paste0("s(", income_var, ")"), newdata = regressor_averages_at_income
				)
				smooth_deriv <- as.data.table(smooth_deriv)[, c("data", "derivative", "se"), with = FALSE]
				setnames(
					smooth_deriv, c("data", "derivative", "se"), c("mean_household_income", "income_deriv", "income_se")
				)

				# For GAM the derivatives are income-varying
				out <- data.table(
					model = model,
					scenarios_derivatives[i],
					smooth_deriv,
					crime_deriv = unname(coef(get(object_name))[crime_var]),
					crime_se = unname(sqrt(diag(vcov(get(object_name))))[crime_var])
				)
			}

			# Row-bind the result
			income_crime_derivatives <- rbind(income_crime_derivatives, out, fill = TRUE)
		}
	}
}

# Compute the the 95% confidence intervals
income_crime_derivatives[, income_upper := income_deriv + 1.96 * income_se]
income_crime_derivatives[, income_lower := income_deriv - 1.96 * income_se]
income_crime_derivatives[, crime_upper := crime_deriv + 1.96 * crime_se]
income_crime_derivatives[, crime_lower := crime_deriv - 1.96 * crime_se]

## Plot the relationship betw. the first derivative of mean household income and life satisfaction at various
## levels of income, holding other regressors at mean values (factor regressors are at their modal values)
# Define subsets for brevity
income_crime_derivatives[controls == "baselineregionfe" & victimization_vars == "victimized12m", base_condition := TRUE]
income_crime_derivatives[(base_condition) & model == "gam", gam_base_condition := TRUE]
income_crime_derivatives[
	(gam_base_condition) & mean_household_income == 24 & income_vars == "mean_household_income", gam_condition := TRUE
]
income_crime_derivatives[
	(base_condition) & model == "ols" & income_vars == "mean_household_income", ols_base_condition := TRUE
]
income_crime_derivatives[(ols_base_condition) & mean_household_income == 24, ols_condition := TRUE]

# RU version
lifesat_income_deriv_plot <- ggplot(
	aes(x = mean_household_income, y = (exp(income_deriv) - 1)),
	data = income_crime_derivatives[(gam_base_condition)]
) +
	geom_line(linetype = "solid", lwd = 2) +
	geom_ribbon(
		aes(ymin = (exp(income_lower) - 1), ymax = (exp(income_upper) - 1)),
		alpha = .3, colour = NA, fill = "grey70"
	) +
	# OLS coefficient
	geom_hline(
		aes(yintercept = (exp(income_crime_derivatives[(ols_base_condition)]$income_deriv[1]) - 1)),
		color = "red", linetype = "dashed"
	) +
	geom_ribbon(
		aes(
			ymin = (exp(income_crime_derivatives[(ols_base_condition)]$income_lower[1]) - 1),
			ymax = (exp(income_crime_derivatives[(ols_base_condition)]$income_upper[1]) - 1)
		),
		alpha = .3, colour = NA, fill = "red"
	) +
	# For mean income line
	geom_vline(aes(xintercept = 24), color = "#ED0000FF", linetype = "dotted") +
	# Annotate GAM estimate
	annotate("text",
		x = 24 + 10,
		y = exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1 + 0.003,
		label = paste0(
			"GAM-оценка для домохозяйства\nсо средним доходом: ",
			round(100 * (exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1), 2), "%"
		),
		hjust = 0, size = 6
	) +
	annotate("segment",
		x = 24 + 10,
		xend = 24,
		y = exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1 + 0.003,
		yend = exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1,
		arrow = arrow(), color = "black"
	) +
	# Annotate OLS estimate
	annotate("text",
		x = 1,
		y = exp(income_crime_derivatives[(ols_condition)]$income_deriv) - 1 + 0.002,
		label = paste0(
			"МНК-оценка: ", round(100 * (exp(income_crime_derivatives[(ols_condition)]$income_deriv) - 1), 2), "%"
		),
		hjust = 0, size = 6
	) +
	scale_y_continuous(
		name = paste0(
			"% изменение в удовлетворенности жизнью из-за увеличения\n",
			"ежемесячного дохода на члена домохозяйства на 1 тыс. руб."
		),
		labels = scales::percent
	) +
	scale_x_continuous(
		name = "Ежемесячный доход на члена домохозяйства, тыс. руб.",
		breaks = pretty_breaks(n = 15), limits = c(1, 152), expand = c(0, 0)
	) +
	theme_minimal() +
	theme(text = element_text(size = 20))

# Save plot
ggsave(
	"media/lifesatisfaction_income_deriv_plot.pdf",
	plot = lifesat_income_deriv_plot, height = 10, width = 15, device = cairo_pdf
)

# EN version
lifesat_income_deriv_plot_en <- ggplot(
	aes(x = mean_household_income, y = (exp(income_deriv) - 1)),
	data = income_crime_derivatives[(gam_base_condition)]
) +
	geom_line(linetype = "solid", lwd = 2) +
	geom_ribbon(aes(
		ymin = (exp(income_lower) - 1),
		ymax = (exp(income_upper) - 1)
	),
	alpha = .3, colour = NA, fill = "grey70"
	) +
	# OLS coefficient
	geom_hline(
		aes(yintercept = (exp(income_crime_derivatives[(ols_base_condition)]$income_deriv[1]) - 1)),
		color = "red", linetype = "dashed"
	) +
	geom_ribbon(
		aes(
			ymin = (exp(income_crime_derivatives[(ols_base_condition)]$income_lower[1]) - 1),
			ymax = (exp(income_crime_derivatives[(ols_base_condition)]$income_upper[1]) - 1)
		),
		alpha = .3, colour = NA, fill = "red"
	) +
	# For mean income line
	geom_vline(aes(xintercept = 24), color = "#ED0000FF", linetype = "dotted") +
	# Annotate GAM estimate
	annotate("text",
		x = 24 + 10,
		y = exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1 + 0.003,
		label = paste0(
			"GAM estimate for a household\nwith the average income: ",
			round(100 * (exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1), 2), "%"
		),
		hjust = 0, size = 6
	) +
	annotate("segment",
		x = 24 + 10,
		xend = 24,
		y = exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1 + 0.003,
		yend = exp(income_crime_derivatives[(gam_condition)]$income_deriv) - 1,
		arrow = arrow(), color = "black"
	) +
	# Annotate OLS estimate
	annotate("text",
		x = 1,
		y = exp(income_crime_derivatives[(ols_condition)]$income_deriv) - 1 + 0.002,
		label = paste0(
			"OLS estimate: ", round(100 * (exp(income_crime_derivatives[(ols_condition)]$income_deriv) - 1), 2), "%"
		),
		hjust = 0, size = 6
	) +
	scale_y_continuous(
		name = "% change in life satisfaction after 1 thou. rubles increase\nin monthly per capita household income",
		labels = scales::percent
	) +
	scale_x_continuous(
		name = "Monthly per capita household income, thou. rubles",
		breaks = pretty_breaks(n = 15), limits = c(1, 152), expand = c(0, 0)
	) +
	theme_minimal() +
	theme(text = element_text(size = 20))

# Save plot
ggsave(
	"media/lifesatisfaction_income_deriv_plot_en.pdf",
	plot = lifesat_income_deriv_plot_en, height = 10, width = 15, device = cairo_pdf
)

## Produce cost of crime estimates from the fitted models
# Create an object with various model specifications and income set at the mean level
costofcrime_estimates <- income_crime_derivatives[
	mean_household_income == round(regressor_averages$mean_household_income, 0) &
		controls %in% c("baselineregionfe") & income_vars == "mean_household_income"
]

# Keep only variables of interest
vars_to_remove <- c(
	"income_se", "income_upper", "income_lower", "crime_se", "crime_upper", "crime_lower",
	names(income_crime_derivatives)[grepl("_exp", names(income_crime_derivatives))]
)
costofcrime_estimates <- costofcrime_estimates[, -c(vars_to_remove), with = FALSE]

# Gather crime vars in one object
costofcrime_estimates[, crime_var := victimization_vars]
costofcrime_estimates[nchar(fearofcrime_vars) > 0, crime_var := fearofcrime_vars]
costofcrime_estimates[, c("victimization_vars", "fearofcrime_vars") := NULL]

# Create the point estimate
costofcrime_estimates[, pointest := (exp(crime_deriv) - 1) / (exp(income_deriv) - 1)]
costofcrime_estimates[, c("crime_deriv", "income_deriv", "mean_household_income") := NULL]

# Function to return the point estimate and confidence interval of the ratio of victimization to income betas from OLS
# life satisfaction models with delta- or Fieller method
ratio_ci_analytical <- function(object_name, crime_var, income_var, method = "delta") {
	# Check whether the OLS model exists
	if (exists(object_name) && grepl("^ols_", object_name)) {
		# Obtain the positions of the coefficients of interest in the coefficients vector
		crime_coef_position <- which(names(coef(get(object_name))) == crime_var)
		income_coef_position <- which(names(coef(get(object_name))) == income_var)

		# Critical value
		nalpha <- 0.05
		t2 <- qt(1 - nalpha / 2, get(object_name)$df)

		# Ratio of coefficients
		pointest <- unname(coef(get(object_name))[crime_coef_position] / coef(get(object_name))[income_coef_position])
		pointest_exp <- unname((exp(coef(get(object_name))[crime_coef_position]) - 1) /
			(exp(coef(get(object_name))[income_coef_position]) - 1))

		if (method == "delta") {
			# Build a formula for the delta method statement
			delta_method_formula <- as.formula(
				paste0(" ~ (exp(x", crime_coef_position, ") - 1)/(exp(x", income_coef_position, ") - 1)")
			)

			# Compute the delta SE
			delta_se <- msm::deltamethod(delta_method_formula, coef(get(object_name)), vcov(get(object_name)))

			# Compute the confidence intervals
			upperci <- pointest_exp + t2 * delta_se
			lowerci <- pointest_exp - t2 * delta_se
		}

		if (method == "fieller") {
			# Create ratio matrix
			# (see https://fbe.unimelb.edu.au/__data/assets/pdf_file/0005/2704649/2037_Joe-Hirschberg_Fieller-Examples-WP-version.pdf for notation)
			# T is replace with x for T is shorthand for TRUE in R
			x <- matrix(data = 0, ncol = length(coef(get(object_name))), nrow = 2)
			x[2, income_coef_position] <- 1
			x[1, crime_coef_position] <- 1
			r <- x %*% coef(get(object_name))
			vr <- (x %*% vcov(get(object_name))) %*% t(x)
			aa <- (r[2, 1]^2) - (t2^2 * vr[2, 2])
			bb <- (2 * t2^2 * vr[1, 2]) - (2 * r[1, 1] * r[2, 1])
			cc <- (r[1, 1]^2) - (t2^2 * vr[1, 1])
			rad <- sqrt(bb^2 - 4 * aa * cc)

			# Compute the confidence intervals
			upperci <- (-bb + rad) / (2 * aa)
			lowerci <- (-bb - rad) / (2 * aa)
		}
		# Return a named list with CIs
		return(c(lowerci = lowerci, upperci = upperci))
	} else {
		return(c(lowerci = NA, upperci = NA))
	}
}

# Function to bootstrap that returns the ratio of two coefficients given the data, formula, and covariate specifications
boot_func <- function(regression_data, i, regression_formula, modelname, crime_varname, income_varname, atvalues) {
	# Resample the data
	regression_data_resampled <- regression_data[i, ]
	regression_data_resampled <- droplevels(regression_data_resampled)

	# Discard the iteration if the region is not in the sample (and the regression model uses regions
	if (levels(atvalues$region) %in% levels(regression_data_resampled$region) ||
		!grepl("region", regression_formula[3])) {
		# Fit the model
		if (modelname == "gam") {
			fit <- gam(regression_formula, data = regression_data_resampled)
		} else if (modelname == "ols") {
			fit <- lm(regression_formula, data = regression_data_resampled)
		}

		# Obtain the positions of the coefficients of interest in the coefficients vector
		crime_coef_position <- which(names(coef(fit)) == crime_varname)

		# Get the crime coefficient from the model
		crime_coef <- unname(coef(fit)[crime_coef_position])
		if (modelname == "ols") {
			# For OLS we simply take the coefficient from the model
			income_coef_position <- which(names(coef(fit)) == income_varname)
			income_coef <- unname(coef(fit)[income_coef_position])
		} else if (modelname == "gam") {
			# For GAM we obtain the income coefficient by computing the derivative at specified values
			income_coef <- unname(
				gratia::derivatives(fit, term = paste0("s(", income_varname, ")"), newdata = atvalues)$derivative
			)
		}
		# Ratio of coefficients
		pointest <- (exp(crime_coef) - 1) / (exp(income_coef) - 1)
		return(c(pointest = pointest))
	} else {
		# Otherwise skip the bootstrap replication
		return(c(pointest = NA))
	}
}

# Function to bootstrap the confidence interval around the ratio of victimization to income betas from OLS/GAM life
# satisfaction models
ratio_ci_bootstrap <- function(modelname, replications = 1000, controlsset, crime_varname, income_varname,
							   satisfaction_varname, atvalues = regressor_averages, cores = 2) {
	## Build the regression formula
	# Add controlsset
	regression_formula <- get(paste0(controlsset, "_controls"))

	# Add crime variable
	regression_formula <- paste0(crime_varname, " + ", regression_formula)

	# Add income variable (in parametric or semiparametric fashion, depending on the model)
	if (modelname == "gam") {
		regression_formula <- paste0("s(", income_varname, ") + ", regression_formula)
	} else if (modelname == "ols") {
		regression_formula <- paste0(income_varname, " + ", regression_formula)
	}

	# Add dependent variable
	regression_formula <- paste0(satisfaction_varname, " ~ ", regression_formula)

	# Escape one exception at formula end
	regression_formula <- gsub("[ \\+]{1,9} $", "", regression_formula, perl = TRUE)

	# Resulting formula object
	regression_formula <- as.formula(regression_formula)

	## Create model data.frame by fitting the model once
	regression_data <- gam(regression_formula, data = rcvs2021_crimecost[!is.na(get("region"))])$model

	## Perform the bootstrap
	boot_result <- boot(
		regression_data,
		statistic = boot_func, R = replications, regression_formula = regression_formula, modelname = modelname,
		crime_varname = crime_varname, income_varname = income_varname, atvalues = atvalues, parallel = "multicore",
		ncpus = cores
	)

	## Get 95% percentile bootstrap confidence interval
	boot_resultci <- boot.ci(boot_result, conf = 0.95, type = "basic")

	## Return a named list with CIs
	return(c(lowerci = unname(boot_resultci$basic[1, 4]), upperci = unname(boot_resultci$basic[1, 5])))
}

## Compute the CIs around cost of crime estimate for each model
for (i in seq_len(nrow(costofcrime_estimates))) {
	# Form the object name
	object_name <- paste(
		costofcrime_estimates$model[i], costofcrime_estimates$satisfaction_vars[i], costofcrime_estimates$crime[i],
		costofcrime_estimates$fearofcrime_vars[i], costofcrime_estimates$income_vars[i],
		costofcrime_estimates$controls[i], costofcrime_estimates$raking_vars[i],
		sep = "_"
	)
	object_name <- gsub("life_satisfaction", "satisf", object_name)
	object_name <- gsub("mean_household_income", "income", object_name)
	object_name <- gsub("is_unsafe_alone", "unsafealone", object_name)
	object_name <- gsub("is_unsafe_at_home", "unsafeathome", object_name)
	object_name <- gsub("fears_becoming_victim", "fearsvictim", object_name)
	object_name <- gsub("_+", "_", object_name)
	object_name <- gsub("_$", "", object_name)

	# Delta-method CI
	delta_ci <- ratio_ci_analytical(
		object_name = object_name, crime_var = costofcrime_estimates$crime_var[i],
		income_var = costofcrime_estimates$income_vars[i], method = "delta"
	)
	costofcrime_estimates[i, cilower_delta := delta_ci["lowerci"]]
	costofcrime_estimates[i, ciupper_delta := delta_ci["upperci"]]

	# Bootsrap CI
	bootstrap_ci <- ratio_ci_bootstrap(
		modelname = costofcrime_estimates$model[i], replications = 2000,
		controlsset = costofcrime_estimates$controls[i], crime_varname = costofcrime_estimates$crime_var[i],
		income_varname = costofcrime_estimates$income_vars[i],
		satisfaction_varname = costofcrime_estimates$satisfaction_vars[i], atvalues = regressor_averages, cores = 12
	)
	costofcrime_estimates[i, cilower_bootstrap := bootstrap_ci["lowerci"]]
	costofcrime_estimates[i, ciupper_bootstrap := bootstrap_ci["upperci"]]
	message(paste0("Processed model ", i, "/", nrow(costofcrime_estimates)))
}

# Save point
fwrite(costofcrime_estimates, file = "estimates/costofcrime_estimates_19aug21.csv", eol = "\n")

## Prepare the table with cost of crime estimates for latex export
# Read in the above-computed cost of crime
costofcrime_estimates <- fread("estimates/costofcrime_estimates_19aug21.csv", encoding = "UTF-8", na.strings = "")

# Remove unused columns
costofcrime_estimates_table <- costofcrime_estimates[, -c("income_vars", "satisfaction_vars", "raking_vars")]

# To thousands of rubles and inverted yearly values,
numeric_vars <- c("pointest", "cilower_delta", "ciupper_delta", "cilower_bootstrap", "ciupper_bootstrap")
costofcrime_estimates_table[,
	c(numeric_vars) := lapply(.SD, function(x) -round(x * 1000 * 12, 0)),
	.SDcols = numeric_vars
]

# Delta CI strings
costofcrime_estimates_table[, ci_delta := paste0("[", ciupper_delta, "--", cilower_delta, "]")]
costofcrime_estimates_table[is.na(cilower_delta), ci_delta := NA]

# Bootstrap CI strings (invert the CIs to get positive values)
costofcrime_estimates_table[, ci_bootstrap := paste0("(", ciupper_bootstrap, "--", cilower_bootstrap, ")")]
costofcrime_estimates_table[is.na(ci_bootstrap), ci_bootstrap := NA]

# To long form
costofcrime_estimates_table <- melt(
	costofcrime_estimates_table,
	id.vars = c("model", "controls", "crime_var", "pointest"), measure.var = c("ci_delta", "ci_bootstrap"),
	variable.name = "type", value.name = "value", na.rm = TRUE
)
costofcrime_estimates_table <- rbind(
	unique(
		data.table(
			model = costofcrime_estimates_table$model, controls = costofcrime_estimates_table$controls,
			crime_var = costofcrime_estimates_table$crime_var, type = "pointest",
			value = costofcrime_estimates_table$pointest
		)
	), costofcrime_estimates_table[, -"pointest"]
)

# CIs below the values
costofcrime_estimates_table <- dcast(costofcrime_estimates_table, crime_var + type ~ model, value.var = "value")
preferred_order <- CJ(
	crime_var = c(
		"victimized12m", "victimized5y", "assault", "robbery", "theft", "larceny", "fraud", "remote", "attemptedremote",
		"other", "fears_becoming_victim", "is_unsafe_alone", "is_unsafe_at_home"
	), type = c("pointest", "ci_bootstrap", "ci_delta"), sorted = FALSE
)
costofcrime_estimates_table <- costofcrime_estimates_table[preferred_order, on = c("crime_var", "type")]

# Output to latex
quiet_stargazer(
	costofcrime_estimates_table[, c("crime_var", "ols", "gam"), with = FALSE],
	type = "latex", summary = FALSE, out = "tables/costofcrime_estimates_table_yearly.tex"
)
