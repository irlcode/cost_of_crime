library(data.table)
library(fastDummies)
library(anesrake)

options(scipen = 999)

# Get project path
setwd(Sys.getenv("CRIMECOST_PATH"))
source("code/helper_functions/utils.r")

# Create tables directory if not exist
dir.create("tables", showWarnings = FALSE)

# Load KOUZh-2020 data prepared beforehand
load("data/kouzh_2020_data.rdata")

# Load RCVS-2021 data prepared beforehand
load("data/rcvs2021_crimecost.rdata")

## Compute the summary statistics
# Define the summary stat variables to include in the table
summary_stat_vars <- c(
	"male", "age", "agegroup", "education_level", "lives_alone", "married", "household_size", "unemployed", "is_rural",
	"locality_population", "locality_avg_income", "mean_household_income", "income_level", "life_satisfaction",
	"fears_becoming_victim", "is_unsafe_alone", "is_unsafe_at_home", "victimized12m", "victimized5y", "crimetype"
)

# Function to return mean and CI for the variable in the given survey object
return_weighted_mean_se <- function(survey_obj, variable) {
	mean_obj <- survey::svymean(~ get(variable), survey_obj, na.rm = TRUE)
	cis <- as.numeric(confint(mean_obj))
	names(cis) <- c("cilower", "ciupper")
	nobs <- sum(!is.na(survey_obj$variables[[variable]]))
	out <- data.table::data.table(variable = variable, meanvalue = as.numeric(mean_obj), t(cis), nobs = nobs)
	return(out)
}

# Objects of interest to compute the summary stat
objs_of_interest <- c("rcvs2021_crimecost", "kouzh_2020_data")

# Init a data.table to store the results (for all observations)
survey_summary_stat_long <- data.table()
for (obj in objs_of_interest) {
	# Create a new object with selected variables and expand their dummies
	temp_obj <- get(obj)[, intersect(names(get(obj)), c(summary_stat_vars, "survey_weight")), with = FALSE]
	temp_obj <- dummy_cols(
		temp_obj,
		remove_first_dummy = FALSE, remove_most_frequent_dummy = FALSE, ignore_na = TRUE, remove_selected_columns = TRUE
	)

	# Set same weight for RCVS
	if (!grepl("kouzh", obj)) {
		temp_obj[, survey_weight := 1]
	}

	# We need to repeat the below logic two more times on a subset of victimised and non-victimised  individuals
	# for the RCVS data.
	# NB: this is a very inelegant and hard-coded way to do it.
	subset_groups <- 1:ifelse(grepl("rcvs", obj), 3, 1)
	for (j in subset_groups) {
		# Create a survey object from the source data with or without weights depending on it being KOUZh
		temp_obj_svy <- survey::svydesign(ids = ~1, data = temp_obj, weights = ~survey_weight)
		obj_name <- obj

		# Next, apply subsetting if it is RCVS and we are at a certain iteration
		if (j == 2) {
			temp_obj_svy <- subset(temp_obj_svy, victimized5y == 0 & victimized12m == 0)
			obj_name <- "not-victimized"
		} else if (j == 3) {
			temp_obj_svy <- subset(temp_obj_svy, victimized5y == 1 | victimized12m == 1)
			obj_name <- "victimized"
		}

		# Compute means, their CI, and number of non-missing observations by variable
		vars_to_compute <- names(temp_obj)
		temp_summarystat <- rbindlist(
			lapply(vars_to_compute, function(x) return_weighted_mean_se(temp_obj_svy, x)),
			fill = TRUE
		)

		# Create 95% CI strings and prepare for the output
		vars_no_digits <- ""
		temp_summarystat[
			!is.na(cilower) & !(variable %in% vars_no_digits),
			cistring := paste0("(", round(cilower, 3), "-", round(ciupper, 3), ")")
		]
		temp_summarystat[
			!is.na(cilower) & variable %in% vars_no_digits,
			cistring := paste0("(", round(cilower, 0), "-", round(ciupper, 0), ")")
		]
		temp_summarystat[!(variable %in% vars_no_digits), meanvalue := round(meanvalue, 3)]
		temp_summarystat[variable %in% vars_no_digits, meanvalue := round(meanvalue, 0)]

		# Prepare output-ready object
		temp_summarystat[, survey := obj_name]
		temp_summarystat <- temp_summarystat[, c("survey", "variable", "nobs", "meanvalue", "cistring")]
		survey_summary_stat_long <- rbind(survey_summary_stat_long, temp_summarystat, fill = TRUE)
	}
}

# To wide object
survey_summary_stat <- dcast(
	survey_summary_stat_long, variable ~ survey,
	value.var = c("nobs", "meanvalue", "cistring")
)

# Proper order of columns
setcolorder(
	survey_summary_stat,
	c(
		"variable", names(survey_summary_stat)[grepl("rcvs2021", names(survey_summary_stat))],
		c(
			"nobs_not-victimized", "meanvalue_not-victimized", "cistring_not-victimized", "nobs_victimized",
			"meanvalue_victimized", "cistring_victimized"
		),
		names(survey_summary_stat)[grepl("kouzh", names(survey_summary_stat))]
	)
)

# Rename the variables
replacement_dict <- c(
	"age" = "Age, years",
	"agegroup_18-34" = "18-34 yrs, %",
	"agegroup_35-49" = "35-49 yrs, %",
	"agegroup_50-64" = "50-64 yrs, %",
	"agegroup_65+" = "65+ yrs, %",
	"crimetype_Assault" = "Assault",
	"rimetype_Fraud" = "Fraud",
	"crimetype_Theft" = "Theft",
	"crimetype_Larceny" = "Petty theft",
	"crimetype_Other" = "Other crimes",
	"crimetype_Fraud" = "Fraud",
	"crimetype_Wire Fraud/Cybercrime" = "Cyber crime",
	"crimetype_Attempted Wire Fraud/Cybercrime" = "Attempt of cyber crime",
	"crimetype_Robbery" = "Robbery",
	"crimetype_Theft" = "Theft",
	"education_level_1" = "High school and less",
	"education_level_2" = "Associate degree",
	"education_level_3" = "Some Bachelors and higher",
	"fears_becoming_victim" = "Fears victimization",
	"household_size" = "Household size, persons",
	"income_level_1" = "Not enough for food, %",
	"income_level_2" = "Not enough for clothes, %",
	"income_level_3" = "Not enough for gadgets and furniture, %",
	"income_level_4" = "Just enough for gadgets, %",
	"income_level_5" = "Enough for a car, %",
	"income_level_6" = "Enough for a house, %",
	"is_rural" = "Rural residence",
	"is_unsafe_alone" = "Feels unsafe at night",
	"is_unsafe_at_home" = "Fears burglary",
	"lives_alone" = "Living alone",
	"male" = "Male",
	"married" = "Married",
	"mean_household_income" =
		"Monthly per capita household income, thou. rubles",
	"life_satisfaction" = "Life satisfaction (1 to 10)",
	"unemployed" = "Unemployed",
	"locality_avg_income" =
		"Local monthly average per capita income, thou. rubles",
	"locality_population" = "Local population, thou. people",
	"victimized12m" = "Victimized during last 12 months",
	"victimized5y" = "Victimized during last 5 years"
)

for (old in names(replacement_dict)) {
	survey_summary_stat[variable == old, variable := replacement_dict[old]]
}

# Reorder rows properly
survey_summary_stat[, index := .I]
survey_summary_stat <- survey_summary_stat[
	match(
		c(32, 1, 14:16, 29, 33, 18, 36, 25, 31, 30, 34, 28, 38, 37, 17, 26, 27, 6, 11, 12, 9, 8, 13, 7, 10),
		index
	)
]
survey_summary_stat[, index := NULL]

# Produce a table with summary statistics for further manual editing
quiet_stargazer(survey_summary_stat, type = "latex", summary = FALSE, out = "tables/survey_summary_stat.tex")


## Compute raking weights for RCVS-2021 in relation to the KOUZh-2020
# Numeric variables to factors for compatibility with anesrake
to_factor <- c("male", "lives_alone", "is_rural", "unemployed", "married")
rcvs2021_crimecost[, c(to_factor) := lapply(.SD, as.factor), .SDcols = to_factor]

# Drop unused levels
rcvs2021_crimecost[, c(to_factor) := lapply(.SD, droplevels), .SDcols = to_factor]

# Define raking variables
raking_variables <- c("agegroup", "male", "lives_alone", "married", "education_level", "unemployed", "is_rural")

## Generate target proportions (from the weighted survey)
# Age group
agegroup_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("agegroup", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
agegroup_proportions[, variable := gsub("agegroup_", "", variable)]
agegroup_proportions_list <- agegroup_proportions$meanvalue
names(agegroup_proportions_list) <- agegroup_proportions$variable

# Sex
male_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("male", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
male_proportions[, variable := 1]
male_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - male_proportions$meanvalue), male_proportions)
male_proportions_list <- male_proportions$meanvalue
names(male_proportions_list) <- male_proportions$variable

# Marital status
married_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("married", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
married_proportions[, variable := 1]
married_proportions <- rbind(
	data.table(variable = 0, meanvalue = 1 - married_proportions$meanvalue),
	married_proportions
)
married_proportions_list <- married_proportions$meanvalue
names(married_proportions_list) <- married_proportions$variable

# Cohabitation
lives_alone_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("lives_alone", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
lives_alone_proportions[, variable := 1]
lives_alone_proportions <- rbind(
	data.table(variable = 0, meanvalue = 1 - lives_alone_proportions$meanvalue),
	lives_alone_proportions
)
lives_alone_proportions_list <- lives_alone_proportions$meanvalue
names(lives_alone_proportions_list) <- lives_alone_proportions$variable

# Education level
educ_level_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("education_level", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
educ_level_proportions[, variable := gsub("education_level_", "", variable)]
educ_level_proportions_list <- educ_level_proportions$meanvalue
names(educ_level_proportions_list) <- educ_level_proportions$variable

# Employment status
unemp_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("unemployed", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
unemp_proportions[, variable := 1]
unemp_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - unemp_proportions$meanvalue), unemp_proportions)
unemp_proportions_list <- unemp_proportions$meanvalue
names(unemp_proportions_list) <- unemp_proportions$variable

# Locality
is_rural_proportions <- survey_summary_stat_long[
	survey == "kouzh_2020_data" & grepl("is_rural", variable),
	c("variable", "meanvalue"),
	with = FALSE
]
is_rural_proportions[, variable := 1]
is_rural_proportions <- rbind(
	data.table(variable = 0, meanvalue = 1 - is_rural_proportions$meanvalue),
	is_rural_proportions
)
is_rural_proportions_list <- is_rural_proportions$meanvalue
names(is_rural_proportions_list) <- is_rural_proportions$variable

# Define a list with raking targets
raking_target <- list(
	"male" = male_proportions_list,
	"agegroup" = agegroup_proportions_list,
	"education_level" = educ_level_proportions_list,
	"married" = married_proportions_list,
	"lives_alone" = lives_alone_proportions_list,
	"unemployed" = unemp_proportions_list,
	"is_rural" = is_rural_proportions_list
)

# Perform raking
rcvs2021_raking_fit <- anesrake(
	inputter = raking_target,
	dataframe = rcvs2021_crimecost[, names(raking_target), with = FALSE],
	caseid = rcvs2021_crimecost$ID, cap = 5, choosemethod = "total",
	type = "pctlim", pctlim = 0.05
)

# Save the object with raking
save(rcvs2021_raking_fit, file = "data/rcvs2021_raking_fit.rdata", compress = "gzip")
