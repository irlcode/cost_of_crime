library(data.table)
library(stringi)
library(stringr)
library(survey)
library(lubridate)
library(fastDummies)
library(anesrake)
library(stargazer)

options(scipen = 999)

# Declare working directory beforehand in an environment variable
# CRIMECOST_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to make effect
setwd(Sys.getenv('CRIMECOST_PATH'))

# Create directory /tables if not exist
dir.create("tables", showWarnings = FALSE)

# Load KOUZh-2020 data prepared by 0a_prepare_kouzh_data.r
load("data/kouzh_2020_data.rdata")

# Load RCVS-2021 data prepared by 0b_prepare_rcvs_data_for_analysis.r
load("data/rcvs2021_crimecost.rdata")

#################
# Compute the summary statistics
# Define the summary stat variables to include in the table
summary_stat_vars <- c("male", "age", "agegroup", "education_level", "lives_alone", "married", "household_size", "unemployed", "is_rural", "locality_population", "locality_avg_income", "mean_household_income", "income_level", "life_satisfaction", "fears_becoming_victim", "is_unsafe_alone", "is_unsafe_at_home", "victimized12m", "victimized5y", "crimetype")

# Function to return mean and CI for the variable in the
# given survey object
# USAGE:	kouzh_2020_data_svy <- survey::svydesign(ids = ~1, data = kouzh_2020_data, weights = ~survey_weight)
#			return_weighted_mean_se(kouzh_2020_data_svy, "age")
return_weighted_mean_se <- function(survey_obj, variable) {

	mean_obj <- svymean(~get(variable), survey_obj, na.rm = T)
	cis <- as.numeric(confint(mean_obj))
	names(cis) <- c("cilower", "ciupper")
	nobs <- sum(!is.na(survey_obj$variables[[variable]]))

	out <- data.table(variable = variable, meanvalue = as.numeric(mean_obj), t(cis), nobs = nobs)
	return(out)

}

# Objects of interest (to compute the summary stat
objs_of_interest <- c("rcvs2021_crimecost", "kouzh_2020_data")

# Init a data.table to store the results (for all observations)
survey_summary_stat_long <- data.table()

for(obj in objs_of_interest) {
	
	# Create a new object with selected variables
	# and expand their dummies
	temp_obj <- get(obj)[, intersect(names(get(obj)), c(summary_stat_vars, "survey_weight")), with = F]
	temp_obj <- dummy_cols(temp_obj, remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)
	
	# Set same weight for RCVS
	if( !grepl("kouzh", obj) ) {

		temp_obj[, survey_weight := 1 ]

	}

	# We need to repeat the below logic two more
	# times on a subset of victimised and non-victimised
	# individuals for the RCVS data. NB: this is
	# a very inelegant and hard-coded way to do it.
	subset_groups <- 1:ifelse(grepl("rcvs", obj), 3, 1)

	for( j in subset_groups ) {

		# Create a survey object from the source data
		# with or without weights depending on it being KOUZh
		temp_obj_svy <- survey::svydesign(ids = ~1, data = temp_obj, weights = ~survey_weight)
	
		obj_name <- obj

		# Next, we apply subsetting if it is RCVS
		# and we are at a certain iteration
		if ( j == 2 ) {

			temp_obj_svy <- subset(temp_obj_svy, victimized5y == 0 & victimized12m == 0 )
			obj_name <- "not-victimized"

		} else if ( j == 3 ) {
			
			temp_obj_svy <- subset(temp_obj_svy, victimized5y == 1 | victimized12m == 1 )
			obj_name <- "victimized"
			
		}

		# Compute means, their CI, and number of non-missing observations
		# by variable
		vars_to_compute <- names(temp_obj)
		
		temp_summarystat <- rbindlist(lapply(vars_to_compute, function(x) { return_weighted_mean_se(temp_obj_svy, x) }), fill = T)
	
		# Create 95% CI strings and prepare for the output
		#vars_no_digits <- c("locality_population", "locality_avg_income", "mean_household_income")
		vars_no_digits <- ""
		temp_summarystat[!is.na(cilower) & !( variable %in% vars_no_digits ), cistring := paste0("(", round(cilower, 3), "-", round(ciupper, 3), ")") ]
		temp_summarystat[!is.na(cilower) & variable %in% vars_no_digits, cistring := paste0("(", round(cilower, 0), "-", round(ciupper, 0), ")") ]
	
		temp_summarystat[!( variable %in% vars_no_digits ), meanvalue := round(meanvalue, 3)]
		temp_summarystat[ variable %in% vars_no_digits, meanvalue := round(meanvalue, 0)]
	
		# Prepare output-ready object
		temp_summarystat[, survey := obj_name ]
		temp_summarystat <- temp_summarystat[, c("survey", "variable", "nobs", "meanvalue", "cistring")]
		
		survey_summary_stat_long <- rbind(survey_summary_stat_long, temp_summarystat, fill = T) 

	}
	
}

# To wide object
survey_summary_stat <- dcast(survey_summary_stat_long, variable ~ survey, value.var = c("nobs", "meanvalue", "cistring"))

# Proper order of columns
setcolorder(survey_summary_stat, c("variable", names(survey_summary_stat)[grepl("rcvs2021", names(survey_summary_stat))],  c("nobs_not-victimized", "meanvalue_not-victimized", "cistring_not-victimized", "nobs_victimized", "meanvalue_victimized", "cistring_victimized"), names(survey_summary_stat)[grepl("kouzh", names(survey_summary_stat))]))

# Rename the varibles
survey_summary_stat[variable == "age",variable := "Age, years"]
survey_summary_stat[variable == "agegroup_18-34",variable := "18-34 yrs, %"]
survey_summary_stat[variable == "agegroup_35-49",variable := "35-49 yrs, %"]
survey_summary_stat[variable == "agegroup_50-64",variable := "50-64 yrs, %"]
survey_summary_stat[variable == "agegroup_65+",variable := "65+ yrs, %"]
survey_summary_stat[variable == "crimetype_Assault",variable := "Assault"]
survey_summary_stat[variable == "rimetype_Fraud",variable := "Fraud"]
survey_summary_stat[variable == "crimetype_Theft",variable := "Theft"]
survey_summary_stat[variable == "crimetype_Larceny",variable := "Petty theft"]
survey_summary_stat[variable == "crimetype_Other",variable := "Other crimes"]
survey_summary_stat[variable == "crimetype_Fraud",variable := "Fraud"]
survey_summary_stat[variable == "crimetype_Wire Fraud/Cybercrime",variable := "Cyber crime"]
survey_summary_stat[variable == "crimetype_Attempted Wire Fraud/Cybercrime",variable := "Attempt of cyber crime"]
survey_summary_stat[variable == "crimetype_Robbery",variable := "Robbery"]
survey_summary_stat[variable == "crimetype_Theft",variable := "Theft"]
survey_summary_stat[variable == "education_level_1",variable := "High school and less"]
survey_summary_stat[variable == "education_level_2",variable := "Associate degree"]
survey_summary_stat[variable == "education_level_3",variable := "Some Bachelors and higher"]
survey_summary_stat[variable == "fears_becoming_victim",variable := "Fears victimization"]
survey_summary_stat[variable == "household_size",variable := "Household size, persons"]
survey_summary_stat[variable == "income_level_1",variable := "Not enough for food, %"]
survey_summary_stat[variable == "income_level_2",variable := "Not enough for clothes, %"]
survey_summary_stat[variable == "income_level_3",variable := "Not enough for gadgets and furniture, %"]
survey_summary_stat[variable == "income_level_4",variable := "Just enough for gadgets, %"]
survey_summary_stat[variable == "income_level_5",variable := "Enough for a car, %"]
survey_summary_stat[variable == "income_level_6",variable := "Enough for a house, %"]
survey_summary_stat[variable == "is_rural",variable := "Rural residence"]
survey_summary_stat[variable == "is_unsafe_alone",variable := "Feels unsafe at night"]
survey_summary_stat[variable == "is_unsafe_at_home",variable := "Fears burglary"]
survey_summary_stat[variable == "lives_alone",variable := "Living alone"]
survey_summary_stat[variable == "male",variable := "Male"]
survey_summary_stat[variable == "married",variable := "Married"]
survey_summary_stat[variable == "mean_household_income",variable := "Monthly per capita household income, thou. rubles"]
survey_summary_stat[variable == "life_satisfaction",variable := "Life satisfaction (1 to 10)"]
survey_summary_stat[variable == "unemployed",variable := "Unemployed"]
survey_summary_stat[variable == "locality_avg_income",variable := "Local monthly average per capita income, thou. rubles"]
survey_summary_stat[variable == "locality_population",variable := "Local population, thou. people"]
survey_summary_stat[variable == "victimized12m",variable := "Victimized during last 12 months"]
survey_summary_stat[variable == "victimized5y",variable := "Victimized during last 5 years"]

# Reorder rows properly
survey_summary_stat[, index := .I]
survey_summary_stat <- survey_summary_stat[match(c(32, 1:5, 14:16, 29, 33, 18, 35, 25, 31, 30, 34, 19:24, 28, 37, 36, 17, 26, 27, 6, 11, 12, 9, 8, 13, 7, 10), index)]
survey_summary_stat[, index := NULL]

# Produce a table with summary statistics for further manual editing
# NB: there are encoding errors on Windows, use this hack instead
temp_out <- stargazer(survey_summary_stat, type = "latex", summary = F, out = "tables/survey_summary_stat.tex")
cat(temp_out, file = "tables/survey_summary_stat.tex", rownames = F, sep = "\n")


#################
# Compute raking weights for RCVS-2021 in relation
# to the KOUZh-2020

# Numeric variables to factors for compatibility with
# anesrake
to_factor <- c("male", "lives_alone", "is_rural", "unemployed", "married")
rcvs2021_crimecost[, c(to_factor) := lapply(.SD, as.factor), .SDcols = to_factor]

# Drop unused levels
rcvs2021_crimecost[, c(to_factor) := lapply(.SD, droplevels), .SDcols = to_factor]

# Define raking variables
raking_variables <- c("agegroup", "male", "lives_alone", "married", "education_level", "unemployed", "is_rural")

# Generate target proportions (from the weighted survey)
agegroup_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("agegroup", variable), c("variable", "meanvalue"), with = F]
agegroup_proportions[, variable := gsub("agegroup_", "", variable)]
agegroup_proportions_list <- agegroup_proportions$meanvalue
names(agegroup_proportions_list) <- agegroup_proportions$variable

male_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("male", variable), c("variable", "meanvalue"), with = F]
male_proportions[, variable := 1 ]
male_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - male_proportions$meanvalue), male_proportions)
male_proportions_list <- male_proportions$meanvalue
names(male_proportions_list) <- male_proportions$variable

married_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("married", variable), c("variable", "meanvalue"), with = F]
married_proportions[, variable := 1 ]
married_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - married_proportions$meanvalue), married_proportions)
married_proportions_list <- married_proportions$meanvalue
names(married_proportions_list) <- married_proportions$variable

lives_alone_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("lives_alone", variable), c("variable", "meanvalue"), with = F]
lives_alone_proportions[, variable := 1 ]
lives_alone_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - lives_alone_proportions$meanvalue), lives_alone_proportions)
lives_alone_proportions_list <- lives_alone_proportions$meanvalue
names(lives_alone_proportions_list) <- lives_alone_proportions$variable

education_level_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("education_level", variable), c("variable", "meanvalue"), with = F]
education_level_proportions[, variable := gsub("education_level_", "", variable)]
education_level_proportions_list <- education_level_proportions$meanvalue
names(education_level_proportions_list) <- education_level_proportions$variable

unemployed_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("unemployed", variable), c("variable", "meanvalue"), with = F]
unemployed_proportions[, variable := 1 ]
unemployed_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - unemployed_proportions$meanvalue), unemployed_proportions)
unemployed_proportions_list <- unemployed_proportions$meanvalue
names(unemployed_proportions_list) <- unemployed_proportions$variable

is_rural_proportions <- survey_summary_stat_long[ survey == "kouzh_2020_data" & grepl("is_rural", variable), c("variable", "meanvalue"), with = F]
is_rural_proportions[, variable := 1 ]
is_rural_proportions <- rbind(data.table(variable = 0, meanvalue = 1 - is_rural_proportions$meanvalue), is_rural_proportions)
is_rural_proportions_list <- is_rural_proportions$meanvalue
names(is_rural_proportions_list) <- is_rural_proportions$variable

# Define a list with raking targets
raking_target <- list( "male" = male_proportions_list, "agegroup" = agegroup_proportions_list, "education_level" = education_level_proportions_list, "married" = married_proportions_list, "lives_alone" = lives_alone_proportions_list, "unemployed" = unemployed_proportions_list, "is_rural" = is_rural_proportions_list)

# Perform raking
rcvs2021_raking_fit <- anesrake(inputter = raking_target, dataframe = rcvs2021_crimecost[, names(raking_target), with = F], caseid = rcvs2021_crimecost$ID, cap = 5, choosemethod = "total", type = "pctlim", pctlim = 0.05)

# Save the object with raking
save(rcvs2021_raking_fit, file = "data/rcvs2021_raking_fit.rdata", compress = "gzip")
