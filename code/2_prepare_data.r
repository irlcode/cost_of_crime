library(data.table)
library(fastDummies)

options(scipen = 999)
set.seed(42)

# Get project path
setwd(Sys.getenv("CRIMECOST_PATH"))
source("code/helper_functions/utils.r")

# Create directories if not exist
dir.create("media", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("estimates", showWarnings = FALSE)

# Load the prepared RCVS-2021 data
load("data/rcvs2021_crimecost.rdata")

# Load the raking weights produced by 1_rcvs_vs_domestic_household_surveys.r
load("data/rcvs2021_raking_fit.rdata")
raking_weights <- data.table(ID = names(rcvs2021_raking_fit$weightvec), raking_weight = rcvs2021_raking_fit$weightvec)

# Add the raking weights to the data
rcvs2021_crimecost <- merge(rcvs2021_crimecost, raking_weights, by = "ID", all.x = TRUE, all.y = FALSE, sort = FALSE)

# Region to factor
rcvs2021_crimecost[, region := as.factor(region)]

# Factor to its numeric representation and back to factor (we will need this for derivative computation)
rcvs2021_crimecost[, region := as.factor(as.numeric(region))]

# Life satisfaction and income to log
vars_to_log <- c("life_satisfaction", "mean_household_income", "locality_avg_income", "locality_population")
for (var in vars_to_log) {
	rcvs2021_crimecost[, paste0("ln", var) := log(get(var))]
}

# Squares of certain variables
vars_to_square <- c("age", "household_size", "locality_population", "locality_avg_income")
for (var in vars_to_square) {
	rcvs2021_crimecost[, paste0(var, "_sq") := get(var)^2]
}

## Crime type to dummies for those victimized in the last 12 months
temp <- dummy_cols(
	rcvs2021_crimecost[, c("crimetype", "victimized12m"), with = FALSE],
	remove_first_dummy = FALSE, remove_most_frequent_dummy = FALSE, ignore_na = TRUE, remove_selected_columns = TRUE
)
names(temp) <- tolower(gsub(" ", "", gsub("crimetype_", "", names(temp))))
names(temp) <- gsub("wirefraud/cybercrime", "remote", names(temp), fixed = TRUE)

# Fill in zeros for non-victimised
temp[victimized12m == 0, 2:ncol(temp) := 0]
temp[, victimized12m := NULL]

# Add to the data
rcvs2021_crimecost <- cbind(rcvs2021_crimecost, temp)
save(rcvs2021_crimecost, file = "data/rcvs2021_crimecost_processed.rdata", compress = "gzip")
