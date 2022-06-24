# Get project path
setwd(Sys.getenv("CRIMECOST_PATH"))

# Read in the file with dependencies
packages <- readLines(con <- file("DEPENDENCIES", encoding = "UTF-8"))
packages <- gsub("^\\s+", "", packages[2:length(packages)])
packages <- gsub("\\s+.*$", "", packages)

# Install the uninstalled packages
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
    # Install the latest versions of all the packages
    install.packages(missing_packages, quiet = TRUE)
}


## In R >=4.2.0, stargazer <= 5.2.3 returns an error for objects with multiple models with long names.
## Below is a quick fix for this.
# Delete stargazer
remove.packages("stargazer")
# Download the source
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
# Save back
writeLines(stargazer_src, con = "stargazer/R/stargazer-internal.R")
# Compile and install the patched package
install.packages("stargazer", repos = NULL, type = "source")
