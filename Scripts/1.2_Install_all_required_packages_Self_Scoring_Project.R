## Install all required packages

list_of_packages <- c(
  "rmdformats", # for rendering the rMarkdown layout
  "tidyverse",  # distinct, filter, %>%, and many more
  "data.table", # for fread (loading .txt files)
  "Hmisc",      # for computing gamma correlations
  "ggrepel",    # for ggplots with labelled points
  "lme4",       # for ML models
  "lmerTest",   # for p values with lmer
  "sjstats",    # partial eta squared and cohens effect size in repeated masures ML
  "sjPlot",     # for printing lmer HTML tables
  "emmeans",    # for pairwise comparisons and effect sizes etc.
  "BayesFactor",# for Baysian analysis
  "DescTools", # for GoodmanKruskalGamma correlations
  "stringi", # for self-scoring evaluation
  "kableExtra",  # for formatted table outout with kable
  "lubridate",  # for converting times
  "readxl", # for reading excel files for the inter-rater reliabilies
  "hms",     # for converting strings to time
  "irr", # for computing inter rater reliabilities
  "xlsx", # for writing excel files
  "rsq",        # for retrieving conditional R squared of lmer models
  "pbkrtest"   # for "kenward-roger" mode when using emmeans
) 

# List of relevant packages
new_packages <- list_of_packages[ # Check whether all packages are installed
  !(list_of_packages %in% installed.packages()[, "Package"])]
# Install them if that is not the case
if (length(new_packages)) install.packages(new_packages) 
# Load the relevant libraries
sapply(list_of_packages, require, character.only = TRUE)

## Ensure that all packages are loaded in a version that was last checked with this script
# (If you run this for the first time, this may take a couple of minutes)
# For more information, see https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html
checkpoint("2023-07-14")
