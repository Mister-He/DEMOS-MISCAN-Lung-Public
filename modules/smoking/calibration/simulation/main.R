# ==============================================================================#
# Preparation
# ==============================================================================#
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(jsonlite)
library(MASS)

# Clear the environment
rm(list = ls())
gc()

# ==============================================================================#
# Configuration
# ==============================================================================#
if (!endsWith(getwd(), "modules/smoking/calibration/simulation")) {
  setwd("modules/smoking/calibration/simulation")
}
# Paths settings
data_path <- "../data/"
params_path <- "../params/"
outputs_path <- "../outputs/"
log_path <- paste0(outputs_path, "logs/")

# Read the strategy index & num_seed from command line
params <- jsonlite::fromJSON(paste0(params_path, "config.json"))

# Configuration
num_seed <- params$configuration$`num_seed`
yearstop <- params$configuration$`yearstop`

# Simulation
startrow <- params$simulation$`start_row`
conditionnumber <- params$simulation$`condition_number`
t_age <- params$simulation$`trunc_age`

# ==============================================================================#
# Simulation
# ==============================================================================#
start <- Sys.time()

# Set the seed run for this
set.seed(num_seed)

# Load scenario-specific data/functions
source("load.R")

# Add race and gender to the baseline population with mapping as the reference
population <- totpopulation %>% left_join(mapping, by = c("race", "gender"))

population$year_immigrated[is.na(population$year_immigrated)] <- 0
yearborn <- population$year_of_birth
yearimmigrated <- as.integer(population$year_immigrated)
index <- population$index
beforeconditions <- NULL

rm(totpopulation, immigrants)
gc()

cat("Done preparation, start simulation!\n")

while (startrow < nrow(population)) {
  # Schedule conditions for current population
  afterconditions <- schedule_population_pre2025(
    ncol = conditionnumber,
    begin = startrow,
    end = nrow(population),
    year_stop = yearstop,
    trunc_age = t_age
  )

  beforeconditions <- rbind(beforeconditions, afterconditions)
  if (is.null(birthmatrix) || sum(birthmatrix) == 0) break

  # Add new births to population
  startrow <- nrow(population)
  population <- addpopulation(birthmatrix, yearstop) %>% rbind(population, .)
  yearborn <- population$year_of_birth
  yearimmigrated <- population$year_immigrated
  index <- population$index

  rm(birthmatrix)
  gc()
}

rm(afterconditions, yearborn, yearimmigrated, index)
gc()

cat("Finish simulation!\n")

beforeconditions <- as.data.frame(beforeconditions)
colnames(beforeconditions) <- c(
  "year_of_death", "year_start_smoking", "year_stop_smoking"
)

populationdata <- cbind(population, beforeconditions)
rm(population, beforeconditions)
gc()

saveRDS(populationdata, "../data/baseline_pop.rds")
