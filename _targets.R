library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "bookdown"))

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/functions.R")


data_targets <- tar_plan(
  data =  data.frame(x = sample.int(100), y = sample.int(100)),
  summary = summ(data) # Call your custom functions as needed.
)



# Targets necessary to build the book / article
book_targets <- tar_plan(
  report = bookdown::render_book(input = ".", output_yaml = "_output.yml", 
                                 config_file = "_bookdown.yml")
)



# run all targets
tar_plan(
  data = data_targets, 
  book = book_targets
)
