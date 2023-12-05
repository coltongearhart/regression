### ---- Set knitr options ----

# set knitr options
knitr::opts_chunk$set(
  cache = FALSE, # cache output
  error = TRUE, # continue code evaluation even if there is an error, in which case the error messages will print
  comment = '', # remove ## prefix for printed text output
  message = FALSE, # suppress printing of messages in the output document
  warning = FALSE, # suppress printing of warnings in the output document
  fig.pos = "hold" # hold figure position so it stays in the order it should be in relative to the surrounding code / text
)

# disable scientific notation for readability purposes
options(scipen = 999)

### ---- Load packages and define functions ----

# load packages for common functions
library(tidyverse)
library(magrittr)
library(kableExtra)

# default function to display dataframes nicely
# options -> vector of column names (gets processed by kable(), so can be latex), number of rows to display, and rounding digits
# -> needed because formatting of raw dataframe is bad when output in markdown
# -> nesting functions instead of piping so doesn't require magrittr
display_nice <- function(df, col.names = NA, nrow = 10, digits = 3) {
  
  # set columns names to the given vector or keep names or original df
  if (identical(col.names, NA)) {
    col.names = colnames(df)
  }
  
  # convert to knitr_kable and style
  # -> always want html format, left aligned with not full width
  # -> table.attr -> have to tell quarto to not process the table (https://github.com/quarto-dev/quarto-cli/issues/5737)
  kable_styling(kable(head(df, n = nrow),
                      col.names = col.names,
                      format = "html",
                      digits = digits,
                      table.attr = 'data-quarto-disable-processing="true"'),
                bootstrap_options = "striped",
                full_width = FALSE,
                position = "left")
}

# set global plot theme
theme_set(theme_bw())

# function to compare results of same thing from different methods
# -> default precision is out to five decimal places
# -> the option for returning results is just to have an easy way to visually compare actual results by saving them in same object
# R NOTE: only works for comparing numeric items
compare <- function(item_1, item_2, return_items = TRUE, decimals = 5) {
  
  # conditionally add the original items being compared to the results
  if (return_items) {
    
    # initialize a list with helpful names for the results and original items being compared
    # -> the extra functions for the names just return the name of the original arguments passed to item_1 and item_2
    results <- vector(mode = "list", length = 3)
    names(results) <- c("comparison", deparse(substitute(item_1)), deparse(substitute(item_2)))
    
    # fill results
    results[[1]] <- round(item_1, decimals) == round(item_2, decimals)
    results[[2]] <- item_1
    results[[3]] <- item_2
  }
  # just return the comparison
  else {
    results <- round(item_1, decimals) == round(item_2, decimals)
  }
  return(results)
}
