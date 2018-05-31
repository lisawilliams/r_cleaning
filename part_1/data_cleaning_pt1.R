###############################################
## Data Cleaning with R (Pt. I)              ##
## Alex Richards, NerdWallet (@alexrichards) ##
###############################################

# Much credit for this cleaning exercise goes to Sandhya Kambhampati (@sandhya__k)
# and Caelainn Barr (@caelainnbarr).

###########
## Intro ##
###########

# - What is data cleaning?
# - What do you need to be able to do to clean data?
# - Why R?

###########################
## Install/load packages ##
###########################

# If you don't have them already! In RStudio, check the "packages" pane.
install.packages("tidyverse") # https://www.tidyverse.org/packages/
install.packages("rvest")

# Load them for use.
library(tidyverse)
library(rvest)

# Check/set working directory

#########################
## Getting data into R ##
#########################

# 1. Delimited files
banks <- read_csv("banklist.csv")

# What to do if the readr package guesses the wrong data type?
bank_spec <- cols(
  CERT = col_character()
  )

# 2. Excel
library(readxl)
same_banks <- read_xlsx('failed_banks.xlsx')

# 3. HTML

# Pull the HTML of a web page into R
# http://espn.go.com/nfl/superbowl/history/winners
sb_page <- read_html('http://espn.go.com/nfl/superbowl/history/winners')

# Let's look at the page in a web browser's developer view.

# Isolate parts of the page that fall within <table> tags
page_tables <- html_nodes(sb_page, 'table')

# Move the table into a data frame
sb_table <- html_table(page_tables[[1]])

############################
## Tweaking the structure ##
############################

# Which rows should be dropped? We can access parts of the data frame by [row, column]
# Pipe it to a View() in RStudio if you want to see how it looks first
sb_table[-(1:2),] %>% View()

# Modify sb_table to drop the necessary rows
sb_table <- sb_table[-(1:2),]

# Check the column names
names(sb_table)

# Update them to be a bit more descriptive
names(sb_table) <- c("numeral", "date", "location", "result")

#######################
## Cleaning the data ##
#######################

# Add a column with Superbowl number as an integer
sb_table$number <- c(1:52)

# Why keep the original?

# Reorder your columns so that "number" appears first (name OR column number works)
sb_table <- sb_table[, c("number", "numeral", "date", "location", "result")]

# R is treating the date as a string; let's fix
# ?as.Date to review formats
sb_table$date <- as.Date(sb_table$date, "%b. %d, %Y")

# Give year its own column using date formats
sb_table$year <- format(sb_table$date, "%Y")

# Let's do the same with month but extract a section of the date
sb_table$month <- substr(sb_table$date, 6, 7)

# Split result into two separate fields based on a delimiter (check first?)
sb_table <- separate(sb_table, result, c("winner", "loser"), sep = ", ", remove = FALSE)

# How to best move scores into their own columns?

# Regular expressions can help when data follows a pattern.
# https://regex101.com/
score_pattern <- "\\d+$"

# Test to see if the regex extraction pattern works
str_extract(sb_table$winner, score_pattern)

# The same pattern can be replaced with an empty string to just have team names
str_replace(sb_table$winner, score_pattern, "")

# We end up with a hanging space, but can add an operation to trim this off
str_replace(sb_table$winner, score_pattern, "") %>% str_trim(side = "right")

# Stitch these steps together to add score columns and remove score from teams
sb_table$winner_score <- str_extract(sb_table$winner, score_pattern)
sb_table$loser_score <- str_extract(sb_table$loser, score_pattern)
sb_table$winner <- str_replace(sb_table$winner, score_pattern, "") %>% str_trim(side = "right")
sb_table$loser <- str_replace(sb_table$loser, score_pattern, "") %>% str_trim(side = "right")

# Switch scores to a numeric type
sb_table$winner_score <- as.numeric(sb_table$winner_score)
sb_table$loser_score <- as.numeric(sb_table$loser_score)

# Did ESPN always put the winners first?
sb_table[sb_table$winner_score <= sb_table$loser_score,]

# Reorder and drop some unnecessary fields
sb_table <- sb_table[, c("number", "numeral", "date", "year", "month", "location", "winner", "winner_score", "loser", "loser_score")]

# Export a CSV copy
write_csv(sb_table, "sb_data.csv")

# What's the advantage in R over a traditional GUI like Excel?

# Notice anything else we could change?
