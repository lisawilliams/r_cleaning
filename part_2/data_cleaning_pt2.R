###############################################
## Data Cleaning with R (Pt. II)             ##
## Alex Richards, NerdWallet (@alexrichards) ##
###############################################

# With thanks to Christine Zhang (@christinezhang) for her Boston employees example.

##############################
## Cleaning with conditions ##
##############################

# We'll need functions from the tidyverse package, for starters
library(tidyverse)

# Let's try to read a CSV of Boston employee salaries
empl <- read_csv('employee-earnings-report-2017.csv')

# Did we get errors/warnings? What did they mean?

# Giving the import process a little guidance
empl_spec <- cols(
  POSTAL = col_character()
)
empl <- read_csv('employee-earnings-report-2017.csv', col_types = empl_spec)

# Some immediate issues we can identify with these data?

# These data are too big to eyeball; we need a query to get a better handle on POSTAL
empl %>% group_by(str_length(POSTAL)) %>%
  summarise(n())

# SQL equivalent:

# SELECT CHAR_LENGTH(POSTAL), COUNT(*)
# FROM EMPL
# GROUP BY 1

# Also, same code w/o piping: summarize(group_by(empl, str_length(POSTAL)), n())
# Hard to write, harder to read

# We can filter these records based on POSTAL length and look at them
empl %>% filter(str_length(POSTAL) == 3) %>% View()
empl %>% filter(str_length(POSTAL) == 5) %>% View()

# We can set up a cleaned field with a conditional
empl$postal_fix <- if_else(str_length(empl$POSTAL) == 4, paste0('0',empl$POSTAL), empl$POSTAL)

# Same for the postal codes that lost even more zeros, with a few key differences
empl$postal_fix <- if_else(str_length(empl$POSTAL) == 3, paste0('00',empl$POSTAL), empl$postal_fix)

# This leaves a few straggling NAs to tangle with
empl %>% filter(is.na(POSTAL)) %>% View()
empl$postal_fix <- if_else(is.na(empl$POSTAL), 'UNKNO', empl$postal_fix)

# We can stash the full file elsewhere and keep a version with key data columns
empl_complete <- empl
empl <- empl %>% select(NAME, `DEPARTMENT NAME`, TITLE, `TOTAL EARNINGS`, postal_fix)

# We can also reorder our data frame
empl %>% arrange(`TOTAL EARNINGS`) %>% View()

# Check the column's data type to see why that didn't work
class(empl$`TOTAL EARNINGS`)

# Possible solutions? How about altering the data type?

# One option: all at once with a regex pattern similar to how we dealt with
# isolating a removing team scores
earn_pattern <- "\\$|,"
empl$ttl_earn <- str_replace_all(empl$`TOTAL EARNINGS`, earn_pattern, "")

# Let's try sorting this again
empl %>% arrange(ttl_earn) %>% View()

# R is still treating it as a character field by default
empl$ttl_earn <- as.numeric(empl$ttl_earn)

# Let's sort by ttl_earn, in descending order, and copy that to its own variable
empl_sorted <- empl %>% arrange(desc(ttl_earn))

# Most straightforward method with the tidyverse parse_number function
empl$ttl_earn <- parse_number(empl$`TOTAL EARNINGS`)

# Let's load a file of CFPB complaints related to student loans
comp <- read_csv('student_loan_complaints.csv')

# How clean are the company names?
comp_summ <- comp %>% group_by(company_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#################################
## Standardizing freeform text ##
#################################

# The task at hand is to create a standardized set of company names (a lookup table)
# that can be linked back to the original version.

# What are the advantages of handling this programmatically in R?

# Create a new data frame with a distinct list of company names
companies <- comp %>% distinct(company_name) %>%
  arrange(company_name)

# Add a placeholder column with empty strings
companies$cleaned_name <- ''

# There are a couple of ways to deal with this if we need something reproducable:
# 1. Fill a counterpart "clean" value for each permutation of company name
companies$cleaned_name[companies$company_name == "Ed Financial Services"] <- "Edfinancial Services"

# ...but it's probably easier to manually create the same table outside of R.

# 2. Looking for the presence of a string or variants
companies %>% filter(str_detect(company_name, regex("navient|navent|naviant|sallie mae|sally mae", ignore_case = TRUE)))

# And replace just the corresponding values in cleaned_name
nav_pattern <- regex("navient|navent|naviant|sallie mae|sally mae", ignore_case = TRUE)
companies$cleaned_name[str_detect(companies$company_name, nav_pattern)] <- "Navient Corp."

# Regardless of how you create this crosswalk, you need to merge it with the
# existing data
companies <- read_csv("cleaned.csv")

# With minimal fuss, this should work. Let's take a look.
left_join(comp, companies) %>% View()

# What if the joining field doesn't have the same name?
names(companies) <- c("dirty_name", "fixed_name")
left_join(comp, companies) %>% View()

# Give the necessary guidance and put it in a new data frame
comp_cleaned <- left_join(comp, companies, by = c("company_name" = "dirty_name"))

# We can try our summary again to see the less fragmented results
comp_summ <- comp_cleaned %>% group_by(fixed_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

##################################
## The wide/long data conundrum ##
##################################

# Let's look at some data that came right out of Chicago's CompStat forms
cs <- read_csv('compstat.csv')

# The gather() function in tidyr can made your wide data long.
gather(
  cs, # the data frame we want to reshape
  timeframe, # a column just for former column names
  events, # a column for the data that used to be spread among columns
  -"Crime Type" # which columns should be a part of this gathering process?
  ) %>% View()

# Put it in a new variable
cs_fixed <- gather(cs, timeframe, events, -`Crime Type`)

# We can reshuffle the row order so it makes a little more sense while looking at it
cs_fixed <- arrange(cs_fixed, `Crime Type`, timeframe)

# Let's say we want to get an idea of YoY change for different crimes

# We can first limit the long data frame to just the crime counts for full calendar years
cs_fixed %>% filter(str_detect(timeframe, "CY")) %>% View()

# We can use another function, mutate(), to add and modify columns in the same step
cs_fixed %>% filter(str_detect(timeframe, "CY")) %>%
  mutate(
    yoy_chg = (events - lag(events)) / lag(events)
  ) %>% View()

# See any problems?

# Even though we are not using the summary capabilities of dplyr here, grouping will keep
# our newly computed column from mixing crime types
cs_chg <- cs_fixed %>% filter(str_detect(timeframe, "CY")) %>%
  group_by(`Crime Type`) %>%
  mutate(
    yoy_chg = (events - lag(events)) / lag(events)
  )

# We can also clean up the percentage change to be more readable
cs_chg$yoy_chg <- round(cs_chg$yoy_chg * 100,2)

# Of course, we might want to know how to do the inverse...

# We can also spread() it back out to a wide version. Let's try with just the original numbers
spread(cs_chg[,-4], timeframe, events) %>% View()

# Now with just the YoY change that we calculated
spread(cs_chg[,-3], timeframe, yoy_chg) %>% View()

# What if we wanted to spread both the original values and the newly computed ones?
# It's a little complicated

# We need to:
gather(cs_chg, data_type, val, -(1:2)) %>% #perform another gather so we just have one value column
  unite(newcolnames, timeframe, data_type, sep = "_") %>% # unite() the text values in timeframe and data_type; these will be new column names
  spread(newcolnames, val) %>% View() # NOW we can spread into a set of columns
