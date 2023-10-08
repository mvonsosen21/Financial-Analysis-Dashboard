
### Financial Analysis

# Goal: RShiny dashboard to display the following:

#1. A time-progressing line/bar chart showing the daily posted balance over time: want to see 3 options - 
#       account 1 balance, account 2 balance, and a total balance of the amounts in both accounts

#2. Expense categorization within individual accounts and both accounts - Questions to answer:
#       Where are we spending the most money in general? 
#       Where are we spending money the most frequently? 
#       Among recurring operating expenses (such as AWS), are we trending up or down in terms of costs? 
#       A time-progressing chart of expenses that are recurring from either/both accounts.

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Read in packages
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(xts)
library(zoo)
library(ggthemes)

### Read in the data / clean the data

# Each dataset has two accounts: First portion is for Checking 1234, second portion is for Checking 5678

# Directory where CSV files are located
folder_path <- "/Users/meganvonsosen/Documents/Financial Analysis/financial_data/"

# Get a list of CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Loop through the CSV files and read them into data frames
data_list <- list()
for (csv_file in csv_files) {
  data <- read.csv(csv_file, header = TRUE, skip = 2)
  data_list <- append(data_list, list(data))
}

# Split into bank accounts
acc1234 = data.frame()
acc5678 = data.frame()

for(df in data_list){
  split_index = which(df$Posted.Date == "Transactions for Checking 5678") # Find the index where the account changes
  nrow_df = nrow(df) # Total number of rows in the dataframe
  acc1234_add = df[1:(split_index-1),]
  acc5678_add = df[(split_index+2):nrow_df,] # +2 because skip the row indicating the account change and the redundant column names
  acc1234 = rbind(acc1234, acc1234_add)
  acc5678 = rbind(acc5678, acc5678_add)
}

# Remove duplicate rows if there is an overlap in the dates of CSVs
acc1234 = unique(acc1234)
acc5678 = unique(acc5678)

# Replace 'Check.Serial..' empty  with N/A
acc1234$Check.Serial..[acc1234$Check.Serial.. == ""] = "N/A"
acc5678$Check.Serial..[acc5678$Check.Serial.. == ""] = "N/A"

## Update the Daily Posted Balance to show a value for each transaction for that day

# Replace blanks with NAs
acc1234$Daily.Posted.Balance[acc1234$Daily.Posted.Balance == ""] = NA
acc5678$Daily.Posted.Balance[acc5678$Daily.Posted.Balance == ""] = NA

# Acct 1234
# Create a mapping of dates to daily posted balances
date_to_value <- acc1234[complete.cases(acc1234), c("Transaction.Date", "Daily.Posted.Balance")]
# Fill in missing values based on the mapping
acc1234$Daily.Posted.Balance[is.na(acc1234$Daily.Posted.Balance)] <- date_to_value$Daily.Posted.Balance[match(acc1234$Transaction.Date[is.na(acc1234$Daily.Posted.Balance)], date_to_value$Transaction.Date)]

# Acct 5678
# Create a mapping of dates to daily posted balances
date_to_value <- acc5678[complete.cases(acc5678), c("Transaction.Date", "Daily.Posted.Balance")]
# Fill in missing values based on the mapping
acc5678$Daily.Posted.Balance[is.na(acc5678$Daily.Posted.Balance)] <- date_to_value$Daily.Posted.Balance[match(acc5678$Transaction.Date[is.na(acc5678$Daily.Posted.Balance)], date_to_value$Transaction.Date)]

# Format the data to be easily combined - change the types to numeric

# Remove dollar sign from numeric columns
acc1234$Amount = gsub("\\$", "", acc1234$Amount)
acc5678$Amount = gsub("\\$", "", acc5678$Amount)
acc1234$Daily.Posted.Balance = gsub("\\$", "", acc1234$Daily.Posted.Balance)
acc5678$Daily.Posted.Balance = gsub("\\$", "", acc5678$Daily.Posted.Balance)

# Convert brackets to - in a function
convert.brackets <- function(x){
  if(grepl("\\(.*\\)", x)){
    paste0("-", gsub("\\(|\\)", "", x))
  } else {
    x
  }
}

# Apply the function
acc1234$Amount = sapply(acc1234$Amount, convert.brackets, USE.NAMES = F)
acc5678$Amount = sapply(acc5678$Amount, convert.brackets, USE.NAMES = F)

# Remove the commas in numeric columns
acc1234$Amount <- as.numeric(gsub(",", "", acc1234$Amount)) 
acc5678$Amount <- as.numeric(gsub(",", "", acc5678$Amount)) 
acc1234$Daily.Posted.Balance <- as.numeric(gsub(",", "", acc1234$Daily.Posted.Balance)) 
acc5678$Daily.Posted.Balance <- as.numeric(gsub(",", "", acc5678$Daily.Posted.Balance)) 

# Change to numeric and keep only expenses
acc1234 = acc1234 %>%
  mutate(Amount = as.numeric(Amount)) %>%
  mutate(Daily.Posted.Balance = as.numeric(Daily.Posted.Balance)) %>%
  filter(Amount < 0)

acc5678 = acc5678 %>%
  mutate(Amount = as.numeric(Amount)) %>%
  mutate(Daily.Posted.Balance = as.numeric(Daily.Posted.Balance)) %>%
  filter(Amount < 0)

# Remove negative sign from Amount
acc1234$Amount = gsub("\\-", "", acc1234$Amount)
acc5678$Amount = gsub("\\-", "", acc5678$Amount)

# Ensure columns are numeric
acc1234 = acc1234 %>%
  mutate(Amount = as.numeric(Amount))

acc5678 = acc5678 %>%
  mutate(Amount = as.numeric(Amount))

## TABLE FOR GOAL 1: Create a table to show total daily posted balances over time for combined accounts

# Acct 1234

# Convert the date columns to date format
acc1234$Posted.Date = mdy(acc1234$Posted.Date)
acc1234$Transaction.Date = mdy(acc1234$Transaction.Date)

# Add in the missing dates
acc1234_alldates = acc1234 %>%
  mutate(Date = as.Date(Posted.Date)) %>%
  complete(Date = seq.Date(acc1234$Posted.Date[1], acc1234$Posted.Date[nrow(acc1234)], by="day")) %>%
  select(Date, Amount, Daily.Posted.Balance) %>%
  rename("Amount_1234" = "Amount") %>%
  rename("Daily.Posted.Balance_1234" = "Daily.Posted.Balance")

# Replace NA Amounts with 0 because there were no transactions that day
acc1234_alldates$Amount_1234[is.na(acc1234_alldates$Amount_1234)] = 0

# Carry over the daily posted amount
acc1234_alldates$Daily.Posted.Balance_1234 = na.locf(acc1234_alldates$Daily.Posted.Balance_1234)

# Acct 5678

# Convert the date columns to date format
acc5678$Posted.Date = mdy(acc5678$Posted.Date)
acc5678$Transaction.Date = mdy(acc5678$Transaction.Date)

# Add in the missing dates
acc5678_alldates = acc5678 %>%
  mutate(Date = as.Date(Posted.Date)) %>%
  complete(Date = seq.Date(acc5678$Posted.Date[1], acc5678$Posted.Date[nrow(acc5678)], by="day")) %>%
  select(Date, Amount, Daily.Posted.Balance) %>%
  rename("Amount_5678" = "Amount") %>%
  rename("Daily.Posted.Balance_5678" = "Daily.Posted.Balance")

# Replace NA Amounts with 0 because there were no transactions that day
acc5678_alldates$Amount_5678[is.na(acc5678_alldates$Amount_5678)] = 0

# Carry over the daily posted amount
acc5678_alldates$Daily.Posted.Balance_5678 = na.locf(acc5678_alldates$Daily.Posted.Balance_5678)

# Group by date to get a sum of amount in/out per day 

## Acct 1234

# Group by the date and sum the amount spent on each date
agg_tbl <- acc1234_alldates %>% group_by(Date) %>% 
  summarise(sum_amount = sum(Amount_1234),
            .groups = 'drop')
acc1234_agg = as.data.frame(agg_tbl)

# Create a dataframe of the dates with the posted balance and then keep only the unique date/balance combos
df_bal = data.frame(acc1234_alldates$Date, acc1234_alldates$Daily.Posted.Balance_1234)
df_bal = df_bal %>%
  distinct()

# Add a column in the data frame of the posted account balance every day
acc1234_agg$Daily.Posted.Balance_1234 = df_bal$acc1234_alldates.Daily.Posted.Balance_1234
acc1234_agg = acc1234_agg %>%
  rename("Amount_1234" = "sum_amount")


## Acct 5678

# Group by the date and sum the amount spent on each date
agg_tbl2 <- acc5678_alldates %>% group_by(Date) %>% 
  summarise(sum_amount = sum(Amount_5678),
            .groups = 'drop')
acc5678_agg = as.data.frame(agg_tbl2)

# Create a dataframe of the dates with the posted balance and then keep only the unique date/balance combos
df_bal2 = data.frame(acc5678_alldates$Date, acc5678_alldates$Daily.Posted.Balance_5678)
df_bal2 = df_bal2 %>%
  distinct()

# Add a column in the data frame of the posted account balance every day
acc5678_agg$Daily.Posted.Balance_5678 = df_bal2$acc5678_alldates.Daily.Posted.Balance_5678
acc5678_agg = acc5678_agg %>%
  rename("Amount_5678" = "sum_amount")

# Combine the accounts into one dataframe based on the earliest date they have in common
start_date_1234 = min(acc1234_agg$Date)
start_date_5678 = min(acc5678_agg$Date)

if((start_date_1234 < start_date_5678) == TRUE){
  sd = start_date_5678
} else if((start_date_5678 < start_date_1234) == TRUE) {
  sd = start_date_1234
} else if((start_date_1234 == start_date_5678) == TRUE) {
  sd = start_date_1234
}

# Filter based on start date
acc1234_agg = acc1234_agg[acc1234_agg[,'Date'] >= sd,]
acc5678_agg = acc5678_agg[acc5678_agg[,'Date'] >= sd,]

accts_total = data.frame(acc1234_agg$Date, acc1234_agg$Amount_1234, acc5678_agg$Amount_5678, acc1234_agg$Daily.Posted.Balance_1234, acc5678_agg$Daily.Posted.Balance_5678)

# Rename columns
accts_total = accts_total %>%
  rename("Date" = "acc1234_agg.Date") %>%
  rename("Amount_1234" = "acc1234_agg.Amount_1234") %>%
  rename("Amount_5678" = "acc5678_agg.Amount_5678") %>%
  rename("Daily.Posted.Balance_1234" = "acc1234_agg.Daily.Posted.Balance_1234") %>%
  rename("Daily.Posted.Balance_5678" = "acc5678_agg.Daily.Posted.Balance_5678") 

# To be used for the time-progressing line/bar chart showing the daily posted balance over time
accts_total$Total_Balance = accts_total$Daily.Posted.Balance_1234 + accts_total$Daily.Posted.Balance_5678

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1. A time-progressing line/bar chart showing the daily posted balance over time: toggle between 3 options - 
#       account 1 balance, account 2 balance, and a total balance of the amounts in both accounts

#### Use 'accts_total' for visualizations which can be seen in the dashboard

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2. Expense categorization within individual accounts and both accounts - 
#       Where are we spending the most money in general? 
#       Where are we spending money the most frequently? (counts)
#       Among recurring operating expenses (such as AWS), are we trending up or down in terms of costs? 
#       A time-progressing chart of expenses that are recurring from either/both accounts.

# Subset necessary columns for both accounts
acc1234_cat = acc1234 %>%
  select(Transaction.Date, Transaction.Type, Description, Amount, Daily.Posted.Balance)

acc5678_cat = acc5678 %>%
  select(Transaction.Date, Transaction.Type, Description, Amount, Daily.Posted.Balance)

# Categorize based on description

categ_1234 = c()
for (i in 1:nrow(acc1234_cat)){
  # check each category
  if(grepl("TRANSFER", acc1234_cat$Description[i], fixed = TRUE) == TRUE){
    categ_1234 = c(categ_1234, "Transfer")
  } else if(grepl("Amzn.com/bill", acc1234_cat$Description[i], fixed = TRUE) == TRUE){
    categ_1234 = c(categ_1234, "Internal Company Purchases") 
  } else if(grepl("Amazon web", acc1234_cat$Description[i], fixed = TRUE) == TRUE) {
    categ_1234 = c(categ_1234, "AWS")
  } else if(grepl("CHECK", acc1234_cat$Description[i], fixed = TRUE) == TRUE) {
    categ_1234 = c(categ_1234, "Check")
  } else if((TRUE %in% sapply(c("Bob", "Jerry", "Sarah"), grepl, acc1234_cat$Description[i])) == TRUE){
    categ_1234 = c(categ_1234, "Team Internal Payout")
  } else if(grepl("PAYROLL", acc1234_cat$Description[i], fixed = TRUE) == TRUE){
    categ_1234 = c(categ_1234, "Team External Payout")
  } else if(grepl("GOOGLE", acc1234_cat$Description[i], fixed = TRUE) == TRUE){
    categ_1234 = c(categ_1234, "Website Costs")
  } else if(grepl("research.conference", acc1234_cat$Description[i], fixed = TRUE) == TRUE){
    categ_1234 = c(categ_1234, "Research Conference")
  } else if((TRUE %in% sapply(c("TAX", "TAXACT"), grepl, acc1234_cat$Description[i])) == TRUE){
    categ_1234 = c(categ_1234, "Tax Costs")
  } else{ # Else includes: food, drinks, gifts, parking, etc. - personal purchases
    categ_1234 = c(categ_1234, "Miscellaneous")
  } 
}

categ_5678 = c()
for (i in 1:nrow(acc5678_cat)){
  # check each category
  if(grepl("TRANSFER", acc5678_cat$Description[i], fixed = TRUE) == TRUE){
    categ_5678 = c(categ_5678, "Transfer")
  } else if(grepl("Amzn.com/bill", acc5678_cat$Description[i], fixed = TRUE) == TRUE){
    categ_5678 = c(categ_5678, "Internal Company Purchases")
  } else if(grepl("Amazon web", acc5678_cat$Description[i], fixed = TRUE) == TRUE) {
    categ_5678 = c(categ_5678, "AWS")
  } else if(grepl("CHECK", acc5678_cat$Description[i], fixed = TRUE) == TRUE) {
    categ_5678 = c(categ_5678, "Check")
  } else if((TRUE %in% sapply(c("Bob", "Jerry", "Sarah"), grepl, acc5678_cat$Description[i])) == TRUE){
    categ_5678 = c(categ_5678, "Team Internal Payout")
  } else if(grepl("PAYROLL", acc5678_cat$Description[i], fixed = TRUE) == TRUE){
    categ_5678 = c(categ_5678, "Team External Payout")
  }  else if(grepl("GOOGLE", acc5678_cat$Description[i], fixed = TRUE) == TRUE){
    categ_5678 = c(categ_5678, "Website Costs")
  } else if(grepl("research.conference", acc5678_cat$Description[i], fixed = TRUE) == TRUE){
    categ_5678 = c(categ_5678, "Research Conference")
  } else if((TRUE %in% sapply(c("TAX", "TAXACT"), grepl, acc5678_cat$Description[i])) == TRUE){
    categ_5678 = c(categ_5678, "Tax Costs")
  } else{ # Else includes: food, drinks, gifts, parking, etc. - personal purchases
    categ_5678 = c(categ_5678, "Miscellaneous")
  } 
}

# Add columns to df and check categories
acc1234_cat$Expense.Category = categ_1234
acc5678_cat$Expense.Category = categ_5678

########## Where are we spending the most money in general?

# Use acc1234_cat and acc5678_cat for aggregations and visualizations - code is seen in dashboard

############ Where are we spending money the most frequently? (table of counts of expense types)

# Use acc1234_cat and acc5678_cat for aggregations and visualizations - code is seen in dashboard

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#3. Among recurring operating expenses (such as AWS), are we trending up or down in terms of costs? 
#   A time-progressing chart of expenses that are recurring from either/both accounts.

# Acc 5678
acc5678_recurring = acc5678_cat %>%
  select(Transaction.Date, Amount, Expense.Category) %>%
  filter(Expense.Category != "Miscellaneous") %>%
  filter(Expense.Category %in% c("AWS", "Website Costs"))

# Identify what type of recurring expense each one is
recur_type = c()
for(i in 1:nrow(acc5678_recurring)){
  if(acc5678_recurring$Expense.Category[i] == "Website Costs"){
    if(acc5678_recurring$Amount[i] < 20){
      recur_type = c(recur_type, "Domain Name Cost") 
    } else{
      recur_type = c(recur_type, "Google Suite Cost") 
    }
  } else{
    recur_type = c(recur_type, "AWS")
  }
}

# Add the recurring payment type to the dataframe and remove expense category

# By category
acc5678_recurring$Recurring.Category = recur_type

# Use acc5678_recurring for aggregations and visualizations - code is seen in dashboard

