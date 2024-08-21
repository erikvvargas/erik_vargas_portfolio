library(tidyverse)
library(lubridate)
library(readxl)

# Week 1: The Data Source Bank ----

## Input Data
df <- read_csv("C:\\Users\\erik.v.vargas\\OneDrive - US Navy-flankspeed\\Documents\\code_analysis\\PD 2023 Wk 1 Input.csv")

# unlist(strsplit(df$`Transaction Code`, "-"))

## Split Transaction Code, rename new field with Bank
df$Bank <-  str_split_fixed(df$`Transaction Code`, fixed("-"), 2)[,1]

# Rename values from 1/2 to Online/In-person
df$`Online or In-Person` <- ifelse(df$`Online or In-Person` == 1, "Online", "In-Person")

# Change date to day of week
df$`Transaction Date` <-  wday(dmy_hms(df$`Transaction Date`), label = TRUE, abbr = FALSE) 

# Total values of transaction by bank
df %>% 
  group_by(Bank) %>% 
  summarise(Value = sum(Value))

# Total values by bank, day of week, and type of transaction
df %>% 
  group_by(Bank, `Online or In-Person`, `Transaction Date`) %>% 
  summarise(Value = sum(Value)) %>% View()

# total values by bank and customer
df %>% 
  group_by(Bank, `Customer Code`) %>% 
  summarise(Value = sum(Value)) %>% View()

# Week 2: International Bank Account Numbers ----

# input data
transactions <- read_csv("C:\\Users\\erik.v.vargas\\OneDrive - US Navy-flankspeed\\Documents\\code_analysis\\Transactions.csv")

codes <- read_csv("C:\\Users\\erik.v.vargas\\OneDrive - US Navy-flankspeed\\Documents\\code_analysis\\Swift Codes.csv")

# Remove dashes in transactions table Sort Code

transactions$`Sort Code` <-  gsub("-", "", transactions$`Sort Code`)

# SWIFT Bank Code lookup join

transactions <- left_join(transactions, codes, by = "Bank")

# add field for Country Code
transactions$`Country Code` <- "GB"

# Create IBAN as displayed

transactions <- transactions %>% 
  mutate(IBAN = paste0(`Country Code`, `Check Digits`, `SWIFT code`, `Sort Code`, `Account Number`))

# put submission in format requested
transactions_small <- transactions %>% 
  select(`Transaction ID`, IBAN)

# Week 3: Targets for Data Source Bank ----

# input data
df <- read_csv("C:\\Users\\erik.v.vargas\\OneDrive - US Navy-flankspeed\\Documents\\code_analysis\\PD 2023 Wk 1 Input.csv")

targets <- read_csv("C:\\Users\\erik.v.vargas\\OneDrive - US Navy-flankspeed\\Documents\\code_analysis\\Targets.csv")

# filter for only transaction codes with DSB
df <- df %>% 
  filter(grepl("DSB", `Transaction Code`))

# Rename values from 1/2 to Online/In-person
df$`Online or In-Person` <- ifelse(df$`Online or In-Person` == 1, "Online", "In-Person")

# Change date to the quarter
df$`Transaction Date` <-  quarter(dmy_hms(df$`Transaction Date`))

# sum transaction values for each quarer and for each type of transaction

df.grouped <- df %>%  
  group_by(`Transaction Date`, `Online or In-Person`) %>% 
  summarise(Value = sum(Value)) 

# pivot targets data so there is one row for each transaction type and quarter pair
# fields renamed
targets.pivot <- targets %>% 
  pivot_longer(!`Online or In-Person`, names_to = "Quarter", values_to = "Target")

# remove "Q" from quarter column and make numeric

targets.pivot$Quarter <- as.integer(str_split_fixed(targets.pivot$Quarter, "", 2)[,2])

# join the two datasets together

df.final <- left_join(df.grouped, targets.pivot, by = c("Online or In-Person", "Transaction Date" = "Quarter"))

# calculate variance to each target

df.final <- df.final %>% 
  mutate("Variance to Target" = Value - Target)

# reorder columns to format requested

df.final <- df.final %>% 
  select(`Online or In-Person`, Quarter = `Transaction Date`, Value, Target, `Variance to Target`)

# Week 4: New Customers ----

# path <- "C:\\Users\\erik.v.vargas\\OneDrive - US Navy-flankspeed\\Documents\\code_analysis\\New Customers.xlsx"

path <- "C:\\Users\\Erik\\Documents\\New Customers.xlsx"

# input data and stack sheets
customers <- path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(~ read_excel(path = path, sheet = .x), .id = "sheet")

# Combine multiple demographic columns into one

customers <- customers %>% 
  mutate(Demographics = coalesce(customers$Demographic, customers$Demagraphic, customers$Demographiic))

# create Joining Date variable

customers <- customers %>% 
  mutate(`Joining Date` = mdy(paste(sheet, `Joining Day`, "2023",
                                    sep = "-")))

# Reshape data so we have a field for each demographic

customers <- customers %>% 
  select(-c(Demographic, Demagraphic, Demographiic))

customers <- customers %>% 
  pivot_wider(names_from = Demographics, values_from = Value)

# ensure data types are correct for each column

glimpse(customers)

customers$`Date of Birth` <- mdy(customers$`Date of Birth`)

# looking for duplicates

length(unique(customers$ID))
nrow(customers)

customers <- customers %>% 
  group_by(ID) %>% 
  arrange(`Joining Date`) %>% 
  filter(row_number() == 1)

# remove columns and put in format requested

customers <- customers %>% 
  select(ID, `Joining Date`, `Account Type`, 
         `Date of Birth`, Ethnicity)

# Week 5: DSB Ranking ----

# input data
dsb <- read_csv("C:\\Users\\erikv\\OneDrive\\Documents\\PD 2023 Wk 1 Input.csv")

## Split Transaction Code, rename new field with Bank
dsb$Bank <-  str_split_fixed(dsb$`Transaction Code`, fixed("-"), 2)[,1]

# change transaction date to be month
dsb$`Transaction Date` <-  month(dmy_hms(dsb$`Transaction Date`), 
                                 label = TRUE, abbr = FALSE)

# grouping by bank and month and getting value total
dsb <- dsb %>% 
  group_by(Bank, `Transaction Date`) %>% 
  summarise(Value = sum(Value)) 

# ranking the banks by month
dsb <- dsb %>% 
  arrange(`Transaction Date`, desc(Value))

dsb$Rank <- rep(c(1,2,3), 12)

# Getting average bank rank across all months
# Getting average transaction value by bank

dsb <- dsb %>% 
  group_by(Bank) %>% 
  mutate(`Avg Rank per Bank` = mean(Rank)) %>% 
  ungroup() %>% 
  group_by(Rank) %>% 
  mutate(`Avg Transaction Value per Rank` = mean(Value))

# Week 6: DSB Customer Ratings ----

# input data
survey <- read_csv("C:\\Users\\erikv\\OneDrive\\Documents\\DSB Customer Survery.csv")

# reshaping data so each customer has 5 rows with two columns
# for each type of online and mobile app
# names are also cleaned for the type of the survey question

survey <- survey %>% 
  pivot_longer(
    -`Customer ID`,
    names_to = c(".value", "Type"),
    names_sep = "-"
  )

# remove whitespace in Type column from original column names

survey$Type <- trimws(survey$Type)

# remove "Overall Rating"

survey <- survey %>% 
  filter(!(Type == "Overall Rating"))

# calculate average rating of each platform per customer

survey <- survey %>% 
  group_by(`Customer ID`) %>% 
  mutate("Average Mobile App Rating" = mean(`Mobile App `),
         "Average Online Rating" = mean(`Online Interface `)
         )

# calculate difference in average ratings for each customer

survey <- survey %>% 
  mutate("Rating Difference" = `Average Mobile App Rating` - `Average Online Rating`)

# categorizing customers based on their preferences

survey <- survey %>% 
  mutate("Preference" = case_when(
     `Rating Difference` >= 2 ~ "Mobile App Superfan",
     `Rating Difference` >= 1 ~ "Mobile App Fan",
     `Rating Difference` <= -2 ~ "Online Interface Superfan",
     `Rating Difference` <= -1 ~ "Online Interface Fan",
    TRUE ~ "Neutral"
  ))

# calculate the Percent of Total customers in each category, rounded to 1 decimal place

t1 <- round(prop.table(table(survey$Preference)) * 100, 1)

t1 <- as.data.frame(t1)

colnames(t1) <- c("Preference", "% of Total")

# Week 7 - Flagging Fraudulent Suspicions ----

# input data
trans_path <- read_csv("C:\\Users\\erikv\\OneDrive\\Documents\\Transaction Path.csv")

trans_detail <- read_csv("C:\\Users\\erikv\\OneDrive\\Documents\\Transaction Detail.csv")

acct_holders <- read_csv("C:\\Users\\erikv\\OneDrive\\Documents\\Account Holders.csv")

acct_info <- read_csv("C:\\Users\\erikv\\OneDrive\\Documents\\Account Information.csv")

# change naming convention of transaction path file

colnames(trans_path)[2:3] <- c("Account To", "Account From")

# ensure no null values in Account Holder ID of account table

sum(is.na(acct_info$`Account Holder ID`))

# Ensure one account holder id per row, separate rows with two
# account holders into two rows

acct_info <- acct_info %>% 
  separate_longer_delim(`Account Holder ID`, delim = ", ")

# length(unique(acct_info$`Account Holder ID`))
# make sure all phone numbers start with 07

acct_holders$`Contact Number` <- paste0("0",acct_holders$`Contact Number`)

# bring all of the tables together

trans_detail <- left_join(trans_detail, trans_path, by = "Transaction ID")

acct_info$`Account Holder ID` <- as.numeric(acct_info$`Account Holder ID`)

acct_info <- left_join(acct_info, acct_holders, by = "Account Holder ID")

trans_detail <- left_join(trans_detail, acct_info, by = c("Account To" = "Account Number"))

# filter out cancelled transactions

trans_detail <- trans_detail %>% 
  filter(`Cancelled?` == "N")

# filter out transactions > 1000

trans_detail <- trans_detail %>% 
  filter(Value >= 1000)

# filter out platinum accounts

trans_detail <- trans_detail %>% 
  filter(`Account Type` != "Platinum")

length(unique(trans_detail$`Transaction ID`))

# Week 8 - Taking Stock ----

