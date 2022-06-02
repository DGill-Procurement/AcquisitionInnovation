#Identify new federal contractors/vendors.
#Current code identifies contractors new to the IRS. 
#Data source is publicly available USASpending.gov prime contract award date.
#Using government-wide USASpending data will instead indentify first time vendors with the federal government as a whole.
#Code may be helpful in achieving New Entrant goals identified in OMB-22-03 Advancing Equity in Federal Procurement.
#https://www.whitehouse.gov/wp-content/uploads/2021/12/M-22-03.pdf

require(dplyr)
require(gtools)
library(lubridate)

setwd("C:\\USASpending\\")

treasury_fy17 <- read.csv('treasury_fy17_contracts_prime_transactions_1.csv')
treasury_fy18 <- read.csv('treasury_fy18_contracts_prime_transactions_1.csv')
treasury_fy19 <- read.csv('treasury_fy19_contracts_prime_transactions_1.csv')
# Bind different years of contract data into a single dataframe.
treasury <- smartbind(treasury_fy17,treasury_fy18,treasury_fy19)
IRS <- subset(treasury, awarding_sub_agency_name == "INTERNAL REVENUE SERVICE")

#Data source has ~250 columns. Grab only selected columns needed for analysis.
IRSsc <- dplyr::select(IRS, recipient_name, recipient_duns ,number_of_offers_received, naics_description, federal_action_obligation, number_of_actions, primary_place_of_performance_state_name,action_date)

IRSsc$action_date <- as.Date(IRSsc$action_date)
IRSsc$year <- year(IRSsc$action_date)
IRSsc$in_2017 <- IRSsc$year ==2017
IRSsc$in_2017 <- as.numeric(IRSsc$in_2017)
IRSsc$cy2017_obligation <- IRSsc$federal_action_obligation * IRSsc$in_2017
IRSsc$in_2018 <- IRSsc$year ==2018
IRSsc$in_2018 <- as.numeric(IRSsc$in_2018)
IRSsc$cy2018_obligation <- IRSsc$federal_action_obligation * IRSsc$in_2018
IRSsc$in_2019 <- IRSsc$year ==2019
IRSsc$in_2019 <- as.numeric(IRSsc$in_2019)
IRSsc$cy2019_obligation <- IRSsc$federal_action_obligation * IRSsc$in_2019

#Create function to calculate the most frequently occuring categorical variable (e.g. does vendor most frequent get transactions as a small or other than small business)
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#Group dataframe by vendor (aka Receipient DUNS Number Unique Identifier)
IRSsc <- group_by(IRSsc, recipient_duns)
#Summarize annual contract spending by vendor.
IRS_summary <- summarize(IRSsc, recipient_name = calculate_mode(recipient_name), cy2017_total = sum(cy2017_obligation), cy2018_total = sum(cy2018_obligation), cy2018_total = sum(cy2018_obligation), cy2018_total = sum(cy2018_obligation), cy2019_total = sum(cy2019_obligation))
#If null value (i.e. no spending with vendor in given year) then inpute zero dolllars.
IRS_summary[is.na(IRS_summary)] <- 0
#If spending with vendor in earlier years = 0, but vendor has received dollars recently - could be considered a new vendor.
write.csv(IRS_summary, file ="IRS_summary_by_year.csv")
