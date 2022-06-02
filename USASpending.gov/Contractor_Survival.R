# Model survival of government contractors and associated hazards.
# Identify rate at which government contractors go out of business (exit federal contracting).
# Calculate the probability of survival considering volatility measures for vendor's federal work.
# OMB has identified a "general decline in the small business supplier base" 
# (see https://www.whitehouse.gov/wp-content/uploads/2021/12/M-22-03.pdf).
# See below for a paper on government contractor surival.
# https://calhoun.nps.edu/handle/10945/58772

library("survival")
library("survminer")
require(cmprsk)
require(data.table)
require(dplyr)
require(gtools)
library(lubridate)
library(pmml)
require(utils)
require(randomForest)

# Download data on subset of contractor population from USASpending.gov.
# Selected data on population of small, disadvantaged contractors in the 8(a) Business Development Program.
# 8(a) contractors are often go out of business after graduation from the program (and losing special eligibility for sole source contracts.)

setwd("/projects/CRA/data/Government-Wide/8a")
file_1 <- fread(unzip("PrimeTransactionsAndSubawards_2021-08-03_H13M23S31465376.zip", files = "Contracts_PrimeTransactions_2021-08-03_H13M23S36_1.csv"))
file_2 <- fread(unzip("PrimeTransactionsAndSubawards_2021-08-03_H18M39S50342648.zip", files = "Contracts_PrimeTransactions_2021-08-03_H18M41S02_1.csv"))
file_1 <- select(file_1,recipient_name,recipient_duns,federal_action_obligation,base_and_all_options_value,award_id_piid,modification_number,period_of_performance_start_date, period_of_performance_potential_end_date,action_type,product_or_service_code,naics_code,place_of_manufacture,type_of_set_aside,number_of_offers_received,action_date,contracting_officers_determination_of_business_size_code, funding_sub_agency_name, award_or_idv_flag, woman_owned_business)
file_2 <- select(file_2,recipient_name,recipient_duns,federal_action_obligation,base_and_all_options_value,award_id_piid,modification_number,period_of_performance_start_date, period_of_performance_potential_end_date,action_type,product_or_service_code,naics_code,place_of_manufacture,type_of_set_aside,number_of_offers_received,action_date,contracting_officers_determination_of_business_size_code, funding_sub_agency_name, award_or_idv_flag, woman_owned_business)
contractor_survival <- smartbind(file_1, file_2)

contractor_survival$action_date <- as.Date(contractor_survival$action_date)

#Create function to calculate the most frequently occuring categorical variable (e.g. does vendor most frequent get transactions as a small or other than small business)
calculate_mode <- function(x) {
  uniqx <- unique(na.omit(x))
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

contractor_survival$funding_sub_agency_name <- as.factor(contractor_survival$funding_sub_agency_name)
#print(unique(contractor_survival$funding_sub_agency_name))

setDT(contractor_survival)  # set the data frame as data table

#Transform transactional data into summary level metrics.
#Term definitions for USASpending source data can be found at https://www.usaspending.gov/?glossary&
#Additional term definitions:
#Obligation means a legally binding contract with a specific vendor and a reservation of funds (i.e. in a "lockbox") to pay for work performed.
#Deobligation is a removal of reserved funds from a contract.
#IDV means indefinite delvery contract vehicle that allows one or more federal agencies to place orders for products or services.
contractor_survival_summary <- contractor_survival[, list(recipient_name = max(recipient_name),
                                    range_contract_value = max(base_and_all_options_value)-mean(base_and_all_options_value), 
                                    range_obligation = max(federal_action_obligation)-mean(federal_action_obligation), sd_obligation = sd(federal_action_obligation), has_IDV = grepl("IDV", award_or_idv_flag),
                                    funds_increased = sum(federal_action_obligation[federal_action_obligation>0]), funds_decreased = sum(federal_action_obligation[federal_action_obligation<0]),
                                    max_contract_value = max(base_and_all_options_value), earliest_contract = min(action_date), latest_contract = max(action_date), num_of_agency_customers = length(unique(funding_sub_agency_name)),
                                    most_common_set_aside = calculate_mode(type_of_set_aside), small_business_or_other = calculate_mode(contracting_officers_determination_of_business_size_code),
                                    mean_offers = mean(na.omit(number_of_offers_received))), 
                             by=list(recipient_duns)]

#Create time and event labels for survival.
contractor_survival_summary$days_as_a_contractor <- contractor_survival_summary$latest_contract - contractor_survival_summary$earliest_contract 
contractor_survival_summary$no_longer_a_contractor <- contractor_survival_summary$days_as_a_contractor < 700
contractor_survival_summary$no_longer_a_contractor <- contractor_survival_summary$latest_contract < max(contractor_survival$action_date) - 365
head(contractor_survival_summary)

contractor_survival_summary <- na.omit(contractor_survival_summary)

contractor_survival_summary$small_business_or_other <- as.factor(contractor_survival_summary$small_business_or_other)
contractor_survival_summary$most_common_set_aside <- as.factor(contractor_survival_summary$most_common_set_aside)

contractor_survival_summary$deob_percent <- contractor_survival_summary$funds_decreased / contractor_survival_summary$funds_increased
contractor_survival_summary$deob_percent <- is.infinite(contractor_survival_summary$deob_percent)
contractor_survival_summary$deob_percent[contractor_survival_summary$deob_percent == -Inf] <- 0

summary(contractor_survival_summary)

setwd("/projects/CRA/Survival")
write.csv(contractor_survival_summary, "new_contractor_survival_summary.csv")
#Plot contractor survival freqency distribution.
hist(as.numeric(contractor_survival_summary$days_as_a_contractor))

#Fit Cox Proportional Hazard Model
coxph_model <- coxph(Surv(days_as_a_contractor, no_longer_a_contractor) ~ small_business_or_other + most_common_set_aside + range_obligation + sd_obligation + deob_percent + range_contract_value + has_IDV + num_of_agency_customers, data = contractor_survival_summary)
cz <- cox.zph(coxph_model)
print(cz)
plot(cz)
summary(coxph_model)
print(coxph_model)

#Try using alternative algorithms to model contractor survival.

#Fit Kaplan Meier Model
contractor_survival_summary$days_as_a_contractor <- as.numeric(contractor_survival_summary$days_as_a_contractor)
km_model <- survfit(contractor_survival_summary$days_as_a_contractor, contractor_survival_summary$no_longer_a_contractor,data=contractor_survival_summary,type='kaplan-meier',conf.type='log')
#Error: unexpected ',' in "km.model <- survfit( Surv(days_as_a_contractor, no_longer_a_contractor) ~ 1),"

#rf_contractor_survival_summary <- select(contractor_survival_summary, days_as_a_contractor, funds_increased, small_business_or_other)
#Create Regression Forest
rf_contractor_survival_summary <- select(contractor_survival_summary, -recipient_name, -latest_contract, - no_longer_a_contractor)
rf_model <- randomForest(days_as_a_contractor ~ .,   data=rf_contractor_survival_summary, ntree=2)
print(rf_model)
varImpPlot(rf_model)

# Note: Predictive Modeling Markup Language (PMML) export will not work if model includes character values. Converted character values to factor so that PMML code can be produced.
coxph_pmml <- pmml(coxph_model)
saveXML(pmml(coxph_model, data=contractor_survival_summary), "coxph_model_pmml.pmml")

plot(survfit(coxph_model), col=4)


#Use predict function on Coxph model.
#Code is work in progress.
contractor_survival_summary$lp <- predict(coxph_model,type="lp")
contractor_survival_summary$expected <- predict(coxph_model,type="expected")

contractor_survival_summary$risk <-predict(coxph_model,type="risk",se.fit=TRUE)
contractor_survival_summary$terms <-predict(coxph_model,type="terms",se.fit=TRUE)

results <- contractor_survival_summary[c('days_as_a_contractor', 'no_longer_a_contractor')]
results <- cbind(results, lp=predict(coxph_model, type="lp"))
results <- cbind(results, risk=predict(coxph_model, type="risk"))
results <- cbind(results, expected=predict(coxph_model, type="expected"))
results <- cbind(results, terms=predict(coxph_model, type="terms"))
coxph_model.age.results <- results
head(coxph_model.age.results)