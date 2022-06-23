require(gdata)
#Load dplyr package for data wrangling / preparation.
require(dplyr)
require(data.table)
#Load package for Random Forest predictive model.
require(randomForest)
#Load package for date calculations.
require(lubridate)
require(pmml)
 
setwd("/projects/CRA/USASpending")
FY_2020 <- fread("FY_2020.csv")
 
FY_2020_mr <- select(FY_2020, action_date, solicitation_date, solicitation_procedures, base_and_all_options_value, type_of_contract_pricing, awarding_agency_name, awarding_sub_agency_name, funding_sub_agency_name, number_of_offers_received, contracting_officers_determination_of_business_size_code, contracting_officers_determination_of_business_size_code, naics_code, parent_award_type)
 
FY_2020_mr$action_date <- as.Date(FY_2020_mr$action_date)
FY_2020_mr$solicitation_date <- as.Date(FY_2020_mr$solicitation_date)
FY_2020_mr$type_of_contract_pricing <- as.factor(FY_2020_mr$type_of_contract_pricing)
FY_2020_mr$solicitation_procedures <- as.factor(FY_2020_mr$solicitation_procedures)
FY_2020_mr$small_business_or_other <- as.factor(FY_2020_mr$contracting_officers_determination_of_business_size_code)
FY_2020_mr$parent_award_type <- as.factor(FY_2020_mr$parent_award_type)
 
FY_2020_mr$PALT_days <- FY_2020_mr$action_date - FY_2020_mr$solicitation_date
FY_2020_mr <- subset(FY_2020_mr, PALT_days > 0 & PALT_days <= 730)
FY_2020_end <- max(FY_2020_mr$action_date)
FY_2020_mr$days_remaining_until_FY_end <- FY_2020_end - FY_2020_mr$solicitation_date
FY_2020_mr$month_of_solicitation <- month(FY_2020_mr$solicitation_date)
 
FY_2020_mr$assisted_acquisition <- FY_2020_mr$awarding_sub_agency_name != FY_2020_mr$funding_sub_agency_name 
FY_2020_mr$civilian_agency_not_DOD <- FY_2020_mr$awarding_agency_name != "DEPARTMENT OF DEFENSE (DOD)" 
 
 
#Impute median number of offerers received (3) in the case of null values.
FY_2020_mr$number_of_offers_received[is.na(FY_2020_mr$number_of_offers_received)] <- 3
 
FY_2020_mr$parent_award_type <- as.character(FY_2020_mr$parent_award_type)
FY_2020_mr$parent_award_type[is.na(FY_2020_mr$parent_award_type)] <- "No Parent Award"
FY_2020_mr$parent_award_type <- as.factor(FY_2020_mr$parent_award_type)
 
summary(FY_2020_mr)
 
FY_2020_mr <- na.omit(FY_2020_mr)
FY_2020_mr <- select(FY_2020_mr, -action_date, -solicitation_date, -awarding_agency_name, -awarding_sub_agency_name, -funding_sub_agency_name, -contracting_officers_determination_of_business_size_code)
 
set.seed(1102)
FY_2020_mr_sample <- sample_n(FY_2020_mr, 50000) 
 
FY_2020_USASpending_PALT_model <- randomForest(PALT_days ~., data=FY_2020_mr_sample, localImp = TRUE, ntree = 10)
 
#FY_2020_USASpending_PALT_model_pmml <- pmml(FY_2020_USASpending_PALT_model)
#saveXML(pmml(FY_2020_USASpending_PALT_model, data=FY_2020_mr_sample), "FY_2020_USASpending_PALT_model_pmml.pmml")
 
 
#Display model performance statistics and plot the importance of features used in the predictive model.
print(FY_2020_USASpending_PALT_model)
importance(FY_2020_USASpending_PALT_model)
varImpPlot(FY_2020_USASpending_PALT_model)
