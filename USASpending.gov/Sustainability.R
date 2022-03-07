#Estimate error rate for contract environmental sustainability data reported to USASpending.gov (e.g. EPA Designated Products and Recovered Materials Sustainability data elements). 
#Train supervised machine learning model to assess how Treasury/IRS contract sustainability coding deviates from lead agency (e.g. Environmental Protection Agency) contract coding. 
require(data.table) # Data wrangling / read in spreadsheets quickly.
require(dplyr) # Data wrangling
require(gtools) # Union / combine datasets
require(randomForest) # Supervised machine learning algorithm.
require(lubridate)
require(pmml)
require(caTools)
library(ROCR)
require(caret)

set.seed(1102)

setwd("/projects/CRA/data/Government-Wide")

#Read in contract records expertly labeled by Environmental Protection Agency employees.
EPA_FY21 <- fread("EPA_FY2021_068_Contracts_Full_20211109_1.csv")
modeling_data <- select(EPA_FY21, recovered_materials_sustainability, epa_designated_product, naics_code, base_and_all_options_value, primary_place_of_performance_zip_4, recipient_duns, award_description)
modeling_data$recovered_materials_sustainability <- as.factor(modeling_data$recovered_materials_sustainability)
modeling_data$epa_designated_product <- as.factor(modeling_data$epa_designated_product)
modeling_data$primary_place_of_performance_zip_4 <- as.numeric(modeling_data$primary_place_of_performance_zip_4)

modeling_data <- na.omit(modeling_data)

modeling_data <- select(modeling_data,-award_description)
str(modeling_data)
summary(modeling_data)

setwd("/projects/CRA/Climate")
write.csv(modeling_data,"Sustainability_modeling_data.csv")

#Code for creating train/test split for models. May not be necessary to create split when using random forest algorithm.
#modeling_data_train <- modeling_data[1:10000, ]
#modeling_data_train <- na.omit(modeling_data_train)
#modeling_data_test <- modeling_data[10001:14152, ]

modeling_data_EPA <- modeling_data

#Create true/false variable indicating whether contract meets EPA designated product requirements.
modeling_data_EPA$meets_EPA_product_requirements <- grepl("MEETS REQUIREMENTS", modeling_data$epa_designated_product, fixed=TRUE)
summary(modeling_data_EPA$meets_EPA_product_requirements)
modeling_data_EPA <- select(modeling_data_EPA,-recovered_materials_sustainability, -epa_designated_product)
EPA_Product_RF_model <- randomForest(as.factor(meets_EPA_product_requirements) ~., data=modeling_data_EPA, localImp = TRUE, ntree = 5)
print(EPA_Product_RF_model)
importance(EPA_Product_RF_model)

#Export to interoperable Predictive Modeling Markup Language. Code not currently working.
EPA_Product_RF_model_pmml <- pmml(EPA_Product_RF_model)
saveXML(pmml(EPA_Product_RF_model_pmml, data=modeling_data_EPA), "EPA_Product_RF_model_pmml.pmml")
#Error in UseMethod("pmml") : no applicable method for 'pmml' applied to an object of class "c('XMLNode', 'RXMLAbstractNode', 'XMLAbstractNode')"

#Planned future development of similar model for Recovered Materials / Sustainability contract clauses.
#Rec_Mat_Sustain_RF_model <- randomForest(recovered_materials_sustainability ~., data=modeling_data_EPA, localImp = TRUE, ntree = 5)

#Identify possible implementation gaps for EPA designated products in Treasury contracts using previously trained machine learning model.

setwd("/projects/CRA/data/treasury")
treasury_fy21 <- fread("fy21_contracts_prime_transactions_1.csv")
treasury_fy21 <- select(treasury_fy21, recovered_materials_sustainability, epa_designated_product, naics_code, base_and_all_options_value, primary_place_of_performance_zip_4, recipient_duns, award_description)
treasury_fy21$meets_EPA_product_requirements <- grepl("MEETS REQUIREMENTS", treasury_fy21$epa_designated_product, fixed=TRUE)
treasury_fy21$primary_place_of_performance_zip_4 <- as.numeric(treasury_fy21$primary_place_of_performance_zip_4)

treasury_fy21 <- na.omit(treasury_fy21)

#Include only data features needed to run classification model.
treasury_fy21_features <- select(treasury_fy21,-epa_designated_product, -recovered_materials_sustainability, -award_description)
predict <- predict(EPA_Product_RF_model, treasury_fy21_features, type = 'response')
treasury_fy21$predicted_EPA_product_applicability <- predict
summary(treasury_fy21)
setwd("/projects/CRA/Climate")
write.csv(treasury_fy21,"treasury_EPA_product_applicability_assessment.csv")


