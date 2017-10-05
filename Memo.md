10/3 

Ranger result

Call:
 ranger(formula = as.formula(paste("response", "~", paste(explanation_variable,      collapse = "+"))), data = TRAIN, mtry = as.numeric(model_list[[1]]$bestTune["mtry"]),      probability = TRUE, importance = "impurity") 

Type:                             Probability estimation 
Number of trees:                  500 
Sample size:                      119042 
Number of independent variables:  57 
Mtry:                             5 
Target node size:                 10 
Variable importance mode:         impurity 
OOB prediction error:             0.03501358 

Gini 
0.02631155 

kaggle
0.205