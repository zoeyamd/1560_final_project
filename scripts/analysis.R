#************************************* ANALYSIS ******************************#
#                                                                             #
#                                                                             #
# This code conducts analyses on summarized data.                             #
#*****************************************************************************#

#' Analyze correlation between trip frequency and ridership
#' 
#' @description This function tests whether there is a correlation between
#' trip frequency x ridership and identifies which pairs are significant
#' 
#' @param joined data frame. your joined frequency ridership df
#' @param ridership column name for your ridership variable 
#' @param frequency column name for your trip frequency variable 
#' 
#' @return list of anova result and post hoc results

anova_test <- function(joined, ridership, frequency){
  #set variable names as strings
  formula_text <- paste(ridership, "~", frequency)
  
  #run anova test
  anova_result <- aov(as.formula(formula_text), 
                      data = joined)
  
  #conduct post hoc analysis to identify difference between specific pairs
  post_hoc_result <- TukeyHSD(anova_result)
  
  return(list(anova = anova_result, post_hoc = post_hoc_result))
}


#' Analyze impact of trip frequency on on-time performance
#' 
#' @description This function tests for each type of OTP incident type (Late, 
#' Early, On-Time) to test whether trip frequency is a predictor of the volume of 
#' that incident type
#' 
#' @param joined data frame. your joined otp ridership df
#' @param incident_cols character vector. list of incident column names
#' 
#' @return list of lm summary for each incident type

lm_model <- function(joined, incident_cols){
  all_results <- c()
  
  #loop through each incident column
  for (col_name in incident_cols){
    formula_text <- paste(col_name, "~ Frequency_Category")
    
    #run linear model
    model <- lm(as.formula(formula_text), data = joined)
    
    #store model results
    all_results[[col_name]] <- summary(model)
  }
  return(all_results)
}

