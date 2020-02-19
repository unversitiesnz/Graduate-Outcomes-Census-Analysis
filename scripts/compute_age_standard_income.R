compute_age_standard_income <- function(data, distribution_dataset_name) {
    # Read age distribution dataset
    age_distribution <- readRDS(paste0('../datasets/',distribution_dataset_name,'.rds'))
    age_distribution
    # calculate
    age_standard_annual_income <- data %>% 
        mutate(age_standard_annual_income = round2(
               age_25_29 * age_distribution[age_distribution$Age == "25-29 Years",]$proportion +
               age_30_34 * age_distribution[age_distribution$Age == "30-34 Years",]$proportion +
               age_35_39 * age_distribution[age_distribution$Age == "35-39 Years",]$proportion +
               age_40_44 * age_distribution[age_distribution$Age == "40-44 Years",]$proportion +
               age_45_49 * age_distribution[age_distribution$Age == "45-49 Years",]$proportion +
               age_50_54 * age_distribution[age_distribution$Age == "50-54 Years",]$proportion +
               age_55_59 * age_distribution[age_distribution$Age == "55-59 Years",]$proportion +
               age_60_64 * age_distribution[age_distribution$Age == "60-64 Years",]$proportion +
               age_65_ * age_distribution[age_distribution$Age == "65 Years And Over",]$proportion
              ,-2)) 
    return(age_standard_annual_income)
}