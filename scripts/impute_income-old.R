library(tidyverse)
library(readxl)
library(openxlsx)

source("../scripts/unz_common.R")

# data: a dataset containing income
# must have format: Field, Level, Age, people_count, mean_income
impute_income <- function(data, dump_name = "inc_data") {
    # Decide if we should impute for each given field/level group?
    ## count the missing -> not really used?
    missing_count <- data %>% group_by(Field, Level) %>%
        summarise(missing_count = sum(is.na(mean_income)))
    head(missing_count)

    ## convert income data to wide with age as columns:
    wide_form <- data %>% select(-c("people_count")) %>% spread(Age, mean_income)
    #print(wide_form)
    print(colnames(wide_form))
    colnames(wide_form)[4:14] <- c('age_15_19', 'age_20_24', 'age_25_29', 'age_30_34', 'age_35_39', 'age_40_44', 'age_45_49', 'age_50_54', 'age_55_59', 'age_60_64', 'age_65_')
    print(colnames(wide_form))
    head(wide_form)

    ## make the call on what can be imputed:
    should_impute <- wide_form %>% left_join(missing_count, by=c("Field", "Level")) %>% 
        mutate(
               can_impute = !
                (# Can't impute if there is a run of four missing between 20 and 54
                    (is.na(age_20_24) & is.na(age_25_29) & is.na(age_30_34) & is.na(age_35_39) & is.na(age_40_44)) |
                    (is.na(age_25_29) & is.na(age_30_34) & is.na(age_35_39) & is.na(age_40_44)) |
                    (is.na(age_30_34) & is.na(age_35_39) & is.na(age_40_44) & is.na(age_45_49)) |
                    (is.na(age_35_39) & is.na(age_40_44) & is.na(age_45_49) & is.na(age_50_54)) |
                    # Can't impute if there is a run of three missing between 50 and 65
                    (is.na(age_50_54) & is.na(age_55_59) & is.na(age_60_64)) |
                    (is.na(age_55_59) & is.na(age_60_64) & is.na(age_65_))
                )
              )
    head(should_impute)

    # Do imputation:
    ## prep fuller for age 15-19 by level
    level_15_19_mean_incomes <- data %>% filter(Age == "15-19 years" & !is.na(mean_income)) %>% 
        group_by(level_code) %>% 
        summarise(weighted_mean_income = weighted.mean(mean_income, people_count))
    print(level_15_19_mean_incomes)
    ### imputation value for level 7+
    higher_level_early_income_impute <- (level_15_19_mean_incomes %>% summarise(v = max(weighted_mean_income)))$v
    higher_level_early_income_impute
    ### assign level 7 - 10 imputation values
    higher_level_imputes <- tibble(level_code = c('10'))
    higher_level_imputes["weighted_mean_income"] <- higher_level_early_income_impute
    higher_level_imputes
    ### bind together the level 1 - 6 dataset with level 7 - 10
    age_15_19_imputable_income <- rbind(level_15_19_mean_incomes[,c("level_code","weighted_mean_income")], higher_level_imputes) %>%
        mutate(mean_income = round2(weighted_mean_income, -2)) %>%
        select(-c("weighted_mean_income"))
    age_15_19_imputable_income
    ##? join age 15 - 19 income dataset with wide income dataset
    ready_to_impute <- should_impute %>% 
        left_join(age_15_19_imputable_income, by="level_code") %>%
        rename(age_15_19_impute_income = mean_income)
    head(ready_to_impute)
    saveRDS(ready_to_impute,paste0("../datasets/dump_pre_impute_",dump_name,".rds"))

    ## Do actual imputation:
    impute_value = function(row) {
        if (row["can_impute"] == "TRUE" && as.numeric(row["missing_count"]) > 0) {
            # first age group imputation:
            if (is.na(row["age_15_19"])) {
                row["age_15_19"] <- row["age_15_19_impute_income"] 
            }
            # impute last age group:
            if (is.na(row["age_65_"])) {
                if (!is.na(row["age_60_64"])) {
                    row["age_65_"] <- row["age_60_64"]
                } else if (!is.na(row["age_55_59"])) {
                    row["age_65_"] <- row["age_55_59"]
                } else {
                    warning("To much missing information to impute last value")
                }
                row["age_65_"] <- row["age_15_19_impute_income"] 
            }
            for(age_group in 5:13) {
                if(is.na(row[age_group])) {
                    if (!is.na(row[age_group + 1])) {
                        row[age_group] <- mean(as.numeric(row[(age_group - 1)]), as.numeric(row[(age_group + 1)]))
                    } else if (age_group + 2 <= 14 && !is.na(row[age_group + 2])) {
                        row[age_group] <- round2(as.numeric(row[(age_group - 1)]) + (1/3) * as.numeric(row[(age_group + 2)]) - as.numeric(row[(age_group - 1)]),-2) 
                    } else if (age_group + 3 <= 14 && !is.na(row[age_group + 3])) {
                        row[age_group] <- round2(as.numeric(row[(age_group - 1)]) + (1/4) * as.numeric(row[(age_group + 3)]) - as.numeric(row[(age_group - 1)]),-2)
                    } else if (age_group + 4 <= 14 && !is.na(row[age_group + 4])) {
                        row[age_group] <- round2(as.numeric(row[(age_group - 1)]) + (1/5) * as.numeric(row[(age_group + 4)]) - as.numeric(row[(age_group - 1)]),-2) 
                    } else if (age_group + 5 <= 14 && !is.na(row[age_group + 5])) {
                        row[age_group] <- round2(as.numeric(row[(age_group - 1)]) + (1/6) * as.numeric(row[(age_group + 5)]) - as.numeric(row[(age_group - 1)]),-2) 
                    } else {
                        warning("To much missing information to impute ", row["Field"], row["level_code"], age_group)
                        row[age_group] <- 0
                    }
                }

            }
        }
        return(row)
    }
    annual_income_with_imputation <- apply(ready_to_impute, 1, impute_value) %>% t %>% as_tibble %>% 
        mutate_at(vars('age_15_19', 'age_20_24', 'age_25_29', 'age_30_34', 'age_35_39', 'age_40_44', 'age_45_49', 'age_50_54', 'age_55_59', 'age_60_64', 'age_65_'), funs(as.integer))
    head(annual_income_with_imputation)
    return (annual_income_with_imputation)
    
}