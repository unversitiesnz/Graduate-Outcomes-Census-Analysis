library(readxl)

connect_to_GO_analysis_database <- function() {
  return(DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server", 
                        Server = "localhost\\SQLEXPRESS",
                        Database = "GO_Analysis_Data",
                        Trusted_Connection = "True"))
}

# custom rounding function to do as expected.
# this function has been passed around a bit: http://janajarecki.com/blog/2014/09/18/r-does-not-round-2-5-to-3/
# it rounds the five value up. R rounds to the nearest even number.
round2 <- function(x, n) {
  posneg = sign(x)

  z = abs(x)*10^n

  z = z + 0.5

  z = trunc(z)

  z = z/10^n

  z * posneg
}

# function to convert number to ordinal value
nth <- function(v, first_value, second_value)
{
    n = as.integer(v)
    if (length(n) > 1) return(sapply(n, nth, first_value = first_value, second_value = second_value))
    
    mod <- function(m, n) ifelse(!(m%%n), n, m%%n)
    suffices <- c("th", "st", "nd", "rd", "th", "th", "th", "th", "th", "th")
    if (n == 1) {
        return (first_value)
    } else if (n == 2) {
        return (second_value)
    } else if (n %% 100 <= 10 || n %% 100 > 20) {
        suffix <- suffices[mod(n+1, 10)]
    }
    else {
        suffix <- 'th'
    }

    paste(n, suffix, sep="")
}

# classification helpers
append_classification_code <- function(dataset, txt_var, code_var, classification_file) {
    field_narrow <- read_excel(classification_file, na = "NA", skip=7)
    field_narrow
    print(names(field_narrow))
    print(names(dataset))
    join_by <- "Descriptor"
    names(join_by) <- txt_var
    data2 <- dataset %>% left_join(field_narrow, by=join_by)
    names(data2)[names(data2) == "Code"] <- code_var
    
    return(data2)
}


append_narrow_field_of_study_code <- function(.data, ..., field_var, code_var = paste0(field_var,"_code")) {
    return(append_classification_code(.data, field_var, code_var, "../metadata/NZSCED_narrow_cen.xlsx"))
}

append_broad_field_of_study_code <- function(.data, ..., field_var, code_var = paste0(field_var,"_code")) {
    return(append_classification_code(.data, field_var, code_var, "../metadata/NZSCED_broad_cen.xlsx"))
}
append_study_level_code <- function(.data, ..., level_var, code_var = paste0(level_var,"_code")) {
    return(append_classification_code(.data, level_var, code_var, "../metadata/Cen_Highest_Qaulification.xlsx"))
}
append_ANZSCO_L2_code <- function(.data, ..., ANZSCO_var, code_var = paste0(ANZSCO_var,"_code")) {
    return(append_classification_code(.data, ANZSCO_var, code_var, "../metadata/AZNSCO_Level2.xlsx"))
}

append_ANZSCO_L5_code <- function(.data, ..., ANZSCO_var, code_var = paste0(ANZSCO_var,"_code")) {
    return(append_classification_code(.data, ANZSCO_var, code_var, "../metadata/ANZSCO_Level5.xlsx"))
}

# text classification helpers
append_classification_txt <- function(dataset, code_var, txt_var, classification_file) {
    field_narrow <- read_excel(classification_file, na = "NA", skip=7)
    field_narrow
    print(names(field_narrow))
    print(names(dataset))
    join_by <- "Code"
    names(join_by) <- code_var
    data2 <- dataset %>% left_join(field_narrow, by=join_by)
    names(data2)[names(data2) == "Descriptor"] <- txt_var
    
    return(data2)
}


append_narrow_field_of_study_txt <- function(.data, ..., code_var, field_var = paste0(field_var,"_text")) {
    return(append_classification_txt(.data, code_var, field_var, "../metadata/NZSCED_narrow_cen.xlsx"))
}
append_broad_field_of_study_txt <- function(.data, ..., code_var, field_var = paste0(field_var,"_text")) {
    return(append_classification_txt(.data, code_var, field_var, "../metadata/NZSCED_broad_cen.xlsx"))
}

append_ANZSCO_L5_txt <- function(.data, ..., code_var, ANZSCO_var = paste0(ANZSCO_var,"_text")) {
    return(append_classification_txt(.data, code_var, ANZSCO_var, "../metadata/ANZSCO_Level5.xlsx"))
}