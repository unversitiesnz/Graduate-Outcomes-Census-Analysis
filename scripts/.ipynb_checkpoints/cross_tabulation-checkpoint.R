
cross_tabulate <- function(wb, worksheet_name, dataset, field_var, level_var, cell_value_var, cell_format
                           , col_names = list("No Qual", "Lvl 1", "Lvl 2", "Lvl 3", "Lvl 4", "Lvl 5-6", "Lvl 7", "Lvl 8", "Lvl 9", "Lvl 10")) {
    income_scale_range = c(min(dataset[[cell_value_var]], na.rm = TRUE), median(dataset[[cell_value_var]], na.rm = TRUE), max(dataset[[cell_value_var]], na.rm = TRUE))
    
    print(income_scale_range)
    cross_tabulation <- dataset %>% select(field_var, level_var, cell_value_var) %>% spread(level_var, cell_value_var)
    if (length(col_names) > 0) {
        colnames(cross_tabulation)[2:(1 + length(col_names))] <- col_names
    }
    
    print(head(cross_tabulation))

    openxlsx::writeData(wb,worksheet_name,cross_tabulation, borders = "surrounding", borderStyle = "medium", rowNames=FALSE)
    last_col <- ncol(cross_tabulation)
    last_row <- nrow(cross_tabulation) + 1
    first_data_row <- 2
    # first row and column bold
    addStyle(wb, worksheet_name, openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = 1:11, gridExpand = TRUE, stack=TRUE)
    # addStyle(wb, worksheet_name, openxlsx::createStyle(textDecoration = "bold"), rows = 2:(nrow(cross_tabulation) + 1), cols = 1, gridExpand = TRUE)
    # set readable widths:
    setColWidths(wb,  worksheet_name, cols = 1, widths = '45')
    #setColWidths(wb,  worksheet_name, cols = 2, widths = '15')
    setColWidths(wb,  worksheet_name, cols = 2:11, widths = '9')
    # set $ format
    addStyle(wb, worksheet_name, openxlsx::createStyle(numFmt = cell_format), rows = 2:last_row, cols = 2:last_col, gridExpand = TRUE, stack = TRUE)

    # set colour grant
    conditionalFormatting(wb, worksheet_name, cols = 2:last_col, rows = 2:last_row, rule = income_scale_range, type = "colourScale", style = c("#f8696b", "#ffeb84", "#63be7b"))

    # create borders:
    ## inner borders:
    addStyle(wb, worksheet_name, openxlsx::createStyle(border = c("top", "bottom")), rows = 1:(nrow(cross_tabulation) + 1), cols = 1:11, gridExpand = TRUE, stack = TRUE)
    ## thick borders:
    addStyle(wb, worksheet_name, openxlsx::createStyle(border = c("bottom"), borderStyle = "medium"), rows = c(9, 13, 24, 27, 34, 46, 50, 58, 71, 77), cols = 1:11, gridExpand = TRUE, stack = TRUE)
    
    addStyle(wb, worksheet_name, openxlsx::createStyle(border = c("right"), borderStyle = "medium"), rows = 1:(nrow(cross_tabulation) + 1), cols = 11, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, worksheet_name, openxlsx::createStyle(border = c("left"), borderStyle = "medium"), rows = 1:(nrow(cross_tabulation) + 1), cols = 1, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, worksheet_name, openxlsx::createStyle(border = c("top"), borderStyle = "medium"), rows = 1, cols = 1:11, gridExpand = TRUE, stack = TRUE)
    addStyle(wb, worksheet_name, openxlsx::createStyle(border = c("bottom"), borderStyle = "medium"), rows = (nrow(cross_tabulation) + 1), cols = 1:11, gridExpand = TRUE, stack = TRUE)
    freezePane(wb, worksheet_name, firstActiveRow = 2,  firstActiveCol = 2)
    return(c(col = last_col, row = last_row))
}

# adds a level summary below a sheet
addLevelIncomeSummary <- function(wb, worksheet_name, dataset, level_var, cell_value_var, start_row) {
    level_summary <- dataset %>% group_by_at(level_var) %>% 
        rename(to_mean = cell_value_var) %>% 
        summarise(average_income = mean(to_mean, na.rm = TRUE)) %>%
        mutate(increase_with_level = (average_income-lag(average_income)) / lag(average_income)) %>%
        transpose(make.names=level_var)
    rownames(level_summary) <- c("AVERAGES", "Increase from preceding qual level")
    
    #start_row <- 81
    openxlsx::writeData(wb,worksheet_name,level_summary, startRow = start_row, startCol = 1, borders = "surrounding", borderStyle = "medium", rowNames=TRUE, colNames=FALSE)
    # set styles for level averages
    addStyle(wb, worksheet_name, openxlsx::createStyle(numFmt = "$ ##,##0", textDecoration = "bold"), rows = start_row, cols = 2:11, gridExpand = TRUE, stack=TRUE)
    addStyle(wb, worksheet_name, openxlsx::createStyle(numFmt = "0%", textDecoration = "bold"), rows = start_row + 1, cols = 2:11, gridExpand = TRUE, stack=TRUE)
    addStyle(wb, worksheet_name, openxlsx::createStyle(textDecoration = "bold"), rows = start_row:(start_row + 1), cols = 1, gridExpand = TRUE, stack=TRUE)
}

#! adds worksheet
cross_tabulate_income <- function(wb, worksheet_name, dataset, field_var, level_var, cell_value_var) {
    addWorksheet(wb, worksheet_name)
    last_location <- cross_tabulate(wb, worksheet_name, dataset, field_var, level_var, cell_value_var, "$ ##,##0")
    addLevelIncomeSummary(wb, worksheet_name, dataset, level_var, cell_value_var, last_location["row"] + 1)
}