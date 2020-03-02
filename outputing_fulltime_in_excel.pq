/*
This is power query M: https://docs.microsoft.com/en-us/powerquery-m/
This code is intended to be used to load data into excel
*/
let
    Source = Sql.Database(".\SQLEXPRESS", "GO_Analysis_Data"),
    data = Source{[Schema="tab",Item="<<REPLACE WITH TABLE NAME>>"]}[Data], /* replace the text with whatever the table is called */
    #"Filtered Rows" = Table.SelectRows(data, each ([employed_code] = 1)), /* change employed code to go between fulltime (1), all (2) and part-time (3) */
    #"Removed Columns" = Table.RemoveColumns(#"Filtered Rows",{"employed_code"}),
    #"Sorted Rows" = Table.Sort(#"Removed Columns",{{"Field_Text", Order.Ascending}}),
    #"Renamed Columns" = Table.RenameColumns(#"Sorted Rows",{{"lvl 11", "lvl OS"}})
in
    #"Renamed Columns"