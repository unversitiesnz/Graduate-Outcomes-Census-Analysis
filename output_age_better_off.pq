let
    Source = Sql.Database(".\SQLEXPRESS", "GO_Analysis_Data"),
    data = Source{[Schema="tab",Item="cen_age_better_off"]}[Data], /* replace the text with whatever the table is called */
    #"Filtered Rows" = Table.SelectRows(data, each ([employed_code] = "1")), /* change employed code to go between fulltime (1), all (2) and part-time (3) */
    #"Removed Columns" = Table.RemoveColumns(#"Filtered Rows",{"employed_code"}),
    #"Sorted Rows" = Table.Sort(#"Removed Columns",{{"Field_Text", Order.Ascending}}),
    /*#"Renamed Columns" = Table.RenameColumns(#"Sorted Rows",{{"lvl 11", "lvl OS"}})*/
    #"Renamed Columns" = #"Sorted Rows",
    #"Changed Type" = Table.TransformColumnTypes(#"Renamed Columns",{{"lvl 04", type text}, {"lvl 05", type text}, {"lvl 07", type text}, {"lvl 08", type text}, {"lvl 09", type text}, {"lvl 10", type text}}),
    #"Replaced Value" = Table.ReplaceValue(#"Changed Type","99","Never",Replacer.ReplaceText,{"lvl 04", "lvl 05", "lvl 07", "lvl 08", "lvl 09", "lvl 10"})
in
    #"Replaced Value"