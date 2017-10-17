library("XML")
library("methods")
result <- xmlParse(file = "C:\\resulQuery.xml")
df <- xmlToDataFrame("C:\\resulQuery.xml")
o_e <- df[ , "Original_Estimate"]
t_s <- df[ , "Time_Spent"]