library("XML")
library("methods")
result <- xmlParse(file = "C:\\resulQuery.xml")
df <- xmlToDataFrame("C:\\resulQuery.xml")