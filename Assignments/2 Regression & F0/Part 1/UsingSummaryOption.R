
require(XLConnect)
df = loadWorkbook("ChemicalProcessData.xlsx")
ChemicalProcessData = readWorksheet(df, sheet = "ChemicalProcessData", header = TRUE)
ChemicalProcessData
plot(ChemicalProcessData)
results=lm(Yield~Factor_1+Factor_2, data= ChemicalProcessData)
results
summary(results)