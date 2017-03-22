
library(pdftools)
library(data.table)
txt <- pdf_text(pdf = "C:/Users/afiq.johari/Desktop/text-mining-ceta-trade-agreement/tradoc_152806.pdf")
txt <- unlist(txt)
txt <- paste(txt, collapse = " ")
