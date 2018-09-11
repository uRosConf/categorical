

d <- read.csv("data_src/NACE_REV2_20180910_105408.csv", encoding = "UTF-8", 
  stringsAsFactors = FALSE)
str(d)
nace_rev2 <- as_classification(d[c("Code", "Description", "Level", "Parent")])
save(nace_rev2, file = "data/nace_rev2.rdata")
