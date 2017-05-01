

## this functionality is based on the drat package
## https://github.com/eddelbuettel/drat
## it is licensed under GPL-3
r <- getOption("repos")
r["declaredesign"] <- paste0("http://declaredesign.github.io/")
options(repos = r)

install.packages("estimatr")
install.packages("fabricatr")
rinstall.packages("randomizr")
install.packages("DeclareDesign")
