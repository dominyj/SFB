# SFB - an R package that plots sectoral financial balances using the AMECO dataset
This is a small R packages that plots sectoral financial balances of countries in the AMECO data set. 

# Installation
You can install this package using the devtools package. 
```
install.packages("devtools")
devtools::install_github("jbschulte/SFB")
```

# How to use
Get a plot of financial sectoral balances of a country or list of countries:
```
library(SFB)
sfb("Germany")
sfb(c("Germany", "Spain", "United States"))
```
Specify time period:
```
sfb("Germany", t_start = 1995, t_end=2015)
```
You can also output the data table with or without the plots:
```
sfb("Germany", getData = TRUE)
sfb("Germany", getData = TRUE, getPlot = FALSE)
```


