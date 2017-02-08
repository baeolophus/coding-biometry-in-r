library(knitr)
library(markdown)

knit("biometry_ch6.Rmd") # produces a .md file
markdownToHTML("biometry_ch6.md", "biometry_ch6.html", 
               fragment.only=TRUE)
#from http://rstudio-pubs-static.s3.amazonaws.com/30704_583c8c8e2b254bf6a0791543d4a3438b.html
