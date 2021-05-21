#here are all the packages that we need to install from CRAN

# mirror  = 'http://cran.us.r-project.org'

packages = c("cachem", "devtools","checkpoint","rJava",
             "tidyverse","ggthemes","corrgram","gridExtra",
             "rmarkdown","summarytools",
             "foreign","haven","expss", "xlsx",
             "hms","lubridate","Hmisc",
             "leaps","MASS", "glmulti")

install.packages(packages)

#sometimes devtools throws errors, on windows this seems to help
# install.packages("devtools", , type = "win.binary")


