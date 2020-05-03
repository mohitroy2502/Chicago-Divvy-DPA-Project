
## Script to run in plumber our MD2.R script
# install.packages("plumber")
library(plumber)
r <- plumb("C:/Users/manis/Desktop/MD2.R")  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)


# C:\Users\manis\Desktop



