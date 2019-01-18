# Sam Tenney
# US Residential Energy Consumption
# https://www.eia.gov/totalenergy/data/browser/?tbl=T02.02#/?f=M&start=199101&end=201809&charted=2-3-4-10-12

data1 <- read.csv("MER_T02_02.csv", header = TRUE, stringsAsFactors = FALSE)

data1$Value <- suppressWarnings(as.numeric(data1$Value))

# Subset to TERCBUS Total Energy Consumed by the Residential Sector
data2 <- subset(data1, MSN=="TERCBUS")

# Subset to "your lifetime"
data3 <- subset(data2, data2$YYYYMM>199100)

# Remove yearly total (coded "month 13", every 13th obs)
data4 <- subset(data3, data3$YYYYMM%%100 != 13)

energy <- data4$Value
tail(energy)

# EDA: Plot the time sereis to see if there's a monthly pattern
plot(energy, type="b")

# ANALYSIS

# Additive (see no evidence of curvature) 
# Constant mean change (see no evidence of changing trend)

# Model: Seasonal ARIMA(1,1,1)x(1,1,1)_12
library(astsa)
# ARIMA (long memory, short memory, difference)
out_energy <- sarima(energy, 1,1,1, 1,1,1,12)

# Forecast next 2 years
future_energy <- sarima.for(energy, n.ahead = 24, 1,1,1, 1,1,1,12)
title(main = "Monthly US Residential Energy Consumption")

