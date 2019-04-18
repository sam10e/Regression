# heart.R
# Sam Tenney

# get the data
fram <- read.csv("frmgham2.csv", header = TRUE)

# subset to participants that didn't have CHD
fram <- subset(fram, PERIOD == 1 & PREVCHD == 0)

# subset to risk factors under study
fram <- fram[,c(2:14,30)]

# filter out missing observations
fram <- na.omit(fram)

# create interpretable versions of all the coded explanatory variables
# Save only the interpretable versions of columns in the dataframe
colnames(fram) <- c("Sex", "TotalCholesterol", "Age", "SystolicBP", "DiastolicBP", "CurrentSmoker", "CigsPerDay", "BMI", "Diabetes", "BPMeds", "HeartRate", "CasualSerumGlucose", "Education", "CVD")

# change all factor variables to R factor objects with a reasonable comparison level
factorCols <- c(1, 6, 9, 10, 13)
fram[factorCols] <- lapply(fram[factorCols], as.factor)
str(fram)

# create training and test datasets
set.seed(7)
n.fram.rows <- dim(fram)[1]
train.rows <- sample(n.fram.rows, 3500)
fram.train <- fram[train.rows, ]
fram.test <- fram[-train.rows, ]

# Analysis
