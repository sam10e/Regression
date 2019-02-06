# Sam Tenney
# Simple Linear Regression

# Chapter 2, exercise 1c
# Create a dataset using the data from playbill.csv found at http://gattonweb.uky.edu/sheather/book/
playbill <- read.csv(header = TRUE, sep = ",", stringsAsFactors = TRUE, text = '
Production,CurrentWeek,LastWeek
42nd Street,684966,695437
Avenue Q,502367,498969
Beauty and Beast,594474,598576
Bombay Dreams,529298,528994
Chicago,570254,562964
Dracula,319959,282778
Fiddler on the Roof,579126,583177
Forever Tango,134042,152833
Goldas Balcony,105853,105698
Hairspray,822775,836959
Mamma Mia!,949462,970190
Movin Out,610007,651808
Rent,386797,378238
The Lion King,1133034,1113510
The Phantom of the Opera,627609,614246
The Producers,911727,933822
Wicked,1180266,1202536
Wonderful Town,479155,488624'
)

# Fit the model CurrentWeek = beta0 + beta1 LastWeek + Epsilon
playbill_out <- lm(CurrentWeek ~ LastWeek, data = playbill)
summary(playbill_out)

# Predict the gross box office results for the current week for 
# a production with $400,000 the previous week
predict(playbill_out, newdata = data.frame(LastWeek = 400000), interval = "prediction")

# Is $450,000 feasible for the current week, given $400,000 in the previous week?
# No, because not even the upper bound is higher than $450,000.


