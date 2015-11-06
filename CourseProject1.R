# Transform path backslashes to forward slashes
back.to.slash <- function(x) {gsub(" \\ ", "/", x)}
path2 <- back.to.slash(path)
path2

pa1_dat <- read.table(path2, TRUE, ";")

head(pa1_dat)
class(pa1_dat$Date)
pa1_dat$Date <- as.Date(pa1_dat$Date, format = "%d / %m / %Y")
## Problem with conversion from string to time. Not performed.
pa1_dat$Time <- as.POSIXct(pa1_dat$Time, format = "%H : %M : %S")

# I try to create a function which can isolate part of the dataframe according to two inputs.
sub_dates <- function(x, y) {pa1_dat[pa1_dat$Date >= x & pa1_dat$Date <= y,]}

## Subset according to dates from 2007-02-01 to 2007-02-02.
pa1_dat <- sub_dates("2007-02-01", "2007-02-02")
gc()
head(pa1_dat)
sum(is.na(pa1_dat))

## Plot 1
hist(as.integer(pa1_dat$Global_active_power), main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = 2)
str(pa1_dat$Global_active_power)
example("points")
