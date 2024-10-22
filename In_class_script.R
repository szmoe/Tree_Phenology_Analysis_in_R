## 22 Oct 2024

library(chillR)
Winters_hours_gaps

# Call different columns/ cells
hourtemps <- Winters_hours_gaps[1,]
hourtemps
Winters_hours_gaps[1,1]
Winters_hours_gaps[,1]
Winters_hours_gaps[1,"Temp"]
Winters_hours_gaps$Hour[5]

# combine different columns with c- command
a <- c(1,2,3,4,5,6) # a vector
hourtemps <- Winters_hours_gaps[, c("Year", 
                                    "Month", 
                                    "Day", 
                                    "Hour", 
                                    "Temp")]
hourtemps

# Compare numbers and check
4 > 0
4 < 7.2
4 == 5
4 >= 4

a <- 5
a > 0

b <- c(-1, 0, 1, 7, 8, 10)
b > 0

# Check for days above certain temp
hourtemps$Temp > 0
hourtemps$Temp > 10

# Add new column
hourtemps[,"Chilling_Hour"] <- hourtemps$Temp > 10
hourtemps

# Add new column of chilling hour (0-7.2)
hourtemps[,"Chilling_Hour"] <- hourtemps$Temp > 0 & 
                               hourtemps$Temp <= 7.2

hourtemps  

# Count by using sum- command
sum(hourtemps$Chilling_Hour) # counts of true cases
sum(hourtemps$Chilling_Hour[c(10:200)]) # sum of chilling hours for particular time period

# Define function
func1 <- function(x) x+1
func1(5)
func1(b)

func2 <- function(x)
{y <- x +1
 z <- y - 7
 return (z)
 }
func2(b)

func2 <- function(x)
{y <- x +1
z <- y - 7
z
}
func2(a)

# chillR function
CH <- function(THourly)
{THourly[,"Chilling_Hour"] <- THourly$Temp > 0 & 
                              THourly$Temp <= 7.2
return(THourly)
}
CH
CH(hourtemps) # apply function to hourtemps

# Sum of chilling hours
CH_sum <- function(THourly)
{THourly[,"Chilling_Hour"] <- THourly$Temp > 0 &
                              THourly$Temp <= 7.2
CHs <- sum(THourly$Chilling_Hour)
return(CHs)}
CH_sum(hourtemps)


# Use which- command to see data for certain days
which(hourtemps$Day == 1) # for first day of the month
which(hourtemps$Year == 2008 &
        hourtemps$Month == 4 &
        hourtemps$Day == 1 & 
        hourtemps$Hour == 0) # first hour of the 1st of April, 2008

# OR
startYear <- 2008
startMonth <- 5
startDay <- 1
startHour <- 0

which(hourtemps$Year == startYear &
        hourtemps$Month == startMonth &
        hourtemps$Day == startDay & 
        hourtemps$Hour == startHour) 

CH_sum_interval <- function(THourly,
                         startYear,
                         startMonth,
                         startDay,
                         startHour,
                         endYear,
                         endMonth,
                         endDay,
                         endHour)
  
{THourly[,"Chilling_Hour"] <- THourly$Temp > 0 &
  THourly$Temp <= 7.2

Start_row <- which(hourtemps$Year == startYear &
                     hourtemps$Month == startMonth &
                     hourtemps$Day == startDay &
                     hourtemps$Hour == startHour)

End_row <- which(hourtemps$Year == endYear &
                     hourtemps$Month == endMonth &
                     hourtemps$Day == endDay &
                     hourtemps$Hour == endHour)

CHs <- sum(THourly$Chilling_Hour[Start_row:End_row])
return(CHs)
}

CH_sum_interval(hourtemps, 2008, 5, 1, 0,
                2008, 5, 31, 0) # need to know the order, the cleaner way is to assign
CH_sum_interval(hourtemps, 2008, 4, 1, 0,
                2008, 4, 30, 0)

# The cleaner way
CH_sum_interval(THourly = hourtemps,
                startYear = 2008,
                startMonth = 4,
                startDay = 1,
                startHour = 0,
                endYear = 2008,
                endMonth = 4,
                endDay = 30,
                endHour = 0)

