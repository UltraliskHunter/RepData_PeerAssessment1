##part1

data <- read.csv("activity.csv",
                 colClasses = c("interval"="numeric", "date" = "character"))
splitdata <- split(data, data$date)
sums <- na.omit(lapply(splitdata, function(x) sum(x[,1])))

sums <- data.frame(stringAsFactors = F,
        date = rep(names(sums), lapply(sums, length)),
        totalsteps = unlist(sums))
qplot(as.Date(date), totalsteps, stat="identity", data = sums,
           geom = "bar", xlab = "Date", ylab = "Total Steps")

mean(sums$totalsteps, na.rm = T)
median(sums$totalsteps, na.rm = T)

##part2
intsplit <- split(data, data$interval)
means <- na.omit(lapply(intsplit, function(x) mean(x[,1], na.rm = T)))
means <- data.frame(stringsAsFactors = F, interval = rep(names(means), lapply(means, length)),
                    avesteps = unlist(means))
means$interval <- as.numeric(means$interval)
ggplot(data = means, aes(x = interval, y = avesteps)) + geom_line() + xlab("Interval") +
        ylab("Average Steps")

maxsteps <- max(means$avesteps, na.rm = T)
maxinterval <- subset(means, avesteps==maxsteps)
maxinterval
##part3 - na's changed to average number of steps over entire period
nas <- data[!complete.cases(data), ]
nrow(nas)

sums[is.na(sums)] = mean(data[,1], na.rm =T)

qplot(as.Date(date), totalsteps, stat="identity", data = sums,
      geom = "bar", xlab = "Date", ylab = "Total Steps")

mean(sums$totalsteps, na.rm = T)
median(sums$totalsteps, na.rm = T)
##part4
data$date <- as.Date(data$date, header = T)
Dow <- data.frame(Day = "character")
levels(Dow$Day) = c("Monday", "Tuesday", "Wednesday", "Thursday",
                    "Friday", "Saturday", "Sunday")
for(i in 1:nrow(data)){
        dayval <- weekdays(data$date[i])
Dow <- rbind(Dow, dayval)
}

Pow <- data.frame(Part = "character", header = T)
levels(Pow$Part) = c("weekday", "weekend")
for(i in 1:nrow(data)){
        Powval <- Dow[i,1]
        if(Powval == "Monday"){
                Pow <- rbind(Pow, "weekday")
        }
        if(Powval == "Tuesday"){
                Pow <- rbind(Pow, "weekday")
        }
        if(Powval == "Wednesday"){
                Pow <- rbind(Pow, "weekday")
        }
        if(Powval == "Thursday"){
                Pow <- rbind(Pow, "weekday")
        }
        if(Powval == "Friday"){
        Pow <- rbind(Pow, "weekday")
        }
        if(Powval == "Saturday"){
                Pow <- rbind(Pow, "weekend")
        }
        if(Powval == "Sunday"){
        Pow <- rbind(Pow, "weekend")
        }
}

datadays <- cbind(data, Pow)