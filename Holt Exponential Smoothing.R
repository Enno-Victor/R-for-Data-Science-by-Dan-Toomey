#Holt exponential smoothing

sleep <- read.table("http://physionet.org/physiobank/database/santa-fe/b2.txt")
colnames(sleep) <- c("heart","chest","oxygen")
head(sleep)
sleepts <- ts(sleep)
plot.ts(sleepts)
#There is a period where the chest expansion stopped and the patient stopped breathing for a few seconds—sleep apnea?
#There is a period where there was no reading for the heart rate or oxygen rate.


#generating forecasts for each of the variables 
heart.ts <- ts(sleep$heart)
heart.forecast <- HoltWinters(heart.ts, gamma=FALSE)
heart.forecast
#Heart rate average at 58. I think that is normal.
#Slight downward trend—again, I think that is normal as you get more into sleep mode.

plot(heart.forecast)


#For Chest 
chest.ts <- ts(sleep$chest)
chest.forecast <- HoltWinters(chest.ts, gamma=FALSE)
chest.forecast
plot(chest.forecast)
#•Looks like a very good fit to the data (red overlay only partially visible)

#For Oxygen 
oxygen.ts <- ts(sleep$oxygen)
oxygen.forecast <- HoltWinters(oxygen.ts, gamma=FALSE)
oxygen.forecast
plot(oxygen.forecast)
#The bad data from the slipped electrodes is distorting the results completely

