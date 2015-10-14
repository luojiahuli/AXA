# A simple Linear Discriminant Analysis model using only the features provided with the dataset.

library(MASS)
speedDistribution <- function(trip)
{
        vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
        return(quantile(vitesse, seq(0.05,1, by = 0.05)))
}

# Accessing driver list folder, and randomply choosing 5 drivers 
drivers = list.files("kaggle/Axa/drivers")
randomDrivers = sample(drivers, size = 200)

refData = NULL
target = 0
names(target) = "target"
for(driver in randomDrivers)
{
        # Creating path toward trips by driver from randomDrivers         
        dirPath = paste0("kaggle/Axa/drivers/", driver, '/')
        for(i in 1:200)
        {
                # pulling out individula trip        
                trip = read.csv(paste0(dirPath, i, ".csv"))
                # Applying calculation and concatinating with target
                features = c(speedDistribution(trip), target)
                # Combining by row refData and features 
                refData = rbind(refData, features)
        }
}

target = 1
names(target) = "target"
submission = NULL

for(driver in drivers)
{
        print(driver)
        dirPath = paste0("kaggle/Axa/drivers/", driver, '/')
        currentData = NULL
        for(i in 1:200)
        {
                trip = read.csv(paste0(dirPath, i, ".csv"))
                features = c(speedDistribution(trip), target)
                currentData = rbind(currentData, features)
        }
        train = rbind(currentData, refData)
        train = as.data.frame(train)
        g = lda(target ~ ., data=train)
        currentData = as.data.frame(currentData)
        p =predict(g, currentData, type = "response")
        q = p$posterior[,2]
        labels = sapply(1:200, function(x) paste0(driver,'_', x))
        result = cbind(labels, q)
        submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "kaggle/Axa/submissionLDA200.csv", row.names=F, quote=F)

