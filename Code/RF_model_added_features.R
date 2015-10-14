# A Random Forest model with added features. 

start = Sys.time()
library(randomForest)

speedDistribution <- function(trip)
{
        vitesse = 3.6*sqrt(diff(trip$x,20,1)^2 + diff(trip$y,20,1)^2)/20
        speed_dist = quantile(vitesse, seq(0.05,1, by = 0.05))
        names(speed_dist) = c("5_speed", "10_speed", "15_speed", "20_speed", "25_speed", "30_speed", 
                              "35_speed", "40_speed", "45_speed", "50_speed", "55_speed", "60_speed", 
                              "65_speed", "70_speed", "75_speed", "80_speed", "85_speed", "90_speed", 
                              "95_speed", "100_speed")
        return(speed_dist)
}

travelDistPerc = function(trip) {
        distance = sqrt(diff(trip$x, 1)^2 + diff(trip$y, 1)^2)
        travel_dist_perc = quantile(distance, seq(0.00,1, by = 0.1))
        names(travel_dist_perc) = c("0_tpc", "10_tpc", "20_tpc", "30_tpc", "40_tpc",
                                    "50_tpc", "60_tpc", "70_tpc", "80_tpc", "90_tpc", "100_tpc")
        return(travel_dist_perc)
}

acceleration = function(trip) {
        distance = sqrt(diff(trip$x, 5)^2 + diff(trip$y, 5)^2)
        acc_dec = diff(distance)
        acc = acc_dec[acc_dec >= 0]
        acc_dist = quantile(acc, seq(0.00,1, by = 0.1))
        names(acc_dist) = c("0_acc", "10_acc", "20_acc", "30_acc", "40_acc",
                            "50_acc", "60_acc", "70_acc", "80_acc", "90_acc", "100_acc")
        return(acc_dist)
}

deceleration = function(trip) {
        distance = sqrt(diff(trip$x, 1)^2 + diff(trip$y, 1)^2)
        acc_dec = diff(distance)
        dec = acc_dec[acc_dec < 0]
        dec_dist = quantile(dec, seq(0.00,1, by = 0.1))
        names(dec_dist) = c("0_decc", "10_decc", "20_decc", "30_decc", "40_decc",
                            "50_decc", "60_decc", "70_decc", "80_decc", "90_decc", "100_decc")
        return(dec_dist)
}

accdecDist = function(trip) {
        distance = sqrt(diff(trip$x, 1)^2 + diff(trip$y, 1)^2)
        acc_dec = diff(distance)
        distr = c()
        # distr = rep(0, length(acc_dec))
        distra = c()
        distra[1] = "-4+"
        distra[2] = "-4 to -3"
        distra[3] = "-3 to -2"
        distra[4] = "-2 to -1"
        distra[5] = "-1 to 0"
        distra[6] = "0 to 1"
        distra[7] = "1 to 2"
        distra[8] = "2 to 3"
        distra[9] = "3 to 4"
        distra[10] = "4+"
        
        distr[acc_dec < -4] = "-4+"
        distr[acc_dec > -4 & acc_dec <= -3] = "-4 to -3"
        distr[acc_dec > -3 & acc_dec <= -2] = "-3 to -2"
        distr[acc_dec > -2 & acc_dec <= -1] = "-2 to -1"
        distr[acc_dec > -1 & acc_dec <= 0] = "-1 to 0"
        distr[acc_dec > 0 & acc_dec <= 1] = "0 to 1"
        distr[acc_dec > 1 & acc_dec <= 2] = "1 to 2"
        distr[acc_dec > 2 & acc_dec <= 3] = "2 to 3"
        distr[acc_dec > 3 & acc_dec <= 4] = "3 to 4"
        distr[acc_dec > 4] = "4+"
        # distr = as.factor(distr)
        fdist = c(distr, distra)
        fdist = table(fdist)
        return(fdist)
}

cornerDist = function(trip) {
        # Cornering Distribution
        angle = atan(diff(trip$y,5)/diff(trip$x,5))*(180/pi)
        trip2 = trip[2:nrow(trip),]
        corners5 = atan(diff(trip2$y,5)/diff(trip2$x,5))*(180/pi) - atan(diff(trip$y,5)/diff(trip$x,5))*(180/pi)
        corners5 = na.omit(corners5)
        
        distrac = c()
        distrc = c()
        
        distrac[1] = "0 to 45"
        distrac[2] = "45+ to 90"
        distrac[3] = "90+ to 135"
        distrac[4] = "135+"
        distrac[5] = "0 to -45"
        distrac[6] = "-45+ to -90"
        distrac[7] = "-90+ to -135"
        distrac[8] = "-135+"
        
        distrc[corners5 >= 0 & corners5 <= 45] = "0 to 45"
        distrc[corners5 > 45 & corners5 <= 90] = "45+ to 90"
        distrc[corners5 > 90 & corners5 <= 135] = "90+ to 135"
        distrc[corners5 > 135] = "135+"
        distrc[corners5 < 0 & corners5 >= -45] = "0 to -45"
        distrc[corners5 < -45 & corners5 >= -90] = "-45+ to -90"
        distrc[corners5 < -90 & corners5 >= -135] = "-90+ to -135"
        distrc[corners5 < -135] = "-135+"
        
        fdistrc = c(distrac, distrc)
        fdistrc = table(fdistrc)
        return(fdistrc)
}

cornerQuant = function(trip) {
        angle = atan(diff(trip$y,5)/diff(trip$x,5))*(180/pi)
        trip2 = trip[2:nrow(trip),]
        corners5 = atan(diff(trip2$y,5)/diff(trip2$x,5))*(180/pi) - atan(diff(trip$y,5)/diff(trip$x,5))*(180/pi)
        corner_quant = quantile(corners5, seq(0.00,1,by=0.1), na.rm=TRUE)
        names(corner_quant) = c("0_corn", "10_corn", "20_corn", "30_corn", "40_corn", "50_corn",
                                "60_corn", "70_corn", "80_corn", "90_corn", "100_corn")
        return(corner_quant)
}

cornerQuantPercStats = function(trip) {
        angle = atan(diff(trip$y,5)/diff(trip$x,5))*(180/pi)
        trip2 = trip[2:nrow(trip),]
        corners5 = atan(diff(trip2$y,5)/diff(trip2$x,5))*(180/pi) - atan(diff(trip$y,5)/diff(trip$x,5))*(180/pi)
        # corners5 = corners5[!is.na(corners5)]
        corner_quant = quantile(corners5, seq(0.00,1,by=0.1), na.rm=TRUE)
        corner_quant_mean = tapply(corners5, findInterval(corners5, corner_quant), mean)
        a = c()
        a[1] = corner_quant_mean[1]
        a[2] = corner_quant_mean[length(corner_quant_mean)]
        if(length(corner_quant_mean) %% 2 == 0) {
                a[3] = (corner_quant_mean[(length(corner_quant_mean)/2)+1] + 
                                corner_quant_mean[(length(corner_quant_mean)/2)]) / 2
        }
        
        if(length(corner_quant_mean) %% 2 != 0) {
                a[3] = corner_quant_mean[ceiling(length(corner_quant_mean)/2)]
        }
        
        a[4] = mean(corner_quant_mean)
        a[5] = sd(corner_quant_mean)
        
        if (is.na(a[5])) {
                a[5] = 0
        }
        
        names(a) = c("FirstQM", "LastQM", "MiddleQM", "MeanQM", "SDQM")
        return(a)
}

set.seed(999)

# Accessing driver list folder, and randomply choosing 5 drivers 
drivers = list.files("kaggle/Axa/drivers")
randomDrivers = sample(drivers, size = 5)

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
                features = c(speedDistribution(trip), acceleration(trip), deceleration(trip), cornerQuant(trip),
                             accdecDist(trip), cornerQuantPercStats(trip), cornerDist(trip), travelDistPerc(trip), target)
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
                features = c(speedDistribution(trip), acceleration(trip), deceleration(trip), cornerQuant(trip),
                             accdecDist(trip), cornerQuantPercStats(trip), cornerDist(trip), travelDistPerc(trip), target)
                currentData = rbind(currentData, features)
        }
        #         currentData = as.data.frame(currentData)
        #         currentData$target = 1
        refData = as.data.frame(refData)
        refData$target = 0
        train = rbind(currentData, refData)
        train = as.data.frame(train)
        train$target = as.factor(train$target)
        names(train) = paste0("n",names(train))
        g = randomForest(y=as.factor(train[,ncol(train)]), x= train[,1:(ncol(train)-1)], do.trace=100, importance = T)
        currentData = as.data.frame(currentData)
        names(currentData) = paste0("n",names(currentData))
        p =predict(g, currentData, type = "prob")
        p = p[,2]
        labels = sapply(1:200, function(x) paste0(driver,'_', x))
        result = cbind(labels, p)
        submission = rbind(submission, result)
}

colnames(submission) = c("driver_trip","prob")
write.csv(submission, "kaggle/Axa/submissionRF.csv", row.names=F, quote=F)

end = Sys.time()