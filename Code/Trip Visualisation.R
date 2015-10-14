# Creating a function which will create a visualisation of all trip trajectories and a 
# faceted image of 12 random trip trajectories for a single driver. Check Visualisation
# folder for example outputs with driver 10.

tripvis = function(driver) {
        
        if (!(driver %in% list.files("kaggle/Axa/drivers/"))) {
                "Not a valid number"
                break
        }
        
        library(ggplot2)
        
        dirPath = paste0("kaggle/Axa/drivers/", driver)
        tripData = list.files(dirPath)
        df = data.frame()
        
        for (i in 1:length(tripData)) {
                # load csv file
                data = read.csv(paste0(dirPath, "/", tripData[i]), stringsAsFactors=FALSE)
                # add which trip number it is to a coloumn
                data$trip = as.character(i)
                # add to dataframe
                df = rbind(df, data)
        }
        
        a = ggplot(df, aes(x,y, colour=trip)) + geom_point(size=.8) + ggtitle(paste0("Trip Trajectories for driver ", driver)) + 
            guides(colour = FALSE)
        b = ggplot(df[df$trip == sample(200,12),], aes(x,y, group=trip)) + geom_point() + facet_wrap(~trip) + geom_line() + 
            ggtitle(paste0("12 random trip Trajectories for driver ", driver))
        c = list(a,b)
        c
        
}
