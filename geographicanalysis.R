
# Load packages
suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

# Reduce the number of columns 
generate.choropleth.maps <- function(df){
  
  rlist <- c("Unit.ID", "City", "State", "CohortDefaultRate", 
             "LONGITUD", "LATITUDE")
  
  
  df_subset <- df[,rlist]
  # LOOK INTO THIS FURTHER - df_subset$avgDefaultRate <- mean(df_subset$CohortDefaultRate2013+ df_subset$CohortDefaultRate2012)
  
  #  START HERE FOR NEW MAP TRIAL
  # Get the USA map
  usa <- map_data("usa")
  
  #dim(usa)
  
  data(state.map)
  #p <- ggplot(state.map, aes(long, lat, group=group)) + geom_polygon()
  
  labs <- data.frame(
    long = df_subset$LONGITUD,
    lat = df_subset$LATITUDE,
    avgDR = df_subset$CohortDefaultRate,
    stringsAsFactors = FALSE)  
  
  labs <- subset(labs, long < -70)
  labs <- subset(labs, long > -125)
  labs <- subset(labs, lat < 50)
  labs <- subset(labs, lat > 25)
  
  
  gg1 <- ggplot(labs, aes(long, lat)) + geom_polygon(data = usa, aes(x=long, y = lat, group = group), 
                                                     fill = "white", color = "blue") + coord_fixed(1.3)
  gg1 <- gg1 + geom_point(aes(colour = avgDR)) + coord_fixed(1.3)
  
  gg1 <- gg1 + scale_color_gradient(name = "Default Rate", low = "dark blue", high = "red")
  
  gg1 <- gg1 + labs(x=NULL, y=NULL)
  gg1 <- gg1 + theme(panel.border = element_blank())
  gg1 <- gg1 + theme(panel.background = element_blank())
  gg1 <- gg1 + theme(axis.ticks = element_blank())
  gg1 <- gg1 + theme(axis.text = element_blank())
  gg1 
  
}


