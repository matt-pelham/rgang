# This program will create 2 plots showing the level of secondary
# loan default rates based on location.  The first plot is average 
# default rates averaged for 2012 and 2013 data across the US.
# The second plot is narrowing the scope of our data down to just 
# the State of New York.
# Code created by:  Kris Arens
# Last update:  10/26/17

#Added the command to refresh the environment
rm(list = ls())

#Source the file containing the functions that load and parse the data
source("dataload.R")

full_df <- load.data()

unflattened_df <- unflatten.data(full_df)

# Libraries for plotting cloropleth graphs

# Load packages
suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

# Reduce the number of columns to just a subset to allow for faster
# processing of data
rlist <- c("Unit.ID", "City", "State", "CohortDefaultRate2012","CohortDefaultRate2013", 
           "LONGITUD", "LATITUDE")

# Create the new dataframe
df_subset <- full_df[,rlist]

# Create the average/mean default rate
df_subset$avgDefaultRate <- (df_subset$CohortDefaultRate2012 + df_subset$CohortDefaultRate2013)/2

# Select the map outline to be used.  This uses the US map with an outline
# of all 48 contiguous states
usa <- map_data("state")

# To better guide the color scale, adjusted the midpoint of the color 
# pallette to the median of the defaul rates.  This allows for a better 
# overall color spread.  USavgdef will be used as midpoint in the gradient 
# scale below

USavgdef <- median(df_subset$avgDefaultRate)

# Reduce the dataset to only those states in the contiguous 48.  This means
# we remove Hawaii and Alaska.  
df_subset <- subset(df_subset, State != "HI")
df_subset <- subset(df_subset, State != "AK")                    
      
#  Also added this code to remove those US Territory's included 
#  such as Guam, US Virgin Islands and Puerto Rico
df_subset <- subset(df_subset, LONGITUD < -70)
df_subset <- subset(df_subset, LONGITUD > -125)
df_subset <- subset(df_subset, LATITUDE < 50)
df_subset <- subset(df_subset, LATITUDE > 25)


# Create the intial plot using the state map and overlaying it with 
# plot points that show the range of average default rates by location.
# The plot is then adjusted to remove the grids, labels and add a title
gg1 <- ggplot(df_subset, aes(LONGITUD, LATITUDE)) + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), 
               fill = "white", color = "blue") + coord_fixed() +
  geom_point(aes(color = avgDefaultRate), size = 1.3) + 
  scale_color_gradient2(name = "Avg Default Rate", low = "orange", 
                              mid = "light blue", high = "black", midpoint = USavgdef)
  
gg1 <- gg1 + ggtitle("Average Loan Default Rates Across the US") +
             theme(plot.title = element_text(size = 16))
gg1 <- gg1 + theme(legend.text = element_text(size = 8))
gg1 <- gg1 + theme(legend.key.size = unit(0.12, "in"))
gg1 <- gg1 + theme(panel.border = element_blank())
gg1 <- gg1 + theme(panel.background = element_blank())
gg1 <- gg1 + theme(axis.ticks = element_blank())
gg1 <- gg1 + theme(axis.text = element_blank())
gg1 <- gg1 + labs(x=NULL, y=NULL)
gg1


# Now let's limit our information to just New York and do the same
# process for creating a plot

dfs_NY <- subset(df_subset, State == "NY")
stateMAP <- map_data("state", region = "new york")
gny <- ggplot(dfs_NY, aes(LONGITUD, LATITUDE)) + 
  geom_polygon(data = stateMAP, aes(x=long, y = lat, group = group), 
               fill = "white", color = "blue") + coord_fixed() +
  geom_point(aes(color = avgDefaultRate), size = 2) + 
  scale_color_gradient2(name = "Avg Default Rate", low = "orange", 
                        mid = "light blue", high = "black", midpoint = USavgdef)

gny<- gny + ggtitle("Average Loan Default Rates Across New York") +
  theme(plot.title = element_text(size = 16))
gny <- gny + theme(legend.text = element_text(size = 8))
gny <- gny + theme(legend.key.size = unit(0.12, "in"))
gny <- gny + theme(panel.border = element_blank())
gny <- gny + theme(panel.background = element_blank())
gny <- gny + theme(axis.ticks = element_blank())
gny <- gny + theme(axis.text = element_blank())
gny <- gny + labs(x=NULL, y=NULL)
gny
