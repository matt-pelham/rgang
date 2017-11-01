
# Load packages
suppressPackageStartupMessages(library(choroplethr))
library(choroplethrMaps)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

# Reduce the number of columns 
generate.choropleth.maps <- function(df, state=NA, mapyear = 2013)
  {
  
    # Reduce the number of columns to just a subset to allow for faster
    # processing of data
    rlist <- c("Unit.ID", "City", "State", "CohortDefaultRate", 
               "LONGITUD", "LATITUDE")
    
    # Create the new dataframe
    df_subset <- df[,rlist]
    
    # Reduce the values graphed by year.  If no year is specified, 
    # default to 2013 (latest year of data available)
    if(mapyear != 2012 && mapyear != 2013){
      msg <- sprintf("The mapyear parameter should be 2012 or 2013 only.  Defaulting to 2013. ")
      message(msg)
      mapyear <- 2013
    }
    df_subset <- subset(df_subset, Year = mapyear)
    
    # To better guide the color scale, adjusted the midpoint of the color 
    # pallette to the mean of the defaul rates.  This allows for a better 
    # overall color spread.  USavgdef will be used as midpoint in the gradient 
    # scale below
    
    USavgdef <- mean(df_subset$CohortDefaultRate)
    
    
    if(is.na(state)){
        # Select the map outline to be used.  This uses the US map with an outline
        # of all 48 contiguous states
        usa <- map_data("state")
        
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
            geom_point(aes(color = CohortDefaultRate), size = 1.3) + 
            scale_color_gradient2(name = "Default Rate %", low = "orange", 
                                  mid = "light blue", high = "black", midpoint = USavgdef)
        
        gg1 <- gg1 + ggtitle(paste("Loan Default Rates Across the US in ", mapyear, sep = "" )) +
            theme(plot.title = element_text(size = 12, face = "bold"))
        gg1 <- gg1 + theme(legend.text = element_text(size = 8))
        gg1 <- gg1 + theme(legend.key.size = unit(0.12, "in"))
        gg1 <- gg1 + theme(panel.border = element_blank())
        gg1 <- gg1 + theme(panel.background = element_blank())
        gg1 <- gg1 + theme(axis.ticks = element_blank())
        gg1 <- gg1 + theme(axis.text = element_blank())
        gg1 <- gg1 + labs(x=NULL, y=NULL)
        gg1 
        
    }else{
        state <- toupper(state)
        # There was a state provided, so let's limit our information to just the state and do the same
        # process for creating a plot
        #TODO: Handle an error if the state code provided is not in the data set, or is AK or HI
        data("state.regions")#Load the state data set to get the region name
        if(state == "AK" || state == "HI" || nrow(subset(state.regions,abb == state)) == 0){
          err <- "Invalid state provided.  Please provide a valid 2 character state abbreviation for one of the contiguous 48 states."
          stop(err)
        }
        region <- subset(state.regions,abb == state)[1,c("region")]
        dfs_state <- subset(df_subset, State == state)
        stateMAP <- map_data("state", region = region)
        gstate <- ggplot(dfs_state, aes(LONGITUD, LATITUDE)) + 
            geom_polygon(data = stateMAP, aes(x=long, y = lat, group = group), 
                         fill = "white", color = "blue") + coord_fixed() +
            geom_point(aes(color = CohortDefaultRate), size = 2) + 
            scale_color_gradient2(name = "Default Rate %", low = "orange", 
                                  mid = "light blue", high = "black", midpoint = USavgdef)
        #Capitalize the region name appropriately and write it in the title
        title <- paste0("Loan Default Rates Across ",capitalState(region), " in ", mapyear)
        gstate<- gstate + ggtitle(title) +
          theme(plot.title = element_text(size = 12, face = "bold"))
        gstate <- gstate + theme(legend.text = element_text(size = 8))
        gstate <- gstate + theme(legend.key.size = unit(0.12, "in"))
        gstate <- gstate + theme(panel.border = element_blank())
        gstate <- gstate + theme(panel.background = element_blank())
        gstate <- gstate + theme(axis.ticks = element_blank())
        gstate <- gstate + theme(axis.text = element_blank())
        gstate <- gstate + labs(x=NULL, y=NULL)
        gstate
    }

  
}

#This came from the help file on toupper
capitalState <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}
