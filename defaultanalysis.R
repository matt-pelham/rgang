library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)



densityplot.default.over.tuition <- function(df){
  
  
  #P1   Density plot to show that lower tuition rates have a higher default rate
  p <- qplot(Tuition,NumInDefault , data = df, geom = "bin2d")
  p <- p + scale_fill_gradient(name = "Frequency", low = "blue", high = "red")
  p <- p + ggtitle("Density Map Measuring Default Rates Against Cost of Tuition ")
  p <- p + xlab("Cost of Tuition ")
  p<- p + ylab("Number in Default")
  p
  
}


densityplot.defaultrate.over.tuition <- function(df){
  #Produces a summary table for default rates, based on  program length and cost of tuition. 
  
  
  df$Tuition_binned <- cut(df$Tuition, 10000*(0:5), labels = c("$1-$10,000","$10,001-$20,000", "$20,001-$30,000", "$30,001-$40,000", "$40,001-$50,000"))
  df <- group_by(df, Tuition_binned, Prog.Meaning)
  sum_12_13 <- summarize(df, MeanDefault_12and13 = mean(CohortDefaultRate))
  Mean_dflt_12_13 <- dcast(sum_12_13, Prog.Meaning ~ Tuition_binned, value.var = "MeanDefault_12and13")
  assign("mean.default.12and13",Mean_dflt_12_13,envir = .GlobalEnv)
  
  
  #create a table that shows the rankings of the default rates from the mean default rates
  rank_dflt_12_13 <- mean.default.12and13 #Mean_dflt_12_13
  rank_dflt_12_13$`$1-$10,000` <- rank(rank_dflt_12_13$`$1-$10,000`)
  rank_dflt_12_13$`$10,001-$20,000` <- rank(rank_dflt_12_13$`$10,001-$20,000`, na.last = "keep")
  rank_dflt_12_13$`$20,001-$30,000` <- rank(rank_dflt_12_13$`$20,001-$30,000`,na.last = "keep")
  rank_dflt_12_13$`$30,001-$40,000` <- rank(rank_dflt_12_13$`$30,001-$40,000`,na.last = "keep")
  rank_dflt_12_13$`$40,001-$50,000` <- rank(rank_dflt_12_13$`$40,001-$50,000`,na.last = "keep")
  assign("rank.default.12and13",rank_dflt_12_13 ,envir = .GlobalEnv)
  
  df <- ungroup(df)
  
  #P2   Produces a log scale which shows the default percentage compared against tuition binned.   Shows that as the tuiion goes up,
  # the defualt rates are less dense at the higher percentages.
  p <- qplot(CohortDefaultRate,  data = df, geom = "density", facets = . ~ Tuition_binned )
  p <- p + ggtitle("Density Plot of Default Rates for Binned Tuition Prices")
  p <- p + xlab("Default Rates")
  p <- p + theme(plot.title = element_text(face = "bold"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + theme(axis.title.y = element_blank())#Remove the Y axis title, don't need for a density plot
  
  # Remove numbers on y axis
  p <- p + theme(axis.text.y = element_blank())
  
  # Remove tick marks on y axis
  p <- p + theme(axis.ticks.y = element_blank())
  p    
  
}


#p4
plot.default.rate.over.degree.and.tuition <- function(df){
  
  #Shows that associates degrees have a higher default rate despite their lower tuition.   In conclusion
  #lower default rates are associated with higher tuition rates along with Bachelor degrees and Master's Degrees.
  #Higher default rates are associated with lower tuition and associates degrees.

  p <- qplot(Tuition, CohortDefaultRate, data = unflattened_df, geom = "point", color= Prog.Meaning,
             alpha= I(.50))
  p <- p + ggtitle("Default Rate over Cost of Tuition")
  p <- p + scale_x_continuous(name = "Cost of Tuition",
                              breaks= 5000*(0:9), labels = dollar)
  p <- p + scale_y_continuous(name = "Default Rate (%)")
  p <- p + theme(panel.border = element_rect(linetype = "solid",
                                             color = "black", fill = NA))
  p <- p + theme(legend.text = element_text(size = 8))
  p <- p + theme(legend.key.size = unit(0.12, "in"))
  p <- p + theme(legend.spacing = unit(0, "in"))
  p <- p + theme(title = element_text(size = 12))
  p <- p + theme(legend.key = element_rect(fill = "white"))
  p <- p + theme(plot.title = element_text(face = "bold"))
  p <- p + theme(panel.grid = element_blank())
  p <- p + scale_color_hue (name = "Program Length")
  p <- p + theme(axis.text.x = element_text(angle = -90))
  p
}  

