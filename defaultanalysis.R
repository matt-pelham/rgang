library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)



densityplot.default.over.tuition <- function(df){
  
  
  #Density plot to show that lower tuition rates have a higher default rate
  p <- qplot(Tuition,NumInDefault , data = df, geom = "bin2d")
  p <- p + scale_fill_gradient(name = "Frequency", low = "blue", high = "red")
  p <- p + xlab("Cost of Tuition ")
  p<- p + ylab("Number in Default")
  p
  
}


densityplot.defaultrate.over.tuition <- function(df){
  #Produces a summary table for default rates, based on  program length and cost of tuition. 
  #Shows that:
  # Prog length
  # 3-Non-Degree 1 Year(900-1799 hours)  --> near the top of default list for all applicable bins
  # 4-Non-Degree 2 Year(1800-2699 hours) --> appears at bottom and top of list (mixed)
  # 5-Associates degree  --> highest default rate in 3 out of 5 tuition bins (close 2nd in two bins that it didn't lead)
  # 6-Bachelor's degree  --> 3rd highest out of the 8 ffor default in 4 out of 5 bins
  # 7-1st Profes Degree  --> lowest defualt rates among all tuition bins it appeared in
  # 8-Master's degree  -->  near 2nd to lowest default rates across all bins
  # 11-Non degree (3 years plus)  no clear pattern
  # 12-two year transfer  --> minimal data, but on bottom half of bins where did occur
  df$Prog.Length<- as.factor(df$Prog.Length)
  df$Tuition_binned <- cut(df$Tuition, 10000*(0:5), labels = c("1-10000","10001-20000", "20001-30000", "30001-40000", "40001-50000"))
  df <- group_by(df, Tuition_binned, Prog.Length)
  sum_12_13 <- summarize(df, MeanDefault_12and13 = mean(CohortDefaultRate))
  Mean_dflt_12_13 <- dcast(sum_12_13, Prog.Length ~ Tuition_binned, value.var = "MeanDefault_12and13")
  assign("mean.default.12and13",Mean_dflt_12_13,envir = .GlobalEnv)
  
  #create a table that shows the rankings of the default rates from the mean default rates
  rank_dflt_12_13 <- mean.default.12and13 #Mean_dflt_12_13
  rank_dflt_12_13$`1-10000` <- rank(rank_dflt_12_13$`1-10000`)
  rank_dflt_12_13$`10001-20000` <- rank(rank_dflt_12_13$`10001-20000`, na.last = "keep")
  rank_dflt_12_13$`20001-30000` <- rank(rank_dflt_12_13$`20001-30000`,na.last = "keep")
  rank_dflt_12_13$`30001-40000` <- rank(rank_dflt_12_13$`30001-40000`,na.last = "keep")
  rank_dflt_12_13$`40001-50000` <- rank(rank_dflt_12_13$`40001-50000`,na.last = "keep")
  #rank_dflt_12_13$`50001-60000` <- rank(rank_dflt_12_13$`50001-60000`)
  #rank_dflt_12_13$`60001-70000` <- rank(rank_dflt_12_13$`60001-70000`,na.last = "keep")
  assign("rank.default.12and13",rank_dflt_12_13 ,envir = .GlobalEnv)
  
  df <- ungroup(df)

  #Produces a log scale which shows the default percentage compared against tuition binned.   Shows that as the tuiion goes up,
  # the defualt rates are less dense at the higher percentages.
  p<- qplot(CohortDefaultRate,  data = df, geom = "density", facets = . ~ Tuition_binned )
  p <- p + ggtitle("Density of Default Rates for Binned Tuition Prices")
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

plot.combined.default.over.tuition <- function(df){
  
  #I'm not sure we need this.- Number of combined default in related to total tuition. Shows that
  #the total number of defaulters is highest among bachelor's degrees
  df$Prog.Length<- as.factor(df$Prog.Length)
  p<- qplot(Tuition, NumInDefault, data = df, geom = "point",
            color = Prog.Length, alpha= I(.80))
  p <- p + ggtitle("Number of Students in Default \nMeasured Against Cost of Tuition\n")
  p <- p + scale_x_continuous(name = "Cost of Tuition",
                              breaks= 5000*(0:9), labels = dollar)
  p <- p + scale_y_continuous(name = "Number of Students in Default")
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


plot.default.rate.over.degree.and.tuition <- function(df){
  
  #Shows that associates degrees have a higher default rate despite their lower tuition.   In conclusion
  #lower default rates are associated with higher tuition rates along with Bachelor degrees and Master's Degrees.
  #Higher default rates are associated with lower tuition and associates degrees.
  df$Prog.Length<- as.factor(df$Prog.Length)
  p<- qplot(Tuition, CohortDefaultRate, data = df, geom = "point", color= Prog.Length,
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


#I looked at Ben's code and thought that the Boxplot idea was great.  I have tried to apply that 
# here and think it works well, but needs some tweaking. 
boxplot.default.rates.v.program.length <- function(df){
  
  pl <- qplot(Prog.Length, CohortDefaultRate, data = unflattened_df, geom = "boxplot") + geom_jitter(alpha = I(.10))
  pl <- pl + scale_x_continuous(name = "Prog Length", breaks = c(3,4,5,6,7,8,11,12))  
  pl <- pl + scale_y_continuous(name = "Default Rate (%)", breaks = c(10,20,30,40))
  pl <- pl + ggtitle("Boxplot Distribution: Student Loan Default Rate vs. Program Length(2012 -2013)")
  pl <- pl + theme(plot.title = element_text(size = 12, face = "bold"))
  pl <- pl + theme(axis.title = element_text(size = 12, face = "bold"))
  pl <- pl + theme(panel.grid = element_blank())
  pl
  
}