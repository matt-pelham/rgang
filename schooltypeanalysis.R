library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)

generate.summary.tables <- function(df){
  levels(df$School.Type)[levels(df$School.Type) == "1"] <- "Public"
  levels(df$School.Type)[levels(df$School.Type) == "2"] <- "Private"
  levels(df$School.Type)[levels(df$School.Type) == "3"] <- "Proprietary"
  
  #With just three levels that remained after we sifted for the complete cases, I decided just #rename the levels...
  
  df <-group_by(df, School.Type, Year)
  df_s <- summarize(df, avg_default = mean(CohortDefaultRate))
  df_st <-dcast(df_s, School.Type ~ Year, value.var = "avg_default")
  assign("summary.defaultrates.by.schooltype",df_st,envir = .GlobalEnv)
  df <- ungroup(df)
  # Public Colleges and Universities had the highest average default rates for years
  # 2012 and 2013. Private-Non Profit Colleges and Universities had the lowest deafult rates-
  # almost half that of it Public and Proprietary counterparts. This summary table alone
  # shows that private institutions outpace their counterparts as it relates to default rate.
  
  df1 <-subset(df, School.Type == "Public")
  df1 <-group_by(df1, School.Type, Year)
  df_s1 <- summarize(df1, perc_default = sum(CohortDefaultRate <= 15)/n())
  df_st1 <- dcast(df_s1, School.Type ~ Year, value.var = "perc_default")
  assign("summary.public.percent.default15",df_st1,envir = .GlobalEnv)
  
  df2 <-subset(df, School.Type == "Private")
  df2 <-group_by(df2, School.Type, Year)
  df_s2 <- summarize(df2, perc_default = sum(CohortDefaultRate <= 15)/n())
  df_st2 <- dcast(df_s2, School.Type ~ Year, value.var = "perc_default")
  assign("summary.private.percent.default15",df_st2,envir = .GlobalEnv)
  
  df3 <-subset(df, School.Type == "Proprietary")
  df3 <-group_by(df3, School.Type, Year)
  df_s3 <- summarize(df3, perc_default = sum(CohortDefaultRate <= 15)/n())
  df_st3 <- dcast(df_s3, School.Type ~ Year, value.var = "perc_default")
  assign("summary.proprietary.percent.default15",df_st3,envir = .GlobalEnv)
  
  #Based on the Summary Tables above, we can see that Private-Non Profit Colleges
  #and Universities are most likely to rid themselves of restrictions as it relates to 
  #dispersing financial aid in the form of loans. If an institution maintains
  # a cohort default rate below 15% for at least three consecutive years, they acquire the
  #autonomy to disperse loan monies. If an institution does not achieve this benchmark
  # they must wait to 30 days to disburse loan monies to first year or first-time borrowers.
  # Within this metric, Public Schools, improved from year to year, showing an increae in the #percentage of 
  # schools with default rates <=15%. Private Schools faltered slightly. Proprietary schools #remained the same. 
  
  
  
  df4 <-subset(df, School.Type == "Public")
  df4 <-group_by(df4, School.Type, Year)
  df_s4 <- summarize(df4, perc_default = sum(CohortDefaultRate >=30)/n())
  df_st4 <- dcast(df_s4, School.Type ~ Year, value.var = "perc_default")
  
  assign("summary.public.percent.default30",df_st4,envir = .GlobalEnv)
  
  df5 <-subset(df, School.Type == "Private")
  df5 <-group_by(df5, School.Type, Year)
  df_s5 <- summarize(df5, perc_default = sum(CohortDefaultRate >=30)/n())
  df_st5 <- dcast(df_s5, School.Type ~ Year, value.var = "perc_default")
  
  assign("summary.private.percent.default30",df_st5,envir = .GlobalEnv)
  
  df6 <-subset(df, School.Type == "Proprietary")
  df6 <-group_by(df6, School.Type, Year)
  df_s6 <- summarize(df6, perc_default = sum(CohortDefaultRate >=30)/n())
  df_st6 <- dcast(df_s6, School.Type ~ Year, value.var = "perc_default")
  assign("summary.proprietary.percent.default30",df_st6,envir = .GlobalEnv)
  
  # Based on the Summary Tables above, we can see that in 2012, Public Colleges and most 
  # Universities were most likely to lose eligibility to participate in federal student 
  # loan and/or Federal Pell Grant programs.In 2013, Proprietary Colleges and Universities
  # were most likely to lose eligibilty to participate in the same programs. Schools are at risk of #suffering this sanction 
  # if their default rate is at or above 30%. Year to year, PUblic instutions improved on this metric. 
  # Private instutions faltered slightly, as did Proprietary Schools. 
}


plot.default.over.tuition <- function(df)
{
  p <- qplot(Tuition, CohortDefaultRate, data = df, geom = "point", color = School.Type, alpha = I(.50)) + labs(color = "School Type") 
  p <- p + xlab("Cost of Tuition (in U.S. Dollars)")
  p <- p + ylab("Default Rate (Percentage)")
  p <- p + scale_x_continuous(labels = dollar)
  p <- p + ggtitle("Student Loan Default Rate vs. Tuition (2012 - 2013)")
  p <- p + theme(plot.title = element_text(size = 12, face = "bold"))
  p <- p + theme(axis.title = element_text(size = 12, face = "bold"))
  p
}




# This plot looks at 3 variables-default rate, tuition and school type. I thought
# it was a good visual representation of the questions both Cody and I were assigned to analyze
plot.default.over.tuition.typefacet <- function(df){
  bp <- qplot(School.Type, CohortDefaultRate, data = df, geom = "boxplot") + geom_jitter(alpha = I(.10))
  bp <- bp + xlab("School Type")
  bp <- bp + ylab("Default Rate (Percentage)")
  bp <- bp + ggtitle("Boxplot Distribution: Student Loan Default Rate vs. School Type (2012 -2013)")
  bp <- bp + theme(plot.title = element_text(size = 12, face = "bold"))
  bp <- bp + theme(axis.title = element_text(size = 12, face = "bold"))
  bp 
 
  # This plot shows a little more about what's going on with the school type distribution. 
  
  # With just three levels in my school.type column, I don't know that my analysis is all that #interesting, 
  # which is why I tried to contextualize as much as I could with the benchmark rates (<15% and #>30%). 
}



 