library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)

generate.summary.tables <- function(df){
  levels(df$School.Type)[levels(df$School.Type) == "1"] <- "Public"
  levels(df$School.Type)[levels(df$School.Type) == "2"] <- "Private"
  levels(df$School.Type)[levels(df$School.Type) == "3"] <- "Proprietary"
  
  # After joining together our datasets and sifting through out incomplete obervations, we were left with just three levels of our
  # School.Type variable among our complete cases. To produce analysis that was easier to comprehend, I decided to just rename levels
  # to their categorical values contained in our datakey. The nubering system used by the department of education seemed to be of little
  # value for our purposes. 
  
  
  df <-group_by(df, School.Type, Year)
  df_s <- summarize(df, avg_default = mean(CohortDefaultRate))
  df_st <-dcast(df_s, School.Type ~ Year, value.var = "avg_default")
  assign("summary.defaultrates.by.schooltype",df_st,envir = .GlobalEnv)
  df <- ungroup(df)
  
  # Public Colleges and Universities had the highest average default rates for years
  # 2012 and 2013. Private-Non Profit Colleges and Universities had the lowest deafult rates-
  # almost half that of it Public and Proprietary counterparts. This summary table alone
  # shows that private institutions outpace their counterparts as it relates to default rate.
  
  
  df1 <-group_by(df, School.Type, Year)
  df_s1 <- summarize(df1, perc_default = sum(CohortDefaultRate <= 15)/n())
  df_st1 <- dcast(df_s1, School.Type ~ Year, value.var = "perc_default")
  
  assign("summary.percent.default15",df_st1,envir = .GlobalEnv)
  
  
  # The summary table above captured how each school type is meeting the Department of Education
  # benchmark of a sub-15% default rate.Schools who are meeting that benchmark are able to ride themselves
  # of the restrictions surrounding the dispersment of student loans. These tables capture the number of 
  # institutions (within each school type) that carry sub-15% default rates over the total number of insitutions
  # in that same school type (public, private, proprietary). Private schools appear most likely to achieve this benchmark. 
  # Public schools rates for this metric decreased from 2012 to 2013. The percentage of Proprietary schools meeting this benchmark 
  # increased from year 2012 to 2013. 
  
  
  df2 <-group_by(df, School.Type, Year)
  df_s2 <- summarize(df2, perc_default = sum(CohortDefaultRate >=30)/n())
  df_st2 <- dcast(df_s2, School.Type ~ Year, value.var = "perc_default")
  
  assign("summary.percent.default30",df_st2,envir = .GlobalEnv)
  
  
  
  # The summary table above captures the percentage of each school type that is 'at-risk' of losing eligibility 
  # to participate in federal student loan and/or Federal Pell Grant programs. Schools that have three consecutive years
  # of cohort loan default rates above 30% could lose the ability to particiapte in these programs. All groups appeared to perform well
  # in this metric as very small percentage of these instituions have default rates above 30%.Within our dataset, Public colleges were
  # most likely to lose eligbility in the programs, though that group improved from 2012 to 2013. Private colleges also improved from 
  # year to year. Proprietary collegs performed best in this metric, but there were fewer observaions among this level. 
}


plot.default.over.tuition <- function(df)
{
  p <- qplot(Tuition, CohortDefaultRate, data = df, geom = "point", color = School.Meaning, alpha = I(.50)) + labs(color = "School Type") 
  p <- p + xlab("Cost of Tuition (in U.S. Dollars)")
  p <- p + ylab("Default Rate (Percentage)")
  p <- p + scale_x_continuous(labels = dollar)
  p <- p + ggtitle("Student Loan Default Rate vs. Tuition (2012 - 2013)")
  p <- p + theme(plot.title = element_text(size = 12, face = "bold"))
  p <- p + theme(axis.title = element_text(size = 12, face = "bold"))
  p <- p + scale_color_manual(values=c("light blue", "orange", "black"))
}

# This scatter plot looks at 3 variables-default rate, tuition and school type. Here, we can see that Public institutions, with a lower
# cost of attendance show higher default rates. 

plot.default.over.tuition.typefacet <- function(df){
  bp <- qplot(School.Meaning, CohortDefaultRate, data = df, geom = "boxplot") + geom_jitter(alpha = I(.10))
  bp <- bp + xlab("School Type")
  bp <- bp + ylab("Default Rate (Percentage)")
  bp <- bp + ggtitle("Boxplot Distribution: Student Loan Default Rate vs. School Type (2012 -2013)")
  bp <- bp + theme(plot.title = element_text(size = 12, face = "bold"))
  bp <- bp + theme(axis.title = element_text(size = 12, face = "bold"))
  bp 
  
  # This box plot shows a little more about what's going with default rates with each individual school type. Here, we have a good
  # picture regarding mean default rates for each school type, the density of each quartile and also outlier values for each school type.
}

