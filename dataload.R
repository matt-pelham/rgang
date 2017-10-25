suppressMessages(library(dplyr))
suppressWarnings(library(openxlsx))

load.data <- function(){
  
  rm(list = ls())
  
  codes <- read.xlsx("IPEDS.xlsx", sheet=2)
  
  codes$OPE.ID <- as.character(codes$OPE.ID)
  
  peps300 <- read.xlsx("peps300.xlsx", sheet = 1)
  peps300$OPEID <- as.character(peps300$OPEID)
  
  #To get the OPE IDs to match between the files, it looks like we just need to remove leading 0's from the
  #OPE ID in the peps300 file, then pad the number with two trailing 0's.  Use the paste0 function to do the padding
  peps300$OPEID <- as.integer(peps300$OPEID)#Cast OPEID as an integer, which will remove leading 0's; we will cast it right back to a string though
  peps300$OPEID <- as.character(peps300$OPEID)
  peps300$OPEID <- paste0(peps300$OPEID,"00") #Paste function can be used to concatenate strings
  
  #Join the codes with the peps file, which will allow us to join this all together with the additional data using UNIT ID
  codes_peps300 <- merge(codes,peps300,by.x = "OPE.ID", by.y = "OPEID")
  
  #The file with OPEID to UnitID mappings has duplicate OPEID values in it, most likely because of multiple sites per institution.
  #Sometimes an OPEID has multiple UNITIDs associated with it, but sometimes the OPEID is repeated with the same UNIT ID multiple times.
  #Remove the records that have duplicate combinations of OPEID and UNITID, as they will only represent duplicate cases in our analysis. 
  #OPEID 1054600 is an example of an institution in the OPEID.csv file that is listed multiple times with the same UNITID.
  #OPEID 109000 is an example of an institution in the OPEID.csv file that is listed multiple times with different UNITIDs.
  codes_peps300 <- codes_peps300[!duplicated(codes_peps300[1:2]),]
  
  latlong <- read.xlsx("IPEDS full dataset.xlsx", sheet = 4)
  
  sfa2012 <- read.xlsx("IPEDS full dataset.xlsx", sheet = 1, na.strings=c("", "."))
  
  sfa2013 <- read.xlsx("IPEDS full dataset.xlsx", sheet = 2, na.strings=c("", "."))
  
  full_df <- merge(codes_peps300, latlong, by.x = "Unit.ID",by.y="UNITID")
  
  full_df <- merge(full_df, sfa2012, by.x = "Unit.ID", by.y = "UNITID")
  
  full_df <- merge(full_df, sfa2013, by.x = "Unit.ID", by.y = "UNITID")
  
  effy2012 <- read.xlsx("IPEDS full dataset.xlsx", sheet = 5, na.strings=c("", "."))
  
  effy2013 <- read.xlsx("IPEDS full dataset.xlsx", sheet = 6, na.strings=c("", "."))
  
  full_df <- merge(full_df, effy2012, by.x = "Unit.ID", by.y = "UNITID")
  
  full_df <- merge(full_df, effy2013, by.x = "Unit.ID", by.y = "UNITID")
  
  ic2012 <- read.xlsx("IPEDS full dataset.xlsx", sheet = 8, na.strings=c("", "."))
  
  ic2013 <- read.xlsx("IPEDS full dataset.xlsx", sheet = 9, na.strings=c("", "."))
  
  full_df <- merge(full_df, ic2012, by.x = "Unit.ID", by.y = "UNITID")
  
  full_df <- merge(full_df, ic2013, by.x = "Unit.ID", by.y = "UNITID")
  
  #The names of the columns imported from the IPEDS site used codes. The
  #column names are changed to more understandable titles.
  
  names(full_df)[names(full_df)== "CHG2AY3.2012"]  <- "Tuition2012"
  names(full_df)[names(full_df)== "PRate.3"]       <- "Ratetype2012"
  names(full_df)[names(full_df)== "UAGRNTA.2012"]  <- "GrantDollars2012"
  names(full_df)[names(full_df)== "UFLOANP.2012"]  <- "LoanPercent2012"
  names(full_df)[names(full_df)== "UFLOANA.2012"]  <- "LoanDollars2012"
  names(full_df)[names(full_df)== "EFYTOTLT.2012"] <- "EnrollTotal2012"
  names(full_df)[names(full_df)== "Dual.Denom.3"]       <- "NumInRepay2012"
  names(full_df)[names(full_df)== "Dual.Num.3"]         <- "NumInDefault2012"
  names(full_df)[names(full_df)== "DRate.3"]       <- "CohortDefaultRate2012"
  
  names(full_df)[names(full_df)== "CHG2AY3.2013"]  <- "Tuition2013"
  names(full_df)[names(full_df)== "PRate.2"]       <- "Ratetype2013"
  names(full_df)[names(full_df)== "UAGRNTA.2013"]  <- "GrantDollars2013"
  names(full_df)[names(full_df)== "UFLOANP.2013"]  <- "LoanPercent2013"
  names(full_df)[names(full_df)== "UFLOANA.2013"]  <- "LoanDollars2013"
  names(full_df)[names(full_df)== "EFYTOTLT.2013"] <- "EnrollTotal2013"
  names(full_df)[names(full_df)== "Dual.Denom.2"]       <- "NumInRepay2013"
  names(full_df)[names(full_df)== "Dual.Num.2"]         <- "NumInDefault2013"
  names(full_df)[names(full_df)== "DRate.2"]       <- "CohortDefaultRate2013"
  
  #We joined two data sets - one with data from 2011-2013, and a second that had data from 2012-2014.  To be consistent, we looked at 
  #only data from the intersection of the two data sets, that is, data from 2012-2013.  We excluded the 2014 data by simply not loading
  #the Excel sheet with these values.  But to remove the 2011 data, we need to remove the columns, as they were all loaded from 1 sheet.
  
  #Remove the columns with 2014 data
  full_df$Year.1 <- NULL
  full_df$Dual.Num.1 <- NULL
  full_df$Dual.Denom.1 <- NULL
  full_df$DRate.1 <- NULL
  full_df$PRate.1 <- NULL
  #Also clean up these 2 columns, as they don't add anything to the data set - they are just static year values that won't be used.
  full_df$Year.2 <- NULL
  full_df$Year.3 <- NULL
  
  full_df <- full_df[complete.cases(full_df), ]  
  
  full_df <- subset(full_df, Tuition2012 > 0 & Tuition2013 > 0) #there is a record that has a 0 value for tuition.  Remove it, since it is likely a mistake.
  
  full_df$School.Type<- factor(full_df$School.Type) 
  
  full_df
  
}

unflatten.data <- function(df){
  #Get the column names that don't have a year in them - those are the "core" columns
  columns.core<- names(df)[which(!grepl("201",names(df),fixed=TRUE))] 
  #Get the column names that have 2012 in them - those are 2012 data columns
  columns.2012 <- names(df)[which(grepl("2012",names(df),fixed=TRUE))] 
  #Get the column names that have 2013 in them - those are 2013 data columns
  columns.2013 <- names(df)[which(grepl("2013",names(df),fixed=TRUE))]
  
  #Select the core columns and the 2012 columns and put them in a new dataset for 2012 data
  df2012 <- df[,c(columns.core, columns.2012)]
  #Remove the 2012 string from the colum names where it exists
  names(df2012) <- gsub("2012","",names(df2012), fixed=TRUE)
  
  #Select the core columns and the 2013 columns and put them in a new dataset for 2013 data
  df2013 <- df[,c(columns.core, columns.2013)]
  #Remove the 2013 string from the colum names where it exists
  names(df2013) <- gsub("2013","",names(df2013), fixed=TRUE)
  
  #Add columns to the yearly data sets to indicate the year - this will distinguish between the data sets when
  #we combine them
  df2012$Year <- 2012
  df2013$Year <- 2013
  #Use rbind to combine the 2012 and 2013 data into one data frame with consistent column names
  #and the Year column that will distinguish between 2012 and 2013 records
  unflattened_df <- rbind(df2012,df2013)
  unflattened_df <- subset(unflattened_df, Tuition <= 50000 & CohortDefaultRate <= 40)
  unflattened_df
}