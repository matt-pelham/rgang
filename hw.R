# Homework 7                                              Data Programming in R
# Due by 6:00 PM on Tue Oct 17              Business Analytics Graduate Program
# via GitHub                                  MSCI:6060 Fall 2017 (Quad Cities)

###############################################################################
#                                                                             #
#                               INSTRUCTIONS                                  #
#                                                                             #
###############################################################################

# This homework corresponds to the Oct 10 class; please refer to
# the corresponding course materials. Please also follow all of the
# guidelines given on prior homeworks, referring to them if necessary.

###############################################################################
#                                                                             #
#                                EXERCISES                                    #
#                                                                             #
###############################################################################

# Clear workspace

rm(list = ls())

###############################################################################

# 1. Read famous_quotes.csv with "stringsAsFactors = FALSE" into a data
# frame called df_quotes. Then complete the following:

df_quotes <- read.csv("famous_quotes.csv", stringsAsFactors = FALSE)


# 1.(a) Save the row indices (or row positions) of the quotes containing
# the substring "fear" (case insensitive) in a vector called row_nums.

#Do a tolower on df_quotes$quote to make the search case insensitive
row_nums <- grep("fear",tolower(df_quotes$quote),fixed = TRUE)

# 1.(b) Calculate the mean number of characters in the quotes in the
# character vector df_quotes$quote. Save the result in mean_chars.
# [Hint: Base your calculation on *all* characters, including spaces
# and punctuation. Do not round the mean, and do not use a for loop to
# complete this task.]

#nchar will give you the number of characters in each quote, divide the number of rows (quotes).
mean_chars <- sum(nchar(df_quotes$quote))/nrow(df_quotes)


# 1.(c) Split df_quotes$quote by the space character and save the
# results in mylist.

mylist <- strsplit(df_quotes$quote, " ", fixed = TRUE)


# 1.(d) Use a for loop to calculate the mean number of words in the
# quotes in the character vector df_quotes$quote. Save the result in
# mean_words. [Hint: Base your calculation on mylist from the preceding
# part; don't make any changes to mylist. Treat each object in mylist as
# a vector of words, which is reasonable even if some of the character
# strings contain punctuation. Do not round the final result.]

wordcount <- 0
for(i in mylist){
  #The length of the character vector in "i" in the list is the number of words for that quote - sum these
  wordcount <- wordcount + length(i)
}
#Divide the total number of words by the length of the list - which is the number of quotes - to get average words per quote.
mean_words <- wordcount / length(mylist) 

# 1.(e) Flatten mylist and save the results in myvec.

myvec <- unlist(mylist)

###############################################################################

# 2. Read in the data file sample_data_1.csv and create a single for
# loop to calculate both the sum of all the numbers in the data as well
# as the product of all the numbers in the data. Save the sum in the
# object total_sum and the product in total_product. Write your code
# so that if one were to change sample_data_1.csv to sample_data_2.csv
# in your code, then your code would still calculate the new sum and
# new product correctly for the new data without requiring any other
# changes.

df <- read.csv("sample_data_1.csv")

total_sum <- 0
total_product <- 1
#create a variable that holds the column index of the number you want to sum in the file.  Makes it a little more flexible.
#If it was index 1 in file 1 and index 5 in file 2, you would just change this variable accordingly.
number_index <- 1 
for(i in 1:nrow(df)){
  total_sum <- total_sum + df[i,number_index] 
  total_product <- total_product * df[i,number_index]
}


###############################################################################

# 3. Create a character vector called men with the three character
# strings "Tim", "Bob", and "Ray". Also create a character vector called
# verbs with the two character strings "walks" and "runs". Then write a
# nested for loop to create a single character vector called sentences,
# which contains all possible sentences of the form "<man> <verb>." For
# example, "Tim walks." and "Ray runs." are two possibilities. [Hint:
# The vector sentences can be initialized with the command "sentences <-
# character()".]

sentences <- character()
men <- c("Tim","Bob","Ray")
verbs <-c("walks","runs")

for(m in 1:length(men)){
  for(v in 1:length(verbs)){
    sentences <- c(sentences,paste0(men[m]," ",verbs[v],"."))
  }
}
