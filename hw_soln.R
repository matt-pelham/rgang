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


row_nums <- grep("fear", tolower(df_quotes$quote), fixed = TRUE)


# 1.(b) Calculate the mean number of characters in the quotes in the
# character vector df_quotes$quote. Save the result in mean_chars.
# [Hint: Base your calculation on *all* characters, including spaces
# and punctuation. Do not round the mean, and do not use a for loop to
# complete this task.]


mean_chars <- mean(nchar(df_quotes$quote))


# 1.(c) Split df_quotes$quote by the space character and save the
# results in mylist.


mylist <- strsplit(df_quotes$quote, " ")


# 1.(d) Use a for loop to calculate the mean number of words in the
# quotes in the character vector df_quotes$quote. Save the result in
# mean_words. [Hint: Base your calculation on mylist from the preceding
# part; don't make any changes to mylist. Treat each object in mylist as
# a vector of words, which is reasonable even if some of the character
# strings contain punctuation. Do not round the final result.]


# Initialize mean_words

mean_words <- 0

# Loop over the quotes in mylist

for(i in 1:length(mylist)) {

    # Add to mean_words the number of words in the quote

    mean_words <- mean_words + length(mylist[[i]])

}

# Divide by length(mylist) to get the final average

mean_words <- mean_words/length(mylist)


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


# Read in data

df <- read.csv("sample_data_1.csv")

# Initialize total_sum and total_product

total_sum <- 0
total_product <- 1

# Loop over the data

for(i in df$data) {

    # Update total_sum

    total_sum <- total_sum + i

    # Update total_product

    total_product <- total_product * i

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


# Setup data

men <- c("Tim", "Bob", "Ray")
verbs <- c("walks", "runs")

# Build sentences

sentences <- character()
for(man in men) {
    for(verb in verbs) {
        sentences <- c(sentences, paste(man, " ", verb, ".", sep = ""))
    }
}

