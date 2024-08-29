library(tidyverse)
library(palmerpenguins)
data(penguins)
glimpse(penguins)

# z = x - (mean(x))/sd
# reason to write a function is to limit errors and copy and pasting code

penguins <- penguins |>
  mutate(
    z_bill_length_mm = (bill_length_mm - mean(bill_length_mm, na.rm = TRUE)) / sd(bill_length_mm, na.rm = TRUE),
    z_bill_depth_mm = (bill_depth_mm - mean(bill_depth_mm, na.rm = TRUE)) / sd(bill_depth_mm, na.rm = TRUE),
    z_flipper_length_mm = (flipper_length_mm - mean(flipper_length_mm, na.rm = TRUE)) / sd(flipper_length_mm, na.rm = TRUE),
    z_body_mass_g = (body_mass_g - mean(body_mass_g, na.rm = TRUE)) / sd(body_mass_g, na.rm = TRUE)
  )

# The way to make this shorter and clearer is to write a functions
# Types of functions
# 1. Vector functions: one of more vectors as input, one vector as output
# It is good to give parameters a default so they do something sensible when the arguemnt isnt provided
z_score_trans <- function(column,middle = 1){
   trim = (1 - middle) / 2
  (column - mean(column,na.rm = T, trim = trim))/ sd(column,na.rm = TRUE)
}


penguins <- penguins %>%
  mutate(z_bill_depth_mm = z_score_trans(bill_length_mm, 0.9))


# Write a function to compute the standard error

sd_error <- function(x){
  sd(x,na.rm = TRUE)/ sqrt(sum(!is.na(x)))
}

sd_error(penguins$body_mass_g)



# Write a Box-Cox function




# Write a function to compute the sums of squares

sum_of_squares <- function(x){
  sum((!is.na(x) - mean(x)))
}

sum_of_squares(penguins$body_mass_g)

# Data frame functions
# arguemnt is the dataframe and the column

my_summary <- function(df, column){
  df %>%
    summarise(mean = mean({{ column }}, na.rm = T),
              sd = sd({{ column }}, na.rm = T))
}

my_summary(penguins,bill_depth_mm)


# Write a function to do mean,max and min values
# adding NULL means that it will work regardless of grouping specification
# adding PICK function allow you to select a subset of columns
my_summary2 <- function(df,column,group_var = NULL){
  df %>%
    group_by(pick({{group_var}})) %>%
    summarise(median = median({{column}}, na.rm = T),
              max = max({{column}}, na.rm = T),
              min = min({{column}}, na.rm = T))
}

my_summary2(penguins,bill_depth_mm,species)


