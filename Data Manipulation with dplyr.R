#Load Libraries----
library(dplyr)
library(readr)
library(ggplot2)
library(ggtext)


#Baby names dataset----
baby_names <-  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

#Filter Function----
#Using Filter Function to get the desired name and saving it in an object
babynames_filtered <- baby_names %>% 
    filter(name == "David")

#Plot using ggplot----
ggplot(babynames_filtered,aes(x = year,y = prop)) +
    geom_line()+
    ggtitle('Name David for a baby' , 'Source: Baby Names dataset')
    
#Filter Multiple names
babynames_combined <- baby_names %>% 
    filter(name %in% c("David" , "Christopher"))

#When was each name most common? ----
top_common_names <- baby_names %>% 
    group_by(name) %>% 
    top_n(1, year)

#Filter for the year 2015
   baby_names %>%
    # Filter for the year 2015
    filter(year == 2015) %>%
    # Sort the number column in descending order 
    arrange(desc(n))

#most common name in every year
   most_common_name <- baby_names %>%
       # Find the most common name in each year
       group_by(year) %>%
       top_n(1,)

#Filter only the names Rachel,Monica,Phoebe,Chandler,Ross,Joey
   Friends_names <- baby_names %>%
       # Filter for the names Steven, Thomas, and Matthew 
       filter(name %in% c("Rachel", "Monica", "Phoebe","Chandler","Ross","Joey"))
   

# Plot the names using a different color for each name
   ggplot(Friends_names, aes(x = year, y = n, color = name)) +
       geom_line()
   
#Total number of baby count each year using summarize----
   baby_names %>% 
       group_by(year) %>% 
       summarize(year_total = sum(n))

#Mutate function  instead of summarize ----
  
  baby_names %>% 
   group_by(year) %>% 
       mutate(year_total = sum(n))
   
 #Using Ungroup ----
   baby_names %>% 
       group_by(year) %>% 
       mutate(year_total = sum(n)) %>% 
       ungroup()
   
 #Fraction of people born in each year with each name ----
   baby_names %>% 
       group_by(year) %>% 
       mutate(year_total = sum(n)) %>% 
       ungroup() %>% 
       mutate(fraction = n / year_total) 
   
   
   
  #Finding the year each name is most common
       baby_names %>%
       group_by(year) %>%
       mutate(year_total = sum(n)) %>%
       ungroup() %>%
       mutate(fraction = n / year_total) %>%
       # Find the year each name is most common
       group_by(name) %>%
       top_n(1,fraction)
   
#Popularity of a name in each year ----
  names_history <-  baby_names %>%
       # Add columns name_total and name_max for each name
       group_by(name) %>%
       mutate(name_total = sum(n),
              name_max = max(n)) %>% 
       ungroup() %>%
       # Add the fraction_max column containing the number by the name maximum 
      mutate(fraction_max = n / name_max)
       
#Visualizing the normalized change in popularity ----
       names_filtered <- names_history %>% 
          # Filter for the names Tom, Dick, and Harry
          filter(name %in% c("John", "Jane", "George"))
       
       # Visualize these names over time
       ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) +
          geom_line()
       


   

   
 
   
   
   
   
   
