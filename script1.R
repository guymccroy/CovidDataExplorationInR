#load tidyverse library
library (tidyverse)

#This is an exercise in data exploration using R Programming and Excel. 
#The data was retrieved from https://ourworldindata.org. 
#This is a dataset with tons of information regarding covid cases around the 
#world. No dataset is perfect, but ourworldindata.org does an excellent job 
#describing the limitations of the dataset. 

#reading the csv file
#NOTE:CovidDeaths.csv contains data dated 02/01/2020 to 02/27/2023
coviddeaths <- read.csv("CovidDeaths.csv")

#After reading the file, we can see that the dataset contains 26 columns of 
#data. I'm mostly interested in understanding the total impact covid had on 
#world populations, so I need only a few columns to explore that. 

#I'm creating a smaller dataframe here. I'm using the select function to pick a
#few columns from the dataset. 
coviddeathsfiltered <- select(coviddeaths, location, date, total_cases,
                              total_deaths, population)

#Here, I'm removing the NA values because they don't help with calculations.
coviddeathsfiltereddroppednas <- drop_na(coviddeathsfiltered, location, date,
                                         total_cases, total_deaths, population)

#I'm creating a max total_cases dataframe which slices out the rows of data 
#with max total_cases. This is kind of a tricky thing to measure exactly, but 
#the logic here is that if you pull the rows of data that contain the highest 
#amount of covid cases, you will be able to calculate the total infection case
#percentage and risk of death percentage from the total population during that
#country's peak covid period. There are obviously many different ways of 
#looking at the data, but I'm interested in what the situation was like at 
#the height of infection, and that looks different for every country. 

#To get the rows of data I need, I use the group by function to group my rows 
#of data by location and population. I then used the slice function from dplyr
#to "Slice" or remove the rows of data by the total_cases column. 
max_total_casesdf <- coviddeathsfiltereddroppednas %>% 
  group_by(location, population) %>% slice(which.max(total_cases))

#I can calculate the percentage of people who died from covid AFTER contracting 
#it using a cross multiplication formula. It can then be stored in a new column 
#which we can name. I used the mutate function and plugged in a formula to 
#produce the new column. I named the new column death_percentage. 
max_tot_cas_with_death_per <- mutate(max_total_casesdf, death_percentage = 
                              total_deaths/total_cases*100)

#I'm using the mutate function again, but now I'm calculating the percentage 
#of the population infected and naming the column percent_pop_infected.
completedf <- mutate(max_tot_cas_with_death_per, percent_pop_infected
                               = (total_cases/population)*100)

#the summary function was used to see some summary statistics of the columns.
#this quickly tells me that the new columns have numerical data being 
#calculated in them. It also gives you a glimpse of the values within the 
#columns. 
summary(completedf)

#finally, I write the results into a csv file to analyze later using the 
#write.csv function. 
write.csv(completedf, "completedf.csv")

#Now that the dataset is saved, I have the choice of software to use to
#visualize the results. 


# Here, I Created a subset of the dataframe for graphing the major superpowers
# to compare their infection and death rates.
superpowers <- subset(completedf, location %in% c("United States", 
                                                  "China", 
                                                  "Russia",
                                                  "Germany", 
                                                  "United Kingdom", 
                                                  "South Korea",
                                                  "France", 
                                                  "Japan",
                                                  "United Arab Emirates",
                                                  "Israel"))

write.csv(superpowers, "superpowers.csv")

#With the subset created, we can more easily graph the locations we are 
#focused on. 
ggplot(superpowers, aes(x = location, y = percent_pop_infected)) + geom_bar(stat='identity') + labs(title = "Percent of Total Population Infected",)

ggplot(superpowers, aes(x = location, y = death_percentage)) + geom_bar(stat='identity') + labs(title = "Total Percentage of Deaths from Covid Cases",)
