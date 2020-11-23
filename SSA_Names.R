library(tidyverse)
library(ggplot2)
library(lubridate)

#WD
library(rstudioapi)

# This links with the R Markdown file which actually contains the analysis. 
# This is just an exploratory file for that project

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )

#data from https://www.ssa.gov/oact/babynames/limits.html

#Finds the file names of each year (yob)
file_names <- Sys.glob("./data/yob*.txt")

#Custom function reads in data and adds year from filename
read_name_yob_data <- function(path){
  temp <- read_csv(path, col_types = "cci", col_names = FALSE)
  yob_id <- str_extract(path, "[0-9]{4}")
  temp %>% mutate(Year = yob_id)
}

#Create master list with every name
master <- map_dfr(file_names, read_name_yob_data)

#Rename
master <- master %>% rename(Name = X1, Sex = X2, Count = X3)

#Arrange the data so that each name/sex pair has only one row 
master <- master %>%
  spread(key = Year, value = Count)

#Replace NA with 0
master[is.na(master)] <- 0

#Create a table of name sums by sex
#This gives a tool for quick exploration of single names

name_sums <- master %>%
  mutate(Sum = rowSums(master[3:141])) %>%
  select(Name, Sex, Sum)

#Then, to get tidy data, we gather it back together
master <- master %>% 
  gather(key = Year, value = "Count", 3:141) %>%
  mutate(Year = as.numeric(Year))

#quick save and reload 
saveRDS(master, file = "master_names")
saveRDS(name_sums, file = "name_sums")

master <- readRDS(file = "./master_names")
name_sums <- readRDS(file = "./name_sums")


#We are interested in only relatively common names. Investigate the distribution
#Table with total occurrences

#name_sums

#Quick exploratory info: the median is only 45 total occurrences 
max(name_sums$Sum)
median(name_sums$Sum)
boxplot(name_sums$Sum)

#A naive visualization shows that the vast majority of names are clustered starting at 0 /yr and 5/yr 
#Note names with less than 5 occurrences are not included 
  
#Let's take a recent year and see what this implies
names_2014 <- read_csv(file_names[135], col_types = "cci", col_names = FALSE) %>%
  rename(Name = X1, Sex = X2, Count = X3)

names_2014 %>% filter(Count <= 100)

#We see that the vast majority of names in 2014 have less than 100 occurrences, 
# yet they are not necessarily extremely odd names
#For example we see "Cleo" and also alternate spellings like "Hollie" both at 100

common <- name_sums %>% filter(Sum >= 6000)

#For the purposes of this exercise, we are looking for names that are relatively common among both
# male and female and investigating the trends over time. 
#I have arbitrarily decided that over 6000 total occurrences is a good minimum for exploration
#I will bring back alternate spellings in the final analysis

common_pairs <- common[common$Name %in% common$Name[duplicated(common$Name)],]

#Now we have 116 names to investigate. Ignore the sums and extract it as a list
names_to_investigate <- common_pairs %>% distinct(Name)
names_to_investigate <- names_to_investigate$Name

#Raw data for all 116 pairs. 
pairs_to_investigate <- master %>% filter(Name %in% names_to_investigate)

#Define a function that will visualize each pair

plot_name <- function(name){
  to_plot <- master %>% filter(Name == as.character(name))
  ggplot(to_plot, aes(x = Year, y = Count, fill = Sex, color = Sex)) + 
    geom_line() +
    scale_x_continuous(breaks = seq(1880, 2020, 20)) + 
    labs(title = to_plot$Name) +
    theme_minimal()
}

#Function to account for common alternate spellings - use later

plot_names <- function(names){
  name_list <- list()
  for (i in length(names)){
    name_list[[i]] <- master %>% filter(Name == names[i])
  }
  name_table <- bind_rows(name_list)
  ggplot(name_table, aes(x = Year, y = Count, fill = Sex, color = Sex)) + 
    geom_line() +
    scale_x_continuous(breaks = seq(1880, 2020, 20)) + 
    labs(title = names[1], subtitle = paste(names[-1], collapse = " ")) +
    theme_minimal()
}

#Generate a graphic for each name in names to investigate (I did it in chunks and oberved the interesting results)
for (i in 1:6){
  print(plot_name(lc_names_to_investigate[i]))
}


#increase or decline in tandem: F dom
tandem_f <- c("Alexis", "Ariel", "Avery", "Bailey", "Billie", "Bobbie", "Cleo", "Dana", "Dominique", "Guadalupe",
  "Jamie", "Kasey", "Kelly", "Laverne", "Lynn", "Marion", "Ollie", "Robin", "Shannon",
  "Taylor", "Tracy")

  #check
  name_sums %>% 
    filter(Name %in% tandem_f) %>% 
    spread(key = Sex, value = Sum) %>% 
    mutate(ratio = M/F) %>%
    arrange((ratio)) %>%
    print(n = 21)


#increase or decline in tandem: M dom
tandem_m <- c("Dakota", "Dallas", "Devon", "Hayden", 
  "Jaden", "Jimmie", "Johnnie", "Jordan", "Lee", "Merle", "Noel", "Parker", 
  "Terry")

  #check
  name_sums %>% 
    filter(Name %in% tandem_m) %>% 
    spread(key = Sex, value = Sum) %>% 
    mutate(ratio = F/M) %>%
    arrange((ratio))

#increase or decline in tandem: tie
tied <- c("Amari", "Armani", "Carey", "Casey", "Devyn", "Emerson", "Finley", "Jackie", "Jaylin", "Jessie", "Jody", "Justice",
  "Kerry", "Kris", "Pat", "Payton", "Peyton", "Sage", "Skyler", "Tommie", "Rowan")

  #check
  name_sums %>% 
    filter(Name %in% tied) %>% 
    spread(key = Sex, value = Sum) %>% 
    mutate(ratio = F/M) %>%
    arrange((ratio)) %>%
    rename("ratio female to male" = ratio) %>%
    print(n = 21)
  
  #Peytons both go here because imo they are fundamentally the same name. Ratio F/M is:
  ((52437+71280)/(23719+48477))

#women take over male name
women_take <- c("Aubrey", "Blair", "Gale", "Harley", "Kendall", "Kim", "Leslie", "Morgan", "Quinn", "Reese", "Riley", "Sidney",
  "Skylar")

#men take over female name
men_take <- c("Ashton", "Robbie")

#cross-cultural
cross_cult <- c("Angel", "Jaime", "Jan", "Jean", "Rene")

#female dominant
female_dom <- c("Addison", "Ashley", "Carmen", "Courtney", "Mary", "Sandy", "Shelby", "Stacey", "Stacy")

  #check
  name_sums %>% 
    filter(Name %in% female_dom) %>% 
    spread(key = Sex, value = Sum) %>% 
    mutate(ratio = M/F) %>%
    arrange(desc(ratio))

#male dominant
male_dom <- c("Adrian", "Cameron", "Charles", "Chris", "Christian", "Dale", "David", "Devin", "Dylan", "Eddie", "Francis", "Freddie", "Hunter", "James", "Jayden", "Jerry", "Jesse",
  "Joe", "John", "Joseph", "Logan", "Micah", "Michael", "Shawn", "Robert", "Ryan", "Tyler", "William")

  #check
  name_sums %>% 
    filter(Name %in% male_dom) %>% 
    spread(key = Sex, value = Sum) %>% 
    mutate(ratio = F/M) %>%
    arrange(desc(ratio))


#in process or otherwise interesting
interesting <- c("Blake", "Charlie", "Elisha", "Emery", "Frankie", "Leighton", "Loren", "Mckinley")

#and "unknown"

for (i in 1:length(interesting)){
  print(plot_name(interesting[i]))
}

