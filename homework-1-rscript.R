library(tidyverse)
### Set your directory
setwd("C:/Users/zades/Documents/GitHub/problem-set-1-ScavengerDLC")



################################################################################
################################################################################
################################################################################
#######                                                                  #######
#######                     Code for Question 1                          #######
#######                                                                  #######
################################################################################
################################################################################
################################################################################



### csvs originally returned an error, more columns than column names. After 
### comparing these csvs to previous ones that I have received I noticed that
### this one had extra information at the top.

employment_totals <- read.csv("SAEMP25N total.csv", skip = 4)

employment_by_industry <- read.csv("SAEMP25N by industry.csv", skip = 4)


### Filter Out "By industry rows" that contain no data 
### See problem in console by running line below 
employment_by_industry |> filter(is.na(LineCode)) |> head(5)

employment_by_industry <- employment_by_industry |> filter(!is.na(LineCode))


### Fix industry names so don't have spaces. Makes testing a lot easier

employment_by_industry <- employment_by_industry |>
  mutate(Description = make.names(Description)) |>
  mutate(Description = str_replace_all(Description, "X", "")) |>
  mutate(Description = str_replace_all(Description, "\\.", ""))


### Made (D) and (T) in employment_by_industry NAs, will solve issue later 
### Not the most elegant solution, but it is quicker than a function for me.

employment_by_industry$X2000 <- na_if(employment_by_industry$X2000, "(D)")
employment_by_industry$X2000 <- na_if(employment_by_industry$X2000, "(T)")
employment_by_industry$X2017 <- na_if(employment_by_industry$X2017, "(D)")
employment_by_industry$X2017 <- na_if(employment_by_industry$X2017, "(T)")


### Pivot dataframes so year and value have their own column and change values to 
### numeric, will solve issue later Year being year breaks the function later, 
### so observed_year is the year that the observation took place.

employment_by_industry <- employment_by_industry |>
  pivot_longer(cols = starts_with("X"), names_to = "observed_year",
               names_transform = list(Year = as.integer),
               names_prefix = "X",
               values_to = "Jobs",
               values_transform = list(Jobs = as.integer)
               )

employment_totals <- employment_totals |>
  pivot_longer(cols = starts_with("X"), names_to = "observed_year",
               names_transform = list(Year = as.integer),
               names_prefix = "X",
               values_to = "Total",
               values_transform = list(Total = as.integer)
  )




### Remove Linecode variable, causes problems in next step if not done, and 
### Pivot by_industry dataframe so each industry has their own column

employment_by_industry <- employment_by_industry |>
  select(!LineCode) |>
  pivot_wider(names_from = Description, values_from = Jobs)


### Join the dataframes

data <- left_join(employment_by_industry, employment_totals, 
                  by = c("observed_year", "GeoName", "GeoFips"))


### Change Industry totals to a proportion of state totals

for (i in 4:13) {
  data[i] <- data[i] / data$Total
}


### Remove Unnecessary variables and save to CSV

data <- data |> select(!GeoFips & !Total)

write.csv(data, "data.csv")



################################################################################
################################################################################
################################################################################
#######                                                                  #######
#######                     Code for Question 2                          #######
#######                                                                  #######
################################################################################
################################################################################
################################################################################



### Create Function with input of number of states, industry, and the year

industry_rank_finder <- function(n, ind, year_select){
  ranking <- vector()
  ranking <- data |> 
  filter(observed_year == year_select) |>
  arrange(desc({{ind}})) |>
  select(GeoName)|>
  head(n)
  print(ranking)
}


### YES!!! FINALLY WORKS

industry_rank_finder(5, Farmemployment , 2000)


### 2A Find top 5 States with largest manufacturing shares

ranking <- industry_rank_finder(5, Manufacturing, 2000)


### 2B Find Share from Both Years

comparison_table <- data |> select(GeoName | observed_year | Manufacturing) |> 
  inner_join(ranking, join_by(GeoName))
  

### 2C Make the Graph

comparison_table |>
  ggplot(aes(x = observed_year, y = Manufacturing, colour = GeoName, group = GeoName)) +
  geom_point(size = 2) + geom_line(size = 1) +
  xlab("Year") + 
  labs(color = NULL) +
  ylab("Share of Jobs in Manufacturing") +
  ggtitle("States with the Largest within-state Manufacturing Shares in 2000")


### Not sure how I would put all of this in a loop but here are the requested 
### graphs in 2D

### Top 10 Farming
### Make the List
ranking <- industry_rank_finder(10, Farmemployment, 2000)

### Find Share from Both Years
comparison_table <- data |> select(GeoName | observed_year | Farmemployment) |> 
  inner_join(ranking, join_by(GeoName))

### Make the Graph
comparison_table |>
  ggplot(aes(x = observed_year, y = Farmemployment, colour = GeoName, group = GeoName)) +
  geom_point(size = 2) + geom_line(size = 1) +
  xlab("Year") + 
  labs(color = NULL) +
  ylab("Share of Jobs in Farming") +
  ggtitle("States with the Largest within-state Farming Shares in 2000")


### Top 15 Information
### Make the List
ranking <- industry_rank_finder(15, Information, 2017)

### Find Share from Both Years
comparison_table <- data |> select(GeoName | observed_year | Information) |> 
  inner_join(ranking, join_by(GeoName))

### Make the Graph
comparison_table |>
  ggplot(aes(x = observed_year, y = Information, colour = GeoName, group = GeoName)) +
  geom_point(size = 2) + geom_line(size = 1) +
  xlab("Year") + 
  labs(color = NULL) +
  ylab("Share of Jobs in Information") +
  ggtitle("States with the Largest within-state Information Shares in 2017")



################################################################################
################################################################################
################################################################################
#######                                                                  #######
#######                     Code for Question 3                          #######
#######                                                                  #######
################################################################################
################################################################################
################################################################################


### Make a function that outputs the two desired plots. Comments denote each
### step of the function. Make sure to input state_select as "string"
### Graphs are output as year_comparison.pdf and difference.pdf


industry_plot_maker <- function(ind, state_select){
  ### Make Data Frame that will be used only for these two graphs
  graphed_data <- data |>
    select(GeoName | observed_year | {{ind}})
  
  other_graphed_data <- data |> 
    select(GeoName | observed_year | {{ind}})
  
  ### Split into 2 data frames to find the average. I tried just about every
  ### other method of finding the mean that I can think of and this is the last 
  ### one that could work with the {{}} of the function by working around it. Instead
  ### of calling the variable name explicitly, I know that the industry will always be
  ### the third column. Taking advantage of that fact, we can call the industry no 
  ### matter what the name is using the index. 
  graphed_data_2000 <- graphed_data |>
    filter(observed_year == 2000)
  
  graphed_data_2017 <- graphed_data |>
    filter(observed_year == 2017)
  
  ### Find Industry Mean for Each Year. I probably could combine a lot of these 
  ### next steps, however I am paranoid that it won't work and I actually got 
  ### this to work first try by stripping every step down to it's smallest components
  ### It took a while and lot of thinking through, but it works!
  industry_mean_2000 <- mean(graphed_data_2000[[3]], na.rm = TRUE)
  
  industry_mean_2017 <- mean(graphed_data_2017[[3]], na.rm = TRUE)
  
  ### Add Industry Mean to Main Dataframe
  graphed_data <- graphed_data |>
    mutate(
      industry_mean = ifelse(observed_year == 2000, industry_mean_2000, industry_mean_2017)
    )
  
  ### Do the same thing for the desired state's data
  state_2000 <- graphed_data[[3]][
    graphed_data$observed_year == 2000 & graphed_data$GeoName == {{state_select}}
  ]
  
  state_2017 <- graphed_data[[3]][
    graphed_data$observed_year == 2017 & graphed_data$GeoName == {{state_select}}
  ]
  
  graphed_data <- graphed_data |>
    mutate(
      state_desired = ifelse(observed_year == 2000, state_2000, state_2017)
    )

  ### Make Year Comparison Graph
  ggplot(graphed_data) +
    geom_histogram(aes(x = {{ind}}), fill = "#FB702D", binwidth = 0.01) +
    geom_vline(aes(xintercept = industry_mean, color = "Mean")) +
    geom_vline(aes(xintercept = state_desired, color = {{state_select}})) +
    scale_color_manual(values = c("#862FE0", "#88E032")) +
    labs(color = NULL) + 
    facet_wrap( ~ observed_year) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
    ggsave("year_comparison.pdf", plot = last_plot())
    
    print("Graph One Complete")
    
    ### Graph One is done
    
    
    ### Start working on the other graph
    other_graphed_data <- data |> 
      select(GeoName | observed_year | {{ind}})
    
    ### Change year values to something different that is more consistently 
    ### interpreted by R
    
    other_graphed_data <- other_graphed_data |>
      mutate(
        observed_year = ifelse(observed_year == 2000, "year_2000", "year_2017")
      )
    
    ### Pivot Dataframe for Easy Subtraction
    other_graphed_data <- other_graphed_data |>
      pivot_wider(names_from = observed_year, values_from = {{ind}})
    
    ### Create Difference Variable and remove extra variables
    other_graphed_data <- other_graphed_data |> 
      mutate(share_difference = year_2017 - year_2000) |>
      select(GeoName | share_difference)
    
    ### Find Average Difference and Value of Desired State using index method to
    ### avoid the headache of using the variable
    average_difference <- mean(other_graphed_data[[2]], na.rm = TRUE)
    
    state_change <- other_graphed_data[[2]][other_graphed_data$GeoName == {{state_select}}]
    
    ### Insert values for Lines on histogram into the dataframe
    other_graphed_data <- other_graphed_data |>
      mutate(
        industry_average = average_difference,
        state_difference = state_change
      )
    
    ### Make Plot
    ggplot(other_graphed_data) +
      geom_histogram(aes(x = share_difference), fill = "#FB702D", bins = 15) +
      geom_vline(aes(xintercept = industry_average, color = "Mean")) +
      geom_vline(aes(xintercept = state_difference, color = {{state_select}})) +
      scale_color_manual(values = c("#862FE0", "#88E032")) +
      labs(color = NULL) + 
      xlab("Difference in Employment Share") + 
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
    ggsave("difference.pdf", plot = last_plot())
    
    print("Graph Two Complete")
}

################################################################################
################################################################################
################################################################################
#######                                                                  #######
#######                       End of Function                            #######
#######                                                                  #######
################################################################################
################################################################################
################################################################################

industry_plot_maker(Financeandinsurance, "Wyoming")
