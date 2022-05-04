getwd()
setwd("D:\\My Study Book\\Google Data Analytics\\Project\\Bollywood_Films")
getwd()

# Loading required libraries
library(tidyverse)
(.packages())

# Loading dataset
bollywood_df <- read.csv("bollywood_dataset.csv")
view(bollywood_df)
bollywood_df <- head(bollywood_df, - 2)
view(bollywood_df)
str(bollywood_df)


# How many movies were released from 1/01/2015 to 31/12/2021 ?
# Which year produced more films ?
# Average production of movies per year ?

summarise(bollywood_df, Total = n())

movies_produced_yearly <- bollywood_df %>% group_by(Released_Year) %>%
                                        summarise(Total_movies = n())
movies_produced_yearly

round(mean(movies_produced_yearly$Total_movies))

# During that period 684 movies were released in theatre and on ott platforms.
# Lowest production year was 2020 with total 66 movies and Highest production year was 2018 and 2019 as 
# both year produced total 127 movies
# Average production of movies per year is 98

# Does the production of movies increased or decreased year by year ? Is there any change and can we find why ?

graph_1 <- movies_produced_yearly %>%
  ggplot(aes(x=Released_Year, y=Total_movies)) + 
  geom_col(fill = "#1E90FF") +
  labs(x="Years", y="Total movies", title = "Bollywood Movies Released", subtitle = "From 1/01/2015 to 31/12/2021") +
  geom_text(aes(label = Total_movies), vjust = 3 ,  col = "White") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  geom_line(color="green",size=1.5) + 
  geom_point()

graph_1

# As we can see the graph the production increased year by year and dropped in 2020 it it happened cause of lockdown

# What is an average movie ratings of all movies ?
# Which year produced more quality content according to ratings ?
# Does the quality of movies increasing or decreasing year by year ?

mean(bollywood_df$IMDB_Ratings)

yearly_average_ratings <- bollywood_df %>% group_by(Released_Year) %>%
                                        summarise(mean_ratings = mean(IMDB_Ratings))

yearly_average_ratings

min(yearly_average_ratings$mean_ratings)
max(yearly_average_ratings$mean_ratings)

graph_2 <- yearly_average_ratings %>%
  ggplot(aes(x=Released_Year, y=mean_ratings)) +
  labs(x="Years", y="Ratings", title = "Bollywood Movies Ratings", subtitle = "Average per year") +
  geom_line(color="green",size=0.5) + 
  geom_point() + 
  ylim(0,10) 

graph_2

# The average movie rating 5.84
# Lowest 5.69 2015 and highest 6.25 2021
# We can say that content quality of the movies increased not much but it is positive

# Top 100 movies according to ratings
# Here I have filtered out those movies which have user counts less than 500 to avoid bias
top_100_ratings <- bollywood_df %>%
  filter(IMDB_Users >= 500)
  
top_100_ratings <- top_100_ratings[order(top_100_ratings$IMDB_Ratings, decreasing = TRUE),]
top_100_ratings <- head(top_100_ratings, 100)

view(top_100_ratings)
head(top_100_ratings,1)

# Top rank goes to Code Name Abdul movie with imdb rating of 8.9 according to ratings

# Top 100 movies according to budget
top_100_budget <- subset(bollywood_df, bollywood_df$Budget_in_crore.INR. != "null")
str(top_100_budget)
top_100_budget$Budget_in_crore.INR. <- as.numeric(top_100_budget$Budget_in_crore.INR.)
top_100_budget <- top_100_budget[order(top_100_budget$Budget_in_crore.INR., decreasing = TRUE),]
top_100_budget <- head(top_100_budget, 100)
view(top_100_budget)
head(top_100_budget,1) 

# Most expensive movie was Thugs of Hindustan with the budget of 310 cr

# Top 100 movies according to box office
top_100_box_office <- subset(bollywood_df, bollywood_df$Worldwide_box_office_gross_in_crore.INR. != "null")
str(top_100_box_office)
top_100_box_office$Worldwide_box_office_gross_in_crore.INR. <- as.numeric(top_100_box_office$Worldwide_box_office_gross_in_crore.INR.)
top_100_box_office <- top_100_box_office[order(top_100_box_office$Worldwide_box_office_gross_in_crore.INR., decreasing = TRUE),]
top_100_box_office <- head(top_100_box_office, 100)
view(top_100_box_office)
head(top_100_box_office,1)

# Dangal movie was the most succesfull movie with box office of 702.48

# Relationship between budget and box office 
# For a movie to become successful does it require high budget


str(top_100_box_office)
top_100_box_office$Budget_in_crore.INR. <- as.numeric(top_100_box_office$Budget_in_crore.INR.)

ref <- cor.test(top_100_box_office$Budget_in_crore.INR., top_100_box_office$Worldwide_box_office_gross_in_crore.INR.)
correlation_1 <- round(ref$estimate, digits = 4)

graph_3 <- top_100_box_office %>%
  ggplot(aes(x=Budget_in_crore.INR., y=Worldwide_box_office_gross_in_crore.INR.)) +
  geom_point() + 
  geom_smooth(method = lm) +
  labs(x="Budget", y="Box Office", title = "Budget Vs Box Office in crore (INR)", subtitle = "Pearson correlation") +
  geom_text(x=275, y=650, label= "r = ", size = 8) +
  geom_text(x=300, y=650, label= correlation_1, size = 8) 
 
graph_3

# As we can see there is some positive correlations as the budget increases there is a chance that 
# it will perform well in boxoffice


sum(top_100_box_office$Worldwide_box_office_gross_in_crore.INR.)
# Top 100 movies from bollywood earned approx 20892.71 cr (INR)

# Relationship between ratings and budget
str(top_100_box_office)
ref_2 <- cor.test(top_100_box_office$Budget_in_crore.INR., top_100_box_office$IMDB_Ratings)
correlation_2 <- round(ref_2$estimate, digits = 4)

graph_4 <- top_100_box_office %>%
  ggplot(aes(x=Budget_in_crore.INR., y=IMDB_Ratings)) +
  geom_point() +
  geom_smooth(method = lm) +
  ylim(0,10) +
  labs(x="Budget", y="Ratings", title = "Ratings Vs Budget", subtitle = "Pearson correlation") +
  geom_text(x=250, y=10, label= "r = ", size = 8) +
  geom_text(x=280, y=10, label= correlation_2, size = 8) 

graph_4

# High budget does not always guarantees good ratings

# Extracting some data sets for further use. 
write.csv(top_100_box_office, file = "top_100_box_office.csv")
write.csv(top_100_budget, file = "top_100_budget.csv")
write.csv(top_100_ratings, file = "top_100_ratings.csv")





