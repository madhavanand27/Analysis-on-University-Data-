
library(tidyverse)
library(ggplot2)
library(dplyr)
library(billboarder)
library(tidyr)
library(plotly)
library(treemapify)
library(treemap)

df <- read.csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\university_by_cnt.csv")
df %>% 
  group_by(country_name) %>% 
  summarize(number_of_uni = n()) %>% 
  arrange(-number_of_uni) %>% 
  top_n(10) %>% 
  ggplot(mapping = aes(x = reorder(country_name, number_of_uni), y = number_of_uni))+
  geom_bar(stat = "identity", fill = "deepskyblue4")+
  labs(title = "Number of universities by countries", 
       subtitle = "The bar chart shows top 10 countries which has the maximum number of universities.",
       x = "Country Names", y = "Number of Universities", caption = "
  R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for
  Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.")+
  theme(rect = element_blank(), 
        plot.title = element_text(size = 16, face = "bold", color = "deepskyblue4"),
        axis.title = element_text(size = 12, face = "italic"), 
        axis.text.x = element_text(size = 10, colour = "black"),
        plot.subtitle = element_text(size = 10, face = "italic"))

max_num_of_uni_by_country_name <- read.csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\max_num_of_uni_by_country_namee.csv")
ggplot(max_num_of_uni_by_country_name, aes(x = reorder(country_name, number_of_universities), y = number_of_universities))+
  geom_bar(stat = "identity", fill = "red4")+
  labs(title = "Number of Universities that are Below Average", 
       subtitle = "The higher the bar is, the more the universities have scores that are below average.",
       x = "Countries", y = "Number of Universities",
       caption = "R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for
  Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.")+
  theme(rect = element_blank(),
        plot.title = element_text(size = 16, face = "bold", color = "red4"),
        axis.title = element_text(size = 12, face = "italic"), 
        axis.text.x = element_text(size = 10, colour = "black"),
        plot.subtitle = element_text(size = 10, face = "italic"))
  

criteria <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\most used criteria of evaluating universities.csv")
billboarder() %>% bb_piechart(criteria) %>%
  bb_title(text = "Most Used Criteria of Evaluating Universities", position = "center")


max_num_of_uni_by_country_name_above_avg <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\number of universities that are above average.csv")  

ggplot(max_num_of_uni_by_country_name_above_avg, aes(x = reorder(country_name, number_of_universities), y = number_of_universities))+
  geom_bar(stat = "identity", fill = "green4")+
  labs(title = "Number of Universities that are Above Average", 
       subtitle = "The higher the bar is, the more the universities have scores that are above average.",
       x = "Countries", y = "Number of Universities",
       caption = "R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for
  Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.")+
  theme(rect = element_blank(),
        plot.title = element_text(size = 16, face = "bold", color = "green4"),
        axis.title = element_text(size = 12, face = "italic"), 
        axis.text.x = element_text(size = 10, colour = "black"),
        plot.subtitle = element_text(size = 10, face = "italic"))

internatioanl_total_students <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\internatioanl_total_students.csv")

ggplot(internatioanl_total_students, aes(x = reorder(country_name, total_international_students), y = total_international_students))+
  geom_bar(stat = "identity", fill = "purple4")+
  theme(rect = element_blank(), 
        axis.text.x = element_text(angle = 90, size = 10, colour = "black"),
        plot.title = element_text(size = 16, face = "bold", colour = "purple4"),
        axis.title = element_text(size = 12, face = "italic"),
        plot.subtitle = element_text(size = 10, face = "italic"))+
  scale_y_continuous(labels = scales::comma)+
  labs(title = "Number of International Students by Countries", 
       subtitle = "Higher the bar is, more international students goes to that country.", 
       caption = "R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for
  Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.", x = "Countries", 
       y = "Number of International Students")

fmale_male_int_total_studentsm <- read.csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\fmale_male_int_total_studentsm Query 7.csv")

#Data formatting. 
fmale_male_int_total_studentsm$total_female_students <- as.numeric(fmale_male_int_total_studentsm$total_female_students)
fmale_male_int_total_studentsm$total_local_male_students <- as.numeric(fmale_male_int_total_studentsm$total_local_male_students)

#un-pivoting the data. 
fmale_male_int_total_studentsm <- fmale_male_int_total_studentsm %>% 
  pivot_longer(cols = c("total_international_students" : "total_female_students" : 
                           "total_students" : "total_local_male_students"),
               values_to = "number_of_students", names_to = "student_type")



plot_ly(fmale_male_int_total_studentsm,
        x = ~ student_type,
        y = ~ number_of_students,
        frame = ~ university_name,
        type = 'bar',
        color = ~ university_name )

#Query 8
ranking_system <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\ranking system.csv")
billboarder() %>% 
  bb_piechart(ranking_system) %>% 
  bb_title(text = "Which ranking system is used most by the universities", position = "center")


#Query 9
numberOfUniByCountry <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\numberOfUniByCountry.csv")

tree <- ggplot(numberOfUniByCountry, aes(area = number_of_universities, 
                                 label = country_name, fill = number_of_universities))+
  geom_treemap(color = "black")+
  geom_treemap_text(size = 8)+
  labs(fill = "Number of Universities", title = "Number of Universities by Country", 
       subtitle = "The more orange or bigger the block is, hight the value will be.",
       caption = "R Core Team (2023). _R: A Language and Environment for Statistical Computing_. R Foundation for
  Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.")+
  theme(legend.position = "null")+
  scale_fill_gradient(low = "lightblue", high = "orange2")
tree

#query 10
student_staff_ratio <- read_csv("C:\\Users\\User\\OneDrive\\Desktop\\Business Analytics\\SQL\\Proj_uni\\csv_for_analysis\\student_staff_ratio.csv")

billboarder() %>% 
  bb_piechart(student_staff_ratio) %>% 
  bb_title(text = "The top 10 Universities have student staff ratio more than the average.", position = "center")
