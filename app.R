library(shiny)
library(tidyverse)
library(shinythemes)

# data
wrangled_df <- read.csv("wrangled_df.csv")
full_data <- wrangled_df %>%
   select(id, class, grad_school, time_between, is_athelete)  %>%
   # make distinct by id and class so each individual 
   # only contributing once to any enrollment post-bA
   distinct(id,class, .keep_all = TRUE)

# Create separate full dataset for degree to allow double-counting students
full_data2 <- wrangled_df %>%
   select(id, class, grad_school, time_between, degree) %>%
   distinct(id, class, degree, .keep_all = TRUE) %>%
   filter(grad_school == 1) %>%
   filter(is.na(degree) != TRUE)

# Generates proportion that goes to graduate school in a given year
dataagg <- full_data %>%
   group_by(class, grad_school) %>% 
   summarize(N = n()) %>%
   spread(grad_school, N) %>%
   filter(is.na(class) != TRUE) %>%
   rename(non_grad = "0") %>%
   rename(grad = "1") %>%
   mutate(total_students = non_grad + grad) %>%
   mutate(prop = grad/total_students) 

# Find sum of years between graduate school for a given year
# na.rm == TRUE so those who didn't go to graduate school will be ignored
dataagg2 <- aggregate(cbind(full_data$time_between), 
                      by = list(class = full_data$class), FUN = sum, na.rm = TRUE) %>%
   rename("sum_years_between"=V1)

# Finally generate average number of years between graduation and grad school for grad students
dataaggfinal <- dataagg %>%
   full_join(dataagg2, by = "class") %>%
   mutate(avg_years_between = sum_years_between/grad)

# Counts degree type
degreedata <- full_data2 %>%
   group_by(class, degree, grad_school) %>% 
   summarize(N = n()) %>%
   full_join(dataagg, by = "class") %>%
   select(class, degree, N, total_students) %>%
   mutate(prop = N/total_students)

degreedata2 <- aggregate(cbind(full_data2$time_between), 
                         by = list(class = full_data2$class, degree = full_data2$degree), 
                         FUN = sum, na.rm = TRUE) %>%
   rename("sum_years_between"=V1)

degreedatafinal <- degreedata %>%
   full_join(degreedata2, by = c("class", "degree")) %>%
   mutate(avg_years_between = sum_years_between/N)

# subset on names of degree types
degree_types <- c("BUSINESS", "LAW", "MEDICAL", "STEM MS"
                  , "STEM PhD", "OTHER MS", "OTHER PhD")

# Athlete Data calculation
athletedata <- full_data %>%
   group_by(class, is_athelete, grad_school) %>% 
   summarize(N = n()) %>%
   spread(grad_school, N) %>%
   filter(is.na(class) != TRUE) %>%
   rename(non_grad = "0") %>%
   rename(grad = "1") %>%
   mutate(total_students = non_grad + grad) %>%
   mutate(prop = grad/total_students) 


athletedata2 <- aggregate(cbind(full_data$time_between), 
                          by = list(class = full_data$class, is_athelete = full_data$is_athelete), 
                          FUN = sum, na.rm = TRUE) %>%
   rename("sum_years_between"=V1)

# Finally generate average number of years between graduation and grad school for grad students among athletes and not
athletedatafinal <- athletedata %>%
   full_join(athletedata2, by = c("class", "is_athelete")) %>%
   mutate(avg_years_between = sum_years_between/grad) %>%
   mutate(is_athlete = ifelse(is_athelete == 1, "Athlete", "Non-Athlete"))

athlete_types <- c("Athlete", "Non-Athlete")

# Define UI for application that creates a line plot for a given name
ui <- fluidPage(theme = shinytheme("cerulean"),
   
   # Application title
   titlePanel("Graduate School Enrollment"),
      
      # Show a plot of the generated distribution
         tabsetPanel(type = "tabs"
                     , tabPanel("Graduate School Enrollment by Degree Type",
                                sidebarLayout(sidebarPanel(
                                   selectInput(inputId = "dt",
                                               label = "Data Type:",
                                               choices = c("Proportion Enrolled in Graduate School", "Average Years Between"),
                                               selected = "Proportion Enrolled in Graduate School"),
                                   # Select Degree Type
                                   checkboxGroupInput(inputId = "dg", 
                                                      label = "Degree:",
                                                      choices = degree_types, 
                                                      selected = "BUSINESS"),
                                   # Limit Years
                                   sliderInput(inputId = "yrs", 
                                               label = "Year of Graduation",
                                               min = 2000, 
                                               max = 2020,
                                               value = c(2000, 2019),
                                               sep = "")),
                                   mainPanel(
                                      plotOutput(outputId ="degree")
                                   )
                                )),
                     
                        tabPanel("Graduate School Enrollment by Athletic Status",
                                sidebarLayout(sidebarPanel(
                                   selectInput(inputId = "dttwo",
                                               label = "Data Type:",
                                               choices = c("Proportion Enrolled in Graduate School", "Average Years Between"),
                                               selected = "Proportion Enrolled in Graduate School"),
                                   # Select Athlete Type
                                   checkboxGroupInput(inputId = "as", 
                                                      label = "Athletic status:",
                                                      choices = athlete_types,
                                                      selected = "Athlete"),
                                   # Limit Years
                                   sliderInput(inputId = "yrstwo", 
                                               label = "Year of Graduation",
                                               min = 2000, 
                                               max = 2020,
                                               value = c(2000, 2019),
                                               sep = "")),
                                   mainPanel(
                                      plotOutput(outputId ="athlete")
                                   )
                                )),
                     tabPanel("Total Graduate School Enrollment by Year",
                              sidebarLayout(sidebarPanel(
                                 selectInput(inputId = "dtthree",
                                             label = "Data Type:",
                                             choices = c("Proportion Enrolled in Graduate School", "Average Years Between"),
                                             selected = "Proportion Enrolled in Graduate School"),
                                 # Limit Years
                                 sliderInput(inputId = "yrsthree", 
                                             label = "Year of Graduation",
                                             min = 2000, 
                                             max = 2020,
                                             value = c(2000, 2019),
                                             sep = "")),
                                 mainPanel(
                                    plotOutput(outputId ="total")
                                 )
                              ))
         )
   
)


# Define server logic required to draw a histogram
server <- function(input, output) {
      
      output$degree <- renderPlot({
        # Make data reactive and select degrees and year range
        use_data <- reactive({
          data <- filter(degreedatafinal, degree %in% input$dg & class >= input$yrs[1] & class <= input$yrs[2])
        })
      
      # Use ifelse statements so that the inputs match the variable names and desired label names
      datavalue <- ifelse(input$dt == "Proportion Enrolled in Graduate School", "prop", "avg_years_between")
      ylabelvalue <- ifelse(input$dt == "Proportion Enrolled in Graduate School", "Proportion of Students Enrolled in Graduate School", "Average Time from Graduation to Enrollment")

      
      # Scatterplot grouped and colored by degree
      ggplot(data = use_data(), aes_string(x = "class", y = datavalue, group = "degree", color = "degree")) +
         geom_point()  + geom_line() +
         labs(x = "Year of Graduation", y = ylabelvalue
              , title = paste(paste(input$dt), "by Degree type")
              , color = "Degree")
      
   })
   
   
   
   output$athlete <- renderPlot({
      # Make data reactive and select degrees and year range
      athletepick <- ifelse(input$as == "Athlete", 1, 0)
      use_data <- reactive({
         data <- filter(athletedatafinal, is_athelete %in% athletepick & class >= input$yrstwo[1] & class <= input$yrstwo[2])
      })
      
     
      # Use ifelse statements so that the inputs match the variable names and desired label names
      datavalue <- ifelse(input$dttwo == "Proportion Enrolled in Graduate School", "prop", "avg_years_between")
      ylabelvalue <- ifelse(input$dttwo == "Proportion Enrolled in Graduate School", "Proportion of Students Enrolled in Graduate School", "Average Time from Graduation to Enrollment")
      
      # Scatterplot grouped and colored by degree
      ggplot(data = use_data(), aes_string(x = "class", y = datavalue, group = "is_athelete", color = "is_athlete")) +
         geom_point()  + geom_line() + 
         labs(x = "Year of Graduation", y = ylabelvalue
              , title = paste(paste(input$dttwo),"depending on Athletic status")
              , color = "Athletic Status")
      
   })
   
   
   output$total <- renderPlot({
      # Make data reactive and select degrees and year range
      use_data <- reactive({
         data <- filter(dataaggfinal, class >= input$yrsthree[1] & class <= input$yrsthree[2])
      })
      
      # Use ifelse statements so that the inputs match the variable names and desired label names
      datavalue <- ifelse(input$dtthree == "Proportion Enrolled in Graduate School", "prop", "avg_years_between")
      ylabelvalue <- ifelse(input$dtthree == "Proportion Enrolled in Graduate School", "Proportion of Students Enrolled in Graduate School", "Average Time from Graduation to Enrollment")
      
      # Scatterplot grouped and colored by degree
      ggplot(data = use_data(), aes_string(x = "class", y = datavalue)) +
         geom_point()  + geom_line() +
         labs(x = "Year of Graduation", y = ylabelvalue
              , title = paste(paste(input$dtthree), "by Year"))
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
