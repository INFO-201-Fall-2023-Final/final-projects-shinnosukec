library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("data_wrangling.R")

about_view <- fluidPage(
  titlePanel("Education Attainment and Smoking Habit"),
  br(),
  h4("INFO201 Team BC-1 (Meihang Huang, Rujia Zhang, Shinnosuke Chuman"),
  br(),
  h3("Our analysis and Why Should Someone Care?"),
  br(),
  img(src = "smoking.jpeg"),
  br(),
  br(),
  p(
    "In the modern world of public health concerns, the pervasive issue of tobacco usage remains large, accompanied by various socioeconomic factors. Our project aims to unravel a complex relationship, shedding light on the cyclical dynamic between tobacco use and the pursuit and completion of educational levels. Many might predict that a more educated person has less or no smoking habit. However, smoking is perceived as one of the most common methods of stress relief. We believe that there is a possibility that a person in a highly educated environment experiences more stress which leads to a smoking habit. "
  ),
  br(),
  p(
    "Based on two datasets we found, we will explore how individuals with different levels of educational attainment exhibit varying patterns of tobacco use, examining whether lower educational levels correlate with higher smoking rates, and conversely, if higher educational achievement acts as a buffer against tobacco consumption."
  ),
  br(),
  h3("Target People"),
  br(),
  p(
    "This investigation is pertinent for policymakers, educators, and health professionals, offering insights into targeted interventions and raising awareness about the broader social determinants of health behaviors."
  ),
  br(),
  h3("Datasets"),
  br(),
  p(
    "In order to explore the relationship between tobacco usage and completion of educational milestones, we chose a combination of two datasets:"
  ),
  br(),
  h5(
    a("Behavioral Risk Factor Data: Tobacco", href = "https://data.cdc.gov/Survey-Data/Behavioral-Risk-Factor-Data-Tobacco-Use-2011-to-pr/wsas-xwh5/about_data")
  ),
  p(
    "Description: State tobacco activities tracking and evaluation, 2011-2019. Tobacco topics included are cigarette and e-cigarette use prevalence by demographics, cigarette and e-cigarette use frequency, and quit attempts."
  ),
  p(
    "Published by: Centers for Disease Control and Prevention (CDC) under U.S. Department of Health & Human Services"
  ),
  p(
    "Collected by: BRFSS - a continuous, state-based surveillance system by CDC that collects information about modifiable risk factors for chronic diseases and other leading causes of death"
  ),
  p(
    "Rows: 43341 rows | Each row represents a state with different year and smoking categories"
  ),
  p(
    "Columns: 31 columns | Year, state, activity type (e.g. cigarette), percentage, etc"
  ),
  br(),
  h5(
    a("S1501 EDUCATIONAL ATTAINMENT", href = "https://data.census.gov/table/ACSST1Y2019.S1501?q=S1501&t=Educational+Attainment&g=010XX00US$0400000&moe=false&tp=false")
  ),
  p(
    "Description: Educational attainment by state, demographics, and year."
  ),
  p("Published by: U.S. Census Bureau"),
  p(
    "Collected by: American Community Survey - an annual demographics survey program conducted by the U.S. Census Bureau"
  ),
  p(
    "Rows: 68 rows per year | Each row is different  educational attainment categories (e.g. high school graduate, bachelor’s degree, etc…) by age groups"
  ),
  p(
    "Columns: 313 columns per year | Leftmost column represents educational attainment categories. Rest of the columns represent states. Each state has six columns (total, total percent, male, male percent, female, female percent) 52 states & territory × 6 categories = 312 columns."
  ),
  br(),
  p(
    "After cleaning and joining two datasets, the dataset includes information from 2011 to 2019 and contains the following columns:"
  ),
  p(
    HTML(
      "Year: The year of the data. <br>
         State: The U.S. state to which the data corresponds. <br>
         Percent of bachelor's degree or higher: The percentage of people with at least a bachelor's degree in the state. <br>
         Percentage of cig user: The percentage of cigarette users in the state. <br>
         Percentage of E-cig user: The percentage of electronic cigarette users in the state. <br>
         E-cig existence: Indicates whether e-cigarettes were available or known during that year in the state. <br>
         E-cig ratio: The ratio of e-cigarette users to the total population in the state."
    )
  )
)

interactive_view1 <- fluidPage(
  titlePanel(
    "Scatterplot: Average Educational Attainment VS Cigarrete Smoking Habit from 2011 to 2019"
  ),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state_view1",
        label = "Choose a state",
        choices = df$State,
        selected = TRUE
      ),
      p("All time average percent of:"),
      htmlOutput(outputId = "alltime_stats"),
      br()
    ),
    mainPanel(plotlyOutput(outputId = "scatter"))
  ),
  
  h3("Observation and Analysis"),
  br(),
  p(HTML(
    paste(
      "In this scatterplot, each data point represents a state of the United States. It illustrates the relationship between the average percentage of bachelor's degrees or higher and the average percentage of smokers in its state's population. The average is calculated based on data from 2011 to 2019. <br>

From this scatter plot, a very clear trend can be observed - as the percentage of smokers increases, the percentage of bachelor's degrees or higher decreases. <br>

This trend implies that lower education levels correlate with higher smoking rates. <br>

A correlation coefficient computed using the cor() function is",
      cor_avg,
      "which is surprisingly low after looking at this obvious trend. This is most likely because of outliers. District of Columbia has a spectacularly high education level compared to other states with a similar smoking population. On the other hand, Puerto Rico has a very low smoking population despite its education level is relatively low."
    )
  )),
)

interactive_view2 <- fluidPage(
  titlePanel(
    "Scatterplot: Average Educational Attainment VS E-cig Smoking Habit from 2016 to 2017"
  ),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "state_view2",
        label = "Choose a state",
        choices = df$State,
        selected = TRUE
      ),
      p("All time average percent of:"),
      htmlOutput(outputId = "alltime_stats_ecig"),
      br()
    ),
    mainPanel(plotlyOutput(outputId = "scatter2"))
  ),
  h3("Observation and Analysis"),
  br(),
  p(HTML(
    paste(
      "This scatterplot specifically focuses on e-cigarette usage. The dataset only provides data about e-cigarettes fully in 2016 and 2017. Therefore, it is plotted based sorely on these two years. <br>

From this scatter plot, a familiar trend can be observed - as the percentage of cigarette users increases, the percentage of bachelor's degrees or higher decreases. However, by eyeballing the trend, it seems weaker. The points are more spread out making it harder to see the familiar declining trend line. <br>

Though the trend is not as clear as the cigarrete users scatterplot, it still implies that lower education levels correlate with higher smoking rates. <br>

As for the previous plot, a correlation coefficient computed using the cor() function is",
      cor_avg_ecig,
      "which is low due to outliers - the District of Columbia and Puerto Rico."
    )
  ))
)

interactive_view3 <- fluidPage(
  titlePanel(
    "Line Graph: Trend of Educational Attainment and Smoking Habit by State from 2011 to 2019"
  ),
  br(),
  sidebarLayout(sidebarPanel(
    selectInput(
      inputId = "state_view3",
      label = "Choose a state",
      choices = df_view3$State,
      selected = TRUE
    ),
    checkboxInput(
      inputId = "edu_on",
      label = "Show the line for Percent of Bachelor's Degree or Higher",
      value = TRUE
    ),
    checkboxInput(
      inputId = "cig_on",
      label = "Show the line for Percent of Cig Smoker",
      value = TRUE
    )
  ),
  mainPanel(plotlyOutput(outputId = "line"))),
  
  h3("Observation and Analysis"),
  br(),
  p(HTML(paste("This line graph shows the trend of the percentage of bachelor's degrees or higher and the percentage of cigarette users from 2011 and 2019. <br>
For the average of all states, it can be observed that the percentage of bachelor's degrees or higher has increased. On the other hand, the percentage of cigarette users has decreased over the years. This trend can also be seen in most states when it is individually observed. <br>
These line graphs indicate that the average education level of people has increased while cigarette users decreased in the United States.")))
  
)

ui <- navbarPage(
  "Education Attainment and Smoking Habit",
  tabPanel("About", about_view),
  tabPanel("Page 1", interactive_view3),
  tabPanel("Page 2", interactive_view1),
  tabPanel("Page 3", interactive_view2) # order changed for convenience 
)

server <- function(input, output) {
  # VIEW 1
  
  output$alltime_stats <- renderUI({
    return(get_alltime_stats(input$state_view1))
  })
  
  output$scatter <- renderPlotly({
    p1 <-
      ggplot(
        df_all_time,
        aes(
          x = `All time average percent of smokers`,
          y = `All time average percent of bachelor's degree or higher`,
          col = State,
          color = State
        )
      ) +
      geom_point() +
      labs(x = "Percent of smokers", y = "Percent of bachelor's degree or higher", color = "States") +
      geom_text(
        data = filter(df_all_time, State == input$state_view1),
        aes(
          color = input$state_view1,
          label = input$state_view1
        ),
        nudge_y = 1
      )
    
    p1 <- ggplotly(p1, tooltip = "text")
    return(p1)
    
  })
  
  
  # VIEW 2
  
  output$alltime_stats_ecig <- renderUI({
    return(get_alltime_stats_ecig(input$state_view2))
  })
  
  output$scatter2 <- renderPlotly({
    p2 <-
      ggplot(df_all_time_ecig,
             aes(
               x = ecig,
               y = edulevel,
               col = State,
               color = State
             )) +
      geom_point() +
      labs(x = "Percent of E-cig user", y = "Percent of bachelor's degree or higher", color = "States") +
      geom_text(
        data = filter(df_all_time_ecig, State == input$state_view2),
        aes(
          color = input$state_view2,
          label = input$state_view2
        ),
        nudge_y = 1
      )
    
    p2 <- ggplotly(p2, tooltip = "text")
    return(p2)
    
  })
  
  
  # VIEW 3
  
  
  
  output$line <- renderPlotly({
    selected <- df_view3 %>% 
      filter(State == input$state_view3)
    
    if(input$edu_on && input$cig_on){
      
      p3 <- ggplot(selected,
                   aes(x = Year,)) +
        geom_line(aes(y = `Percent of bachelor's degree or higher`), color = "red") +
        geom_line(aes(y = `Percentage of cig user`), color = "blue") +
        annotate("text", x = selected$Year[5], y = selected$`Percent of bachelor's degree or higher`[5], label = "Percent of Bachelor's Degree or Higher", color = "red", vjust = -0.1) +
        annotate("text", x = selected$Year[5], y = selected$`Percentage of cig user`[5], label = "Percent of Cig User", color = "blue", vjust = -0.1) +
        labs(x = "Year", y = "Percentage") +
        scale_x_continuous(breaks = seq(min(selected$Year), max(selected$Year), by = 1))
      p3 <- ggplotly(p3, tooltip = "text")
      return(p3)
      
    }else if (input$edu_on){
      
      p3 <- ggplot(selected,
                   aes(x = Year,)) +
        geom_line(aes(y = `Percent of bachelor's degree or higher`), color = "red") +
        annotate("text", x = selected$Year[5], y = selected$`Percent of bachelor's degree or higher`[5], label = "Percent of Bachelor's Degree or Higher", color = "red", vjust = -0.1) +
        labs(x = "Year", y = "Percentage") +
        scale_x_continuous(breaks = seq(min(selected$Year), max(selected$Year), by = 1))
      p3 <- ggplotly(p3, tooltip = "text")
      return(p3)
      
    }else if (input$cig_on){
      
      p3 <- ggplot(selected,
                   aes(x = Year,)) +
        geom_line(aes(y = `Percentage of cig user`), color = "blue") +
        annotate("text", x = selected$Year[5], y = selected$`Percentage of cig user`[5], label = "Percent of Cig User", color = "blue", vjust = -0.1) +
        labs(x = "Year", y = "Percentage") +
        scale_x_continuous(breaks = seq(min(selected$Year), max(selected$Year), by = 1))
      p3 <- ggplotly(p3, tooltip = "text")
      return(p3)
      
    }else{
      return(NULL)
    }
      
  })
  
  
}

shinyApp(ui = ui, server = server)
