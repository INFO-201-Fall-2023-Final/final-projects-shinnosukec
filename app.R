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
      "This scatterplot is a compelling graphical representation of the relationship between the average percentage of individuals with bachelor’s degrees or higher and the average percentage of smokers in each state of the United States. The data is averaged over a period from 2011 to 2019, providing a broad and comprehensive view of this relationship across the nation.<br><br>
In the scatterplot, each data point is distinctively colored, representing one of the U.S. states or territories. The vertical axis of the plot quantifies the percentage of the population with bachelor's degrees or higher, while the horizontal axis measures the percentage of smokers within the state's population. The positioning of each data point thus reflects the unique combination of these two variables for each state.<br><br>
A trend is evident upon examining the scatterplot: there is a consistent negative correlation between the level of higher education and smoking rates. This trend is observed as a pattern where states with higher percentages of bachelor's degree holders tend to have lower percentages of smokers, and vice versa. The data points largely follow this downward trajectory, illustrating the inverse relationship between educational attainment and smoking prevalence.<br><br>
However, the correlation coefficient calculated using the cor() function stands at
", cor_avg, "which, at first glance, seems surprisingly low given the apparent trend. This discrepancy likely arises due to the presence of outliers that impact the overall correlation. Two notable examples stand out:<br><br>
1. District of Columbia: This area shows a particularly high level of education, with an exceptional percentage of residents holding bachelor's degrees or higher, especially when compared to other states with a similar proportion of smokers. D.C. is an outlier on the higher education axis, skewing the data.<br><br>
2. Puerto Rico: On the opposite end, Puerto Rico presents a low percentage of smokers, but its level of higher education attainment is not correspondingly high. This makes Puerto Rico an outlier in terms of smoking rates relative to education levels.<br><br>
These outliers, particularly prominent in their deviation from the general trend, play a significant role in affecting the calculated correlation coefficient. Without these outliers, the negative correlation might appear stronger.<br><br>
In summary, this scatterplot not only displays a general negative correlation between higher education levels and smoking rates across the U.S. states but also highlights the impact of outliers on statistical correlations. It serves as a thought-provoking visual analysis tool, showcasing the nuances in the relationship between education and health-related behaviors.<br><br>
"
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
    paste("This scatterplot offers an insightful visual representation of the relationship between e-cigarette usage and the average percentage of individuals holding bachelor’s degrees or higher in each state of the United States. The data is specifically drawn from the years 2016 and 2017 due to the availability of e-cigarette usage data, a period during which e-cigarettes gained significant popularity.<br><br>
In the plot, each state is represented by a unique data point. As for the previous scatterplot, the vertical axis measures the percentage of the population with at least a bachelor’s degree, while the horizontal axis quantifies the percentage of e-cigarette users within each state. This setup allows for a direct comparison between educational attainment and the prevalence of e-cigarette usage across different states.<br><br>
A notable trend can be observed in the scatterplot, though it appears less pronounced than in the traditional cigarette usage scatterplot. As the percentage of e-cigarette users increases in each state, there tends to be a decrease in the percentage of individuals with higher educational degrees. However, this negative correlation is subtler and the data points are more spread apart, making the declining trend line less apparent. This spread suggests a weaker association between education levels and e-cigarette usage compared to traditional cigarettes.<br><br>
Despite the weaker trend, the scatterplot still indicates that higher education levels are generally associated with lower rates of e-cigarette usage, even though to a lesser extent than with traditional cigarette usage. This observation hints at some distinct characteristics in the consumption patterns of e-cigarettes as opposed to traditional tobacco products.<br><br>
The correlation coefficient calculated using the cor() function is",
      cor_avg_ecig,
      "which is a relatively low value. Again, it can be attributed to outliers such as the District of Columbia and Puerto Rico. The District of Columbia stands out with high educational attainment but moderate e-cigarette usage, while Puerto Rico shows low e-cigarette usage despite lower levels of higher education.<br><br>
In conclusion, this scatterplot provides a nuanced view of the relationship between education levels and e-cigarette usage in the United States. While it confirms a negative correlation similar to that observed with traditional cigarettes, the connection is less pronounced, reflecting the unique position of e-cigarettes in American society. Despite claims by media and manufacturers about the reduced harm of e-cigarettes compared to traditional cigarettes, the data suggests that societal attitudes towards the health impacts of smoking, whether traditional or electronic, are influenced by educational background, although in complex and varied ways.<br><br>
"
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
  p(HTML(paste("The line graph illustrates two trends over a nine-year period, from 2011 to 2019, across the United States. The first trend line in red represents the percentage of the population with bachelor’s degrees or higher, showcasing a clear and consistent upward trajectory. This increase is gradual yet steady, suggesting a significant rise in higher education attainment among Americans during this period.<br><br>

In contrast, the second trend line in blue on the graph represents the percentage of cigarette users in the same timeframe, which demonstrates a converse, downward trend. Starting from a higher percentage in 2011, there is a noticeable decline in cigarette usage year after year, culminating in a significantly lower percentage by 2019. This decrease is relatively smooth and continuous, indicating a substantial shift in health habits among the populace.<br><br>

The graph not only presents these trends on a national average but also provides state-by-state breakdowns (please select an individual state from the dropdown menu). Interestingly, the patterns remain consistent in most states, with a few exceptions that highlight regional differences. These variations are subtly indicated on the graph, providing a nuanced understanding of the trends across different geographical areas.<br><br>

The colors chosen for the graph are distinct, with the education trend represented in blue and the smoking trend in red, enhancing readability and comprehension. The axes are clearly labeled, with the horizontal axis representing the years from 2011 to 2019 and the vertical axis denoting the percentage values for both education and cigarette usage.<br><br>

The graph suggests that higher education may play a pivotal role in promoting healthier lifestyle choices. It acts as a compelling piece of evidence to support the notion that improvements in education correlate with positive shifts in public health behaviors.

Overall, this line graph is not only a depiction of statistical data but also a narrative of societal progress in education and health awareness within the United States over nearly a decade. It serves as a testament to the evolving attitudes and behaviors of Americans, highlighting the beneficial impact of education on lifestyle choices and health.<br><br>
")))
  
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