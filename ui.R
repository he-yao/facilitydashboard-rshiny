#Adapted from
#gdp_app - an example application using the Shiny library
#Eric Howard, University of Washington, Seattle, WA
#March 19, 2019

library(shiny)
#Read in data
ps3 <- read.csv('PS3.csv', header=T)

#Define UI for application
shinyUI(fluidPage(
  
  titlePanel("Facility Dashboard"),
  br(),
  #Introduction of this Shiny app
  div(
    div(
      HTML("Côte d’Ivoire (CI) remains one of the highest HIV-prevalence countries in West Africa 
           at an estimated 3.7% of the adult population. With funding from the US President’s 
           Emergency Plan for AIDS Relief (PEPFAR), Health Alliance International (HAI) has been 
           partnering with the CI Ministry of Health since 2009 to strengthen HIV/AIDS prevention, 
           care, and treatment service delivery. The program serves 237 health facilities in four 
           regions (17 districts) in the North and Northeast. Since only 37.2% of people living 
           with HIV in CI report knowing their status, one of the strategic foci of the program is 
           to help facilities provide more HIV tests and increase the yield of testing (percentage 
           of test results that are positive). Since index testing is a strategy that has been shown 
           to yield more positives than conventional facility-based testing services, we also focus 
           on index testing.")
    )
  ),
  br(),
  div(
    div(
      HTML("This <strong>Facility Dashboard</strong> is designed to help program staff, regional and 
           district health directors, and facility managers monitor the performance of HIV testing 
           service delivery at each of the 237 health facilities supported by HAI over the five 
           quarters from October 2017 to December 2018.
           <div></div>
           The <strong>Summary</strong> tab shows the number of all types of HIV tests performed, 
           the number of index tests performed, the number of positives, and the yield. You can switch 
           between the number of positives and yield as the value that you would like to compare to 
           the number of tests provided.
           <div></div>
           The <strong>Compare</strong> tab shows how the chosen facility compares with the other 236 
           facilities supported by the program in terms of the number of positive results and yield.")
    )
  ),
  br(),

  #Sidebar where one can choose the region, district, and health facility
  sidebarPanel(
    #Choose region first, district second,  then facility
    htmlOutput("region_selector"),
    htmlOutput("district_selector"),
    htmlOutput("facility_selector")
    ),
  
  mainPanel(
    #Use two tabs to present data. One for facility-specific summary, and the other for comparison
    #across facilities.
    tabsetPanel(type = "tabs",
                #Facility-specific tab
                tabPanel("Summary",
                         br(),
                         textOutput("name1"), #Specific facility name displayed according to one's selection
                         tags$head(tags$style("#name1{color: red; 
                                              font-size: 20px; 
                                              font-style: bold;}")), #Style of the facility name displayed
                         br(),
                         #Allow choice of right axis
                         radioButtons("raxis", "Compare the number of tests to (change the right axis to):",
                                      c("Number of positive test results" = "pos",
                                        "Yield (% results that are positive)" = "yld")),
                         plotOutput("HTCplot"), #Line plot of number of all types of HIV tests
                         plotOutput("INDplot")), #Line plot of number of index tests
                #Comparison tab
                tabPanel("Compare",
                         br(),
                         textOutput("name2"), #Comparison between specific facility and others based on one's selection
                         tags$head(tags$style("#name2{color: red; 
                                              font-size: 20px; 
                                              font-style: bold;}")),
                         br(),
                         #Allow adjustment of the y axis
                         sliderInput("scale1", "Rescale the y axis:",
                                     min = 0,
                                     max = max(ps3$HTC_POS),
                                     value = max(ps3$HTC_POS),
                                     step = 20),
                         #Line plot that displays number of positives at all facilities and highlights chosen facility
                         plotOutput("COMPAREplot1"),  
                         sliderInput("scale2", "Rescale the y axis:",
                                     min = 0,
                                     max = max(ps3$HTC_IND_POS),
                                     value = max(ps3$HTC_IND_POS),
                                     step = 10),
                         #Line plot that displays number of index positives at all facilities and highlights chosen facility
                         plotOutput("COMPAREplot2"))
    )
))
)