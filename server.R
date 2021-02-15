#Adapted from
#gdp_app - an example application using the Shiny library
#Eric Howard, University of Washington, Seattle, WA
#March 19, 2019

library(shiny)
#Load in the data
ps3 <- read.csv('PS3.csv', header=T)

#Transform yield data into percentages
ps3$HTC_YLDp <- ps3$HTC_YLD*100
ps3$HTC_IND_YLDp <- ps3$HTC_IND_YLD*100

#Assign dataframe to the global environment
assign("ps3", ps3, envir=globalenv())

shinyServer(function(input, output){
  #Region selector drop down menu contains the 4 regions
  output$region_selector = renderUI({
    selectInput(inputId="region",
                label="Region:",
                choices=as.character(unique(ps3$region)))
  })
  
  #Region selection determines which districts appear in the District selector
  #drop down menu.
  output$district_selector = renderUI({
    data_available1 = ps3[ps3$region == input$region, "district"] #Subsets the district based on region selection
    selectInput(inputId="district",
                label="District:",
                choices=unique(data_available1),
                selected=unique(data_available1)[1])
  })
  
  #District selection determines which facilities appear in the Facility selector
  #drop down menu.
  output$facility_selector = renderUI({
    data_available2 = ps3[ps3$district == input$district, "facility"]
    selectInput(inputId="facility",
                label="Facility:",
                choices=unique(data_available2),
                selected=unique(data_available2)[1])
  })
  
  #Display the name of the chosen facility on top of the Summary tab
  output$name1 <- renderText({
    input$facility
  })
  
  #Display the name of the chosen facility vs. other facilities on top of the Compare tab
  output$name2 <- renderText({
    paste(input$facility, "vs. Other Facilities")
  })
  
  #Plot the number of tests vs. number of positives OR yield for the chosen facility
  output$HTCplot <- renderPlot({
    highlight.facility <- input$facility #takes the value from the facility selectInput() element in the UI
    #Create a new plot
    plot.new()
    #Subset the PS3 data.frame for just the selected facility
    sub.ps3 <- subset(ps3, facility==highlight.facility)
    #Plot the number of tests
    plot(HTC~quarter, data=sub.ps3, type="l", lwd=2,
         bty="n", xaxt="n", yaxt="n", ylim=c(0,max(sub.ps3$HTC)), xlab="", ylab="")
    #Common x axis
    axis(side=1, xlab="Quarter", at=1:5, labels=c("2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4"))
    #Left axis
    axis(side=2, las=1)
    #Customize color and distance of left axis name
    mtext(side=2, line=2.5, "Number of HIV tests", font=2)
    
    #Plot either number of positives OR yield on top of the plot of number of tests
    if(input$raxis=="pos") { 
      #If one chooses to plot the number of positives
      par(new=TRUE)
      plot(HTC_POS~quarter, data=sub.ps3, col="red", type="l", lwd=2,
           bty="n", xaxt="n", yaxt="n", ylim=c(0,max(sub.ps3$HTC_POS)), xlab="", ylab="")
      #Right axis: change the color to match the color of the line
      axis(side=4, las=1, col="red", col.axis="red")
      #Customize color and distance of right axis name
      mtext(side=4, line=-2, "Number of positive HIV tests", font=2, col="red")
      #Add a title to the plot
      title(main="Number of HIV tests and positive test results", xlab="Quarter")
    }
    
    if(input$raxis=="yld") { 
      #If one chooses to plot yield
      par(new=TRUE)
      plot(HTC_YLDp~quarter, data=sub.ps3, col="coral", type="l", lwd=2,
           bty="n", xaxt="n", yaxt="n", ylim=c(0,100), xlab="", ylab="")
      #Right axis
      axis(side=4, las=1, col="coral", col.axis="coral")
      #Customize color and distance of right axis name
      mtext(side=4, line=-2, "Yield (%)", font=2, col="coral")
      #Add a title to the plot
      title(main="Number of HIV tests and yield", xlab="Quarter")
    }
  })
  
  #Plot the number of index tests vs. number of positives OR yield for the chosen facility
  output$INDplot <- renderPlot({
    highlight.facility <- input$facility
    plot.new()
    sub.ps3 <- subset(ps3, facility==highlight.facility)
    #Plot the number of index tests
    plot(HTC_IND~quarter, data=sub.ps3, type="l", lwd=2,
         bty="n", xaxt="n", yaxt="n", ylim=c(0,max(sub.ps3$HTC_IND)), xlab="", ylab="")
    #Common x axis
    axis(side=1, xlab="Quarter", at=1:5, labels=c("2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4"))
    #Left axis
    axis(side=2, las=1)
    #Customize color and distance of left axis name
    mtext(side=2, line=2.5, "Number of index tests", font=2)

    if(input$raxis=="pos") { 
      #If one chooses to plot number of positive results
      par(new=TRUE)
      plot(HTC_IND_POS~quarter, data=sub.ps3, col="red", type="l", lwd=2,
           bty="n", xaxt="n", yaxt="n", ylim=c(0,max(sub.ps3$HTC_IND_POS)), xlab="", ylab="")
      #Right axis
      axis(side=4, las=1, col="red", col.axis="red")
      #Customize color and distance of right axis name
      mtext(side=4, line=-2, "Number of positive index tests", font=2, col="red")
      #Add a title to the plot
      title(main="Number of index tests and positive index test results", xlab="Quarter")
    }
    
    if(input$raxis=="yld") { 
      #If one choose to plot yield
      par(new=TRUE)
      plot(HTC_IND_YLDp~quarter, data=sub.ps3, col="coral", type="l", lwd=2,
           bty="n", xaxt="n", yaxt="n", ylim=c(0,100), xlab="", ylab="")
      #Right axis
      axis(side=4, las=1, col="coral", col.axis="coral")
      #Customize color and distance of right axis name
      mtext(side=4, line=-2, "Yield (%)", font=2, col="coral")
      #Add a title to the plot
      title(main="Number of index tests and yield", xlab="Quarter")
    }
  })
  
  #Compare number of positives between chosen facility and others
  output$COMPAREplot1 <- renderPlot({ 
    #Slider scale
    max.HTC.POS <- input$scale1 #takes the value from the scale1 sliderInput() element in the UI
    min.HTC.POS <- 0
    unique.facility <- unique(ps3$facility)
    highlight.facility <- input$facility #takes the value from the facility selectInput() element in the UI
    #Create a new plot
    plot.new()
    plot.window(xlim =c(min(na.omit(ps3$quarter)), max(na.omit(ps3$quarter))), ylim=c(min.HTC.POS, max.HTC.POS))
    #Loop through and create line plots for each facility in grey
    for (i in 1:length(unique.facility)){
      fcy <- unique.facility[i]
      sub.ps3 <- subset(ps3, facility==fcy)
      lines(x=sub.ps3$quarter, y=sub.ps3$HTC_POS, col="grey", xaxt="n")
    }
    
    #Create and label the x and y axis
    axis(side=1, xlab="Quarter", at=1:5, labels=c("2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4"))
    axis(side=2)
    
    #Subset the PS3 data.frame for just the selected facility
    sub.ps3 <- subset(ps3, facility==highlight.facility)
    #Add a line, in red, for the select facility
    lines(x=sub.ps3$quarter, y=sub.ps3$HTC_POS, col="red", lwd=2)
    #Add a title to the plot
    title(main="Number of positive HIV test results", xlab="Quarter")
  })
  
  #Compare number of index positives between chosen facility and others
  output$COMPAREplot2 <- renderPlot({
    max.HTC.IND.POS <- input$scale2 #takes the value from the scale2 sliderInput() element in the UI
    min.HTC.IND.POS <- 0
    unique.facility <- unique(ps3$facility)
    highlight.facility <- input$facility #takes the value from the facility selectInput() element in the UI
    #Create a new plot
    plot.new()
    plot.window(xlim =c(min(na.omit(ps3$quarter)), max(na.omit(ps3$quarter))), ylim=c(min.HTC.IND.POS, max.HTC.IND.POS))
    #Loop through and create line plots for each facility in grey
    for (i in 1:length(unique.facility)){
      fcy <- unique.facility[i]
      sub.ps3 <- subset(ps3, facility==fcy)
      lines(x=sub.ps3$quarter, y=sub.ps3$HTC_IND_POS, col="lightgrey", xaxt="n")
    }
    
    #Create and label the x and y axis
    axis(side=1, xlab="Quarter", at=1:5, labels=c("2017Q4", "2018Q1", "2018Q2", "2018Q3", "2018Q4"))
    axis(side=2)
    
    #Subset the PS3 data.frame for just the selected facility
    sub.ps3 <- subset(ps3, facility==highlight.facility)
    #Add a line, in coral, for the select facility
    lines(x=sub.ps3$quarter, y=sub.ps3$HTC_IND_POS, col="coral", lwd=2)
    #Add a title to the plot
    title(main="Number of positive index test results", xlab="Quarter")
  })
})