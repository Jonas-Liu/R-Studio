
tabItem_eda <- 
  tabItem(tabName = "eda",
          
          sidebarPanel(
            HTML("<h4>Input Parameters</h4>"),
            dateInput("input.from",
                      label = "From",
                      min = "2018-04-02",
                      max = Sys.Date(),
                      value = "2018-04-02"
            ),
            dateInput("input.to",
                      label = "To",
                      min = "2018-04-02",
                      max = Sys.Date(),
                      value = Sys.Date()
            ),
            selectInput("option.period",
                        label = "Periods",
                        choices = list("1 day" = 1, "5 days" = 5, "1 month" = 22, "6 months" = 6*22),
                        selected = "1 day"
            ),
            selectInput("option.scale",
                        label = "Scale Type",
                        choices = list("Linear Scale" = "linear", "Logarithmic Scale" = "log"),
                        selected = "Linear Scale"
            ),
            actionButton("Plot.button", 
                         "Submit", 
                         class = "btn btn-primary")
          ), # sidebarPanel
          mainPanel(
            column(7,
                   HTML("<h3>Histogram of SOFR</h4>"),
                   plotOutput(outputId = "test.plot")
            ),
            column(5,
                   HTML("<h3>Summary Table</h4>"),
                   tableOutput(outputId = "summary.table"),
                   br(),
                   HTML("<h3>Shapiro-Wilk Test</h4>"),
                   tableOutput(outputId = "test.table")
            ),
            hr(),
            HTML("<h3>Secured Overnight Financing Rate Chart</h4>"),
            plotlyOutput(outputId = "chart.plot", width = "1000px")
            
          ) # mainPanel
  )



#############################
#   Categorical Variables   #
#############################
# Plot Graph (page 1)
output$chart.plot <- renderPlotly({
  if(input$Plot.button > 0){
    isolate({
      # Period
      df.date <- as.Date(df[[1]])
      df.from <- which(abs(df.date-as.Date(input$input.from)) == min(abs(df.date - as.Date(input$input.from)),na.rm=TRUE))[1]
      df.to <- which(abs(df.date-as.Date(input$input.to)) == min(abs(df.date - as.Date(input$input.to)),na.rm=TRUE))[1]
      if(df.from < df.to){
        tmp <- df.from
        df.from <- df.to
        df.to <- tmp
      }
      df.idx <- seq(df.to,df.from,as.numeric(input$option.period))
      # Scaled
      if(input$option.scale == "linear"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = as.numeric(df[[3]][df.idx]))
      }else if(input$option.scale == "log"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = log(as.numeric(df[[3]][df.idx])))
      }
      # ggplot
      p <- ggplot(df.plot, aes(date, rate)) +
        geom_line(data = df.plot, col = "#9ecae1", size = 0.5) +
        xlab("Date") + ylab("Rate(%)") 
      p <- p + theme(
        panel.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(colour = "black"), 
        panel.grid.major = element_line(colour = "#deebf7")
        
      )
      p <- p + geom_point(colour = "#3182bd", size = 0.5)
      
      ggplotly(p, dynamicTicks = TRUE) %>% rangeslider(borderwidth = 1) 
    })
  }else{ # default option
    df.idx <- seq(1,df.nrow,1)
    df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                          rate = as.numeric(df[[3]][df.idx]))
    p <- ggplot(df.plot, aes(date, rate)) +
      geom_line(data = df.plot, col = "#9ecae1", size = 0.5) +
      xlab("Date") + ylab("Rate(%)") 
    p <- p + theme(
      panel.background = element_rect(fill = "white", colour = NA),
      axis.line = element_line(colour = "black"), 
      panel.grid.major = element_line(colour = "#deebf7")
      
    )
    p <- p + geom_point(colour = "#3182bd", size = 0.5)
    
    ggplotly(p, dynamicTicks = TRUE) %>% rangeslider(borderwidth = 1) 
  }
})

# Summary tabel (page 1)
output$summary.table <- renderTable({
  if(input$Plot.button > 0){
    isolate({
      # Period
      df.date <- as.Date(df[[1]])
      df.from <- which(abs(df.date-as.Date(input$input.from)) == min(abs(df.date - as.Date(input$input.from)),na.rm=TRUE))[1]
      df.to <- which(abs(df.date-as.Date(input$input.to)) == min(abs(df.date - as.Date(input$input.to)),na.rm=TRUE))[1]
      if(df.from > df.to){
        tmp <- df.from
        df.from <- df.to
        df.to <- tmp
      }
      df.idx <- seq(df.from,df.to,as.numeric(input$option.period))
      # Scaled
      if(input$option.scale == "linear"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = as.numeric(df[[3]][df.idx]))
      }else if(input$option.scale == "log"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = log(as.numeric(df[[3]][df.idx])))
      }
      res <- as.numeric(summary(df.plot[,2]))
      res <- as.data.frame(cbind(t(res), sd(df.plot[,2])))
      res <- cbind(c("Minimum", "1st Quartile", "Median",
                     "Mean", "3rd Quartile", "Maximum", "Standard Deviation"),
                   t(round(res, 2))
      )
      colnames(res) <- c("Statistics", "Value")
      print(res)
    })
  }else{
    df.idx <- seq(1,df.nrow,1)
    df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                          rate = as.numeric(df[[3]][df.idx]))
    res <- as.numeric(summary(df.plot[,2]))
    res <- as.data.frame(cbind(t(res), sd(df.plot[,2])))
    res <- cbind(c("Minimum", "1st Quartile", "Median",
                   "Mean", "3rd Quartile", "Maximum", "Standard Deviation"),
                 t(round(res, 2))
    )
    colnames(res) <- c("Statistics", "Value")
    print(res)
  }
})

output$test.plot <- renderPlot({
  if(input$Plot.button > 0){
    isolate({
      # Period
      df.date <- as.Date(df[[1]])
      df.from <- which(abs(df.date-as.Date(input$input.from)) == min(abs(df.date - as.Date(input$input.from)),na.rm=TRUE))[1]
      df.to <- which(abs(df.date-as.Date(input$input.to)) == min(abs(df.date - as.Date(input$input.to)),na.rm=TRUE))[1]
      if(df.from < df.to){
        tmp <- df.from
        df.from <- df.to
        df.to <- tmp
      }
      df.idx <- seq(df.to,df.from,as.numeric(input$option.period))
      # Scaled
      if(input$option.scale == "linear"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = as.numeric(df[[3]][df.idx]))
      }else if(input$option.scale == "log"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = log(as.numeric(df[[3]][df.idx])))
      }
      # ggplot
      
      ggplot(df.plot, aes(x=rate)) +
        geom_histogram(aes(y = ..density..), fill = "#deebf7", col = "#3182bd") +
        geom_density(color = "#3182bd", size = 1) +
        theme(
          panel.background = element_rect(fill = "white", colour = NA),
          axis.line = element_line(colour = "black"), 
          panel.grid.major = element_line(colour = "#deebf7")) +
        labs(x = "Rate(%)", y = "Frequency")
      
    })
  }else{ # default option
    df.idx <- seq(1,df.nrow,1)
    df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                          rate = as.numeric(df[[3]][df.idx])
    )
    
    ggplot(df.plot, aes(x=rate)) +
      geom_histogram(aes(y = ..density..), fill = "#deebf7", col = "#3182bd") +
      geom_density(color = "#3182bd", size = 1) +
      theme(
        panel.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(colour = "black"), 
        panel.grid.major = element_line(colour = "#deebf7")) +
      labs(x = "Rate(%)", y = "Frequency")
    
  }
  
  
}, width = 600)

output$test.table <- renderTable({
  
  if(input$Plot.button > 0){
    isolate({
      # Period
      df.date <- as.Date(df[[1]])
      df.from <- which(abs(df.date-as.Date(input$input.from)) == min(abs(df.date - as.Date(input$input.from)),na.rm=TRUE))[1]
      df.to <- which(abs(df.date-as.Date(input$input.to)) == min(abs(df.date - as.Date(input$input.to)),na.rm=TRUE))[1]
      if(df.from > df.to){
        tmp <- df.from
        df.from <- df.to
        df.to <- tmp
      }
      df.idx <- seq(df.from,df.to,as.numeric(input$option.period))
      # Scaled
      if(input$option.scale == "linear"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = as.numeric(df[[3]][df.idx]))
      }else if(input$option.scale == "log"){
        df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                              rate = log(as.numeric(df[[3]][df.idx])))
      }
      
      res <- shapiro.test(df.plot[,2])
      res <- as.data.frame(res[2:1])
      
      
      print(res)
    })
  }else{
    df.idx <- seq(1,df.nrow,1)
    df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                          rate = as.numeric(df[[3]][df.idx]))
    res <- shapiro.test(df.plot[,2])
    res <- as.data.frame(res[2:1])
    
    print(res)
  }
  
})