
tabItem_model <- 
  tabItem(tabName = "model",
          
          navbarPage("Modeling",
                     tabPanel("Vasicek Model",
                              sidebarPanel(
                                HTML("<h4>Input Parameters</h4>"),
                                dateInput("model1.from",
                                          label = "From",
                                          min = "2018-04-02",
                                          max = Sys.Date(),
                                          value = "2018-04-02"
                                ),
                                dateInput("model1.to",
                                          label = "To",
                                          min = "2018-04-02",
                                          max = Sys.Date(),
                                          value = Sys.Date()
                                ),
                                selectInput("model1.period",
                                            label = "Periods",
                                            choices = list("1 day" = 1, "5 days" = 5, "1 month" = 22, "6 months" = 6*22),
                                            selected = "1 day"
                                ),
                                actionButton("Model_button_1", 
                                             "Submit", 
                                             class = "btn btn-primary")
                              ), # sidebarPanel
                              
                              mainPanel(
                                withMathJax(),
                                div(includeMarkdown("info/vasicek.md"),
                                    align = "justify"),
                                br(),
                                hr(),
                                conditionalPanel(
                                  condition = "input.Model_button_1 == 0",
                                  h4("Ready for analysis, please submit parameters.",
                                     style = "color:gray;")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.Model_button_1 > 0",
                                  HTML("<h3>Five Simulated Path</h4>"),
                                  plotlyOutput(outputId = "simulate.plot", width = "1000px"),
                                  HTML("<h3>Parameter Table</h4>"),
                                  tableOutput(outputId = "parameter.table")
                                )
                              )
                              
                     ), # End of Vasicek Model
                     
                     
                     
                     tabPanel("GARCH Model",
                              sidebarPanel(
                                HTML("<h4>Input Parameters</h4>"),
                                
                                dateInput("model2.from",
                                          label = "From",
                                          min = "2018-04-02",
                                          max = Sys.Date(),
                                          value = "2018-04-02"
                                ),
                                dateInput("model2.to",
                                          label = "To",
                                          min = "2018-04-02",
                                          max = Sys.Date(),
                                          value = Sys.Date()
                                ),
                                
                                numericInput("model2.type",
                                            label = "AR Term",
                                            value = 0,
                                            min = 0,
                                            max = 24,
                                            step = 1
                                            ),
                                
                                numericInput("model2.type",
                                             label = "MA Term",
                                             value = 0,
                                             min = 0,
                                             max = 24,
                                             step = 1
                                ),
                                
                                actionButton("Model_button_2", 
                                             "Submit", 
                                             class = "btn btn-primary")
                              ), # sidebarPanel
                              
                              mainPanel(
                                withMathJax(),
                                div(includeMarkdown("info/garch.md"),
                                    align = "justify"),
                                br(),
                                hr(),
                                conditionalPanel(
                                  condition = "input.Model_button_2 == 0",
                                  h4("Ready for analysis, please submit parameters.",
                                     style = "color:gray;")
                                ),
                                
                                conditionalPanel(
                                  condition = "input.Model_button_2 > 0",
                                  HTML("<h3>To be continued....</h4>"),
                                  
                                )
                              )
                     ) # End of ARIMA
                     
                     
          ) # End of navbar
  )




#############
#  Vasicek  #
#############

# Fit model function
fit.model.fnc <- function(data){
  # Reframe the data
  data <- na.omit(data)/100
  
  p.name <- c("speed of reversion (a)", "long-term mean level (b)", "volatility (sigma)")
  
  n <- length(data)
  
  # Least squares regression
  Sx <- sum(data[-n])
  Sy <- sum(data[-1])
  Sxx <- sum(data[-n]^2)
  Syy <- sum(data[-1]^2)
  Sxy <- sum(data[-1]*data[-n])
  
  mu <- (n*Sxy-Sx*Sy)/(n*Sxx-Sx^2)
  lambda <- (Sy-mu*Sx)/n
  sd <- sqrt((n*Syy-Sy^2-mu*(n*Sxy-Sx*Sy))/(n*(n-2)))
  
  
  a <- -log(mu)
  
  b <- lambda/(1-mu)
  sig <- sd*sqrt(-2*log(mu)/(1-mu^2))
  
  
  p.value <- c(a, b, sig)
  
  res <- data.frame(value = p.value, name = p.name)
  return(res)
}

# Simulation function
sim.model.fnc <- function(data,parameters,nsim=5){
  n <- length(data)
  data <- data/100
  fit <- matrix(NA,nrow=nsim,ncol=n)
  a <- parameters[1]
  b <- parameters[2]
  sig <- parameters[3]
  
  for (sim in 1:nsim){
    fit[sim,1] <- data[1]
    for (i in 2:n){
      add <- fit[sim,i-1]*exp(-a)+b*(1-exp(-a))+sig*sqrt((1-exp(-2*a))/(2*a))*rnorm(1)
      fit[sim,i] <- add
    }
  }
  if(a != 0){
    t <- seq(0,n-1,1)
    exp <- b+(data[1]-b)*exp(-a*t)
    std <- sqrt(sig^2/(2*a)*(1-exp(-2*a*t))) 
  }else{
    exp <- std <- rep(NA, n)
  }
  return(data.frame("data" = t(fit*100), "exp"=exp*100, "std"=std*100))
}



# Simulation plot (page 2)
output$simulate.plot <- renderPlotly({
  if(input$Model_button_1 > 0){
    isolate({
      # Period
      df.date <- as.Date(df[[1]])
      df.from <- which(abs(df.date-as.Date(input$model1.from)) == min(abs(df.date - as.Date(input$model1.from)),na.rm=TRUE))[1]
      df.to <- which(abs(df.date-as.Date(input$model1.to)) == min(abs(df.date - as.Date(input$model1.to)),na.rm=TRUE))[1]
      if(df.from < df.to){
        tmp <- df.from
        df.from <- df.to
        df.to <- tmp
      }
      df.idx <- seq(df.to,df.from,as.numeric(input$model1.period))
      df.model <- data.frame(date = as.Date(df[[1]][df.idx]),
                             rate = as.numeric(df[[3]][df.idx]))
      
      
      # get the model parameter
      param <<- fit.model.fnc(df.model$rate)
      
      
      # fit the model
      model.res <- sim.model.fnc(df.model$rate, param$value)
      exp <- model.res$exp
      std <- model.res$std
      simulate <- model.res[,1:5]
      
      
      # ggplot
      p <- ggplot(df.model, aes(date, rate)) + xlab("Date") + ylab("Rate(%)") 
      
      for(i in 1:5){
        p <- p + geom_line(data = data.frame(date = df.model$date,rate = simulate[,i]), col = "#deebf7", size = 0.5)
      }
      
      p <- p + geom_line(data = data.frame(date = df.model$date,rate = exp), col = "black", linetype = "dashed", size = 0.5) + 
        geom_line(data = data.frame(date = df.model$date,rate = exp+2*std), col = "black", linetype = "dashed", size = 0.5) + 
        geom_line(data = data.frame(date = df.model$date,rate = exp-2*std), col = "black", linetype = "dashed", size = 0.5)
      
      p <- p + geom_line(data = df.model, col = "#9ecae1", size = 0.5)
      
      p <- p + theme(
        panel.background = element_rect(fill = "white", colour = NA),
        axis.line = element_line(colour = "black"), 
        panel.grid.major = element_line(colour = "#deebf7")
        
      )
      p <- p + geom_point(colour = "#3182bd", size = 0.5)
      
      ggplotly(p)
    })
  }
})


# Parameter table (page 2)
output$parameter.table <- renderTable({
  if(input$Model_button_1 > 0){
    isolate({
      param.table <- data.frame(t(param$value * 100))
      colnames(param.table) <- param$name
      print(param.table)
    })
  }
}, width = "200%", align = "c", digits = 4)


