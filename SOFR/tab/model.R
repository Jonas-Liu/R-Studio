
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
                                  HTML("<h3>Five Simulated Path</h3>"),
                                  plotlyOutput(outputId = "simulate.plot", width = "800px"),
                                  HTML("<h3>Parameter Table</h3>"),
                                  tableOutput(outputId = "parameter1.table")
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
                                
                                
                                numericInput("model2.fore",
                                             label = "Forecast Period (Days)",
                                             min = 1,
                                             max = 20,
                                             value = 10
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
                                  HTML("<h3>Conditional Variance Analysis</h3>"),
                                  column(6,
                                         HTML("<h4>Model Parameters</h4>"),
                                         tableOutput(outputId = "par.tb")
                                  ),
                                  column(6,
                                         HTML("<h4>Variance Plot</h4>"),
                                         plotOutput(outputId = "estimate.plot")
                                  ),
                                  HTML("Here a GARCH(1,1) is applied to the conditional variance dynamics, and an optimized ARIMA model is applied to the conditional mean dynamics."),
                                  HTML("<h3>Forecast</h3>"),
                                  plotOutput(outputId = "forecast.plot")
                                )
                              )
                     ) # End of ARIMA-GARCH
                     
                     
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



# Simulation plot (page 1)
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


# Parameter table 1 (page 1)
output$parameter1.table <- renderTable({
  if(input$Model_button_1 > 0){
    isolate({
      param.table <- data.frame(t(param$value * 100))
      colnames(param.table) <- param$name
      print(param.table)
    })
  }
}, width = "200%", align = "c", digits = 4)






#################
#  ARIMA-GARCH  #
#################

# Fit the ARIMA model
ft.arima.fn <- function(x, ft.p = 4, ft.q = 4, ...){
  re.aic <- Inf
  re.par <- c(0,0,0)
  for (p in 1:ft.p) for (d in 0:1) for (q in 1:ft.q) {
    tmp.aic <- AIC(arima(x, order=c(p, d, q)))
    if (tmp.aic < re.aic) {
      re.aic <- tmp.aic
      re.par <- c(p, d, q)
      re.arima <- arima(x, order=re.par, ...)
    }
  }
  # Output the results and parameters
  list(re.arima, re.par, re.aic)
}



# Fit the GARCH model
ft.garch.fn <- function(x, ft.re, ...){
  if(!is.na(ft.re)[2]) arma.par <- ft.re[[2]][-2]
  ft.spec <- ugarchspec(mean.model = list(armaOrder = arma.par))
  
  # Model Estimation
  ugarchfit(spec = ft.spec, x, ...)
  
}


# Execute the model fitting

ft.model <- function(){
    # Period
    df.date <- as.Date(df[[1]])
    df.from <- which(abs(df.date-as.Date(input$model2.from)) == min(abs(df.date - as.Date(input$model2.from)),na.rm=TRUE))[1]
    df.to <- which(abs(df.date-as.Date(input$model2.to)) == min(abs(df.date - as.Date(input$model2.to)),na.rm=TRUE))[1]
    if(df.from < df.to){
      tmp <- df.from
      df.from <- df.to
      df.to <- tmp
    }
    df.idx <- seq(df.to,df.from,1)
    df.model <<- data.frame(date = as.Date(df[[1]][df.idx]),
                            rate = as.numeric(df[[3]][df.idx]))
    
    # log return
    ft.rate <- diff(log(df.model$rate[!is.na(df.model$rate)]))
    # Get the proper arima model
    ft.re <- ft.arima.fn(ft.rate)
    # Fit the GARCH Model
    ft.garch <- ft.garch.fn(ft.rate, ft.re)
    
    ft.garch
}

# Parameters table (page 2)
output$par.tb <- renderTable({
  if(input$Model_button_2 > 0){
    isolate({
      ft.garch <- ft.model()
      
      ft.coef <- as.data.frame(ft.garch@fit$matcoef)
      round(ft.coef, 4)
    })
  }
}, rownames = TRUE)

# Estimation plot (page 2)
output$estimate.plot <- renderPlot({
  if(input$Model_button_2 > 0){
    isolate({
      ft.garch <- ft.model()
      # Forecasting
      fore <- ugarchforecast(ft.garch, n.ahead = input$model2.fore)
      
      # Get the forecast variance
      plt.fvar <- data.frame(
        date = c(as.Date(tail(df.model$date, 20)), as.Date(tail(df.model$date, 1)) +  1:input$model2.fore),
        cvar = c(tail(ft.garch@fit$var, 20), rep(NA, input$model2.fore)),
        est = c(tail((ft.garch@fit$residuals)^2, 20), rep(NA, input$model2.fore)),
        fore = c(rep(NA, 20), (fore@forecast$sigmaFor)^2)
      )
      # Plot the forecast variance
      plt.fvar %>%
        gather(name, value, c(2:4)) %>%
        ggplot(aes(x = date, y = value, color = name)) +
        geom_line(size = 1) +
        scale_color_manual(labels = c('Conditional Variance', 'Squared Residuals', 'Forecast'),
                           values=c('gray','steelblue', '#9ecae1')) +
        theme(
          panel.background = element_rect(fill = "white", colour = NA),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "#deebf7"),
          legend.title = element_blank(),
          legend.position = "bottom",
          axis.title.x = element_blank()
        ) + ylab("Variance")
  })
  }
})

# Forecast Plot (page 2)
output$forecast.plot  <- renderPlot({
  if(input$Model_button_2 > 0){
    isolate({
      ft.garch <- ft.model()
      # Forecasting
      fore <- ugarchforecast(ft.garch, n.ahead = input$model2.fore)
      # Transformed back to the SOFR
      fore.rate <- exp(diffinv(fore@forecast$seriesFor[,1], xi = log(tail(df.rate, 1))))
      plt.frate <- data.frame(
        date = c(as.Date(tail(df.model$date, 20)), as.Date(tail(df.model$date, 1)) +  1:input$model2.fore),
        act = c(tail(df.model$rate,20), rep(NA,input$model2.fore)),
        fore = c(rep(NA,19), fore.rate)
      )
      
      # Plot the forecast rate
      plt.frate %>%
        gather(name, value, c(2:3)) %>%
        ggplot(aes(x = date, y = value, color = name)) +
        geom_line(size = 1) +
        scale_color_manual(labels = c('Actual', 'Forecast'),
                           values=c('steelblue', '#9ecae1'))+
        theme(
          panel.background = element_rect(fill = "white", colour = NA),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "#deebf7"),
          legend.title = element_blank(),
          legend.position = "bottom",
          axis.title.x = element_blank()
        ) + ylab("Rate(%)")
      
    })
    } # End of IF condition
}, height = 200)

