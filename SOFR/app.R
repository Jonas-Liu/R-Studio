######################################################
#          UI file for the shiny application         #
#                Author: Zheng Liu                   #
######################################################

# Set up packages
packages <- c("readxl", "ggplot2", "shiny", "shinythemes", "plotly")
for(package in packages){
    if(!require(package, character.only = T)) install.packages(package)
    library(package, character.only = T)
}

# Get URL
URL.base <- 'retrieve?multipleRateTypes=true&startdate=04022018&enddate='
URL.prefix <- 'https://websvcgatewayx2.frbny.org/mktrates_external_httponly/services/v1_0/mktRates/excel/'
URL.suffix <- '&rateType=R3'

URL.date <- Sys.Date()
URL.date <- format(URL.date, '%m%d%Y')
URL.content <- paste(URL.prefix, URL.base, URL.date, URL.suffix, sep = '')

# Download file
file.name <- "~/SOFR.xls"
if(!file.exists(file.name) || (format(file.info(file.name)$mtime, '%m%d%Y') != URL.date)){
    download.file(url = URL.content, destfile = file.name, method = "curl")
}

df <- read_xls(path = "~/SOFR.xls", col_name = T, skip = 3)
df.nrow <- dim(df)[1]
df <- df[c(df.nrow-7:df.nrow),]
df.rate <- as.numeric(df[[3]])
df.vol <- as.numeric(gsub(",", "", df[[8]]))

# Define UI
shinyUI(fluidPage(theme = shinytheme("yeti"),
                  navbarPage("SOFR",
                             tabPanel("Chart",
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
                                          HTML("<h3>Secured Overnight Financing Rate Chart</h4>"),
                                          plotlyOutput(outputId = "chart.plot"),
                                          HTML("<h3>Summary Table</h4>"),
                                          tableOutput(outputId = "summary.table")
                                      ) # mainPanel
                                      
                             ),# tabPanle: Plot
                             tabPanel("Interest Rate Model",
                                      sidebarPanel(
                                          HTML("<h4>Input Parameters</h4>"),
                                          dateInput("model.from",
                                                    label = "From",
                                                    min = "2018-04-02",
                                                    max = Sys.Date(),
                                                    value = "2018-04-02"
                                          ),
                                          dateInput("model.to",
                                                    label = "To",
                                                    min = "2018-04-02",
                                                    max = Sys.Date(),
                                                    value = Sys.Date()
                                          ),
                                          selectInput("model.period",
                                                      label = "Periods",
                                                      choices = list("1 day" = 1, "5 days" = 5, "1 month" = 22, "6 months" = 6*22),
                                                      selected = "1 day"
                                          ),
                                          selectInput("option.model",
                                                      label = "Model",
                                                      choices = list("Vasicek" = "v", "CIR" = "c", "Ho-Lee" = "hl"),
                                                      selected = "Vasicek"
                                          ),
                                          actionButton("Model.button", 
                                                       "Simulate", 
                                                       class = "btn btn-primary")
                                      ), # sidebarPanel
                                      mainPanel(
                                          HTML("<h3>Five Simulated Path</h4>"),
                                          plotlyOutput(outputId = "simulate.plot"),
                                          HTML("<h3>Parameter Table</h4>"),
                                          tableOutput(outputId = "parameter.table")
                                      ) # mainPanel
                                      
                             ),# tabPanle: Model
                             tabPanel("Data Table",
                                      # select Inputs
                                      fluidRow(
                                          column(4,
                                                 dateInput("dt.from",
                                                           label = "From",
                                                           min = "2018-04-02",
                                                           max = Sys.Date(),
                                                           value = "2018-04-02"
                                                 ),
                                                 dateInput("dt.to",
                                                           label = "To",
                                                           min = "2018-04-02",
                                                           max = Sys.Date(),
                                                           value = Sys.Date()
                                                 )
                                                 
                                          ),
                                          
                                          column(4,
                                                 selectInput("dt.rate.gl",
                                                             label = "Rate:",
                                                             choices = c("Greater Than","Less Than"),
                                                             selected = "Greater Than"
                                                 ),
                                                 sliderInput("dt.rate.nu",
                                                             label = "",
                                                             min = min(df.rate),
                                                             max = max(df.rate),
                                                             value = min(df.rate)
                                                 ),
                                          ),
                                          column(4,
                                                 selectInput("dt.vol.gl",
                                                             label = "Volume:",
                                                             choices = c("Greater Than","Less Than"),
                                                             selected = "Greater Than"
                                                 ),
                                                 sliderInput("dt.vol.nu", 
                                                             label = "",
                                                             min = min(df.vol),
                                                             max = max(df.vol),
                                                             value = min(df.vol)
                                                 ),
                                          )
                                      ),
                                      # data
                                      dataTableOutput("data.table")
                                      
                             )# tabPanle: Data
                  ) # navbarPage
)) # shinyUI(fluidPage)


# Simulation function
sim.model.fnc <- function(method,data,parameters,nsim=5){
    n <- length(data)
    data <- data/100
    fit <- matrix(NA,nrow=nsim,ncol=n)
    if(method == "v"){
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
        if(a > 1e-5){
            t <- seq(0,n-1,1)
            exp <- b+(data[1]-b)*exp(-a*t)
            std <- sqrt(sig^2/(2*a)*(1-exp(-2*a*t))) 
        }else{
            exp <- std <- rep(NA, n)
        }
    }else if(method == "c"){
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
        if(a > 1e-5){
            t <- seq(0,n-1,1)
            exp <-data[1]*exp(-a*t)+b*(1-exp(-a*t))
            std <- sqrt(data[1]*(sig^2/a)*(exp(-a*t)-exp(-2*a*t))+(b*sig^2/(2*a))*(1-exp(-a*t))^2)
        }else{
            exp <- std <- rep(NA, n)
        }
    }
    
    
    return(data.frame("data" = t(fit*100), "exp"=exp*100, "std"=std*100))
}



######################################################
#      Server file for the shiny application         #
#                Author: Zheng Liu                   #
######################################################


# Define server logic
shinyServer(function(input, output) {
    # Read data
    df <- read_xls(path = file.name, col_name = T, skip = 3)
    colnames(df) <- c("Date", "Name", "Rate(%)", "1st Percentile(%)", "25th Percentile(%)",
                      "75th Percentile(%)", "99th Percentile(%)", "Volume($Billions)")
    df.nrow <- dim(df)[1]
    df <- df[c(df.nrow-7:df.nrow),]
    df.nrow <- dim(df)[1]
    
    # Plot Graph
    output$chart.plot <- renderPlotly({
        if(input$Plot.button > 0){
            isolate({
                # Period
                df.date <- as.Date(df[[1]])
                df.from <- which(abs(df.date-as.Date(input$input.from)) == min(abs(df.date - as.Date(input$input.from)),na.rm=TRUE))
                df.to <- which(abs(df.date-as.Date(input$input.to)) == min(abs(df.date - as.Date(input$input.to)),na.rm=TRUE))
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
        }else{
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
    
    output$summary.table <- renderTable({
        if(input$Plot.button > 0){
            isolate({
                # Period
                df.date <- as.Date(df[[1]])
                df.from <- which(abs(df.date-as.Date(input$input.from)) == min(abs(df.date - as.Date(input$input.from)),na.rm=TRUE))
                df.to <- which(abs(df.date-as.Date(input$input.to)) == min(abs(df.date - as.Date(input$input.to)),na.rm=TRUE))
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
                colnames(res) <- c("Minimum", "1st Quartile", "Median",
                                   "Mean", "3rd Quartile", "Maximum", "Standard Deviation")
                print(res)
            })
        }else{
            df.idx <- seq(1,df.nrow,1)
            df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                                  rate = as.numeric(df[[3]][df.idx]))
            res <- as.numeric(summary(df.plot[,2]))
            res <- as.data.frame(cbind(t(res), sd(df.plot[,2])))
            colnames(res) <- c("Minimum", "1st Quartile", "Median",
                               "Mean", "3rd Quartile", "Maximum", "Standard Deviation")
            print(res)
        }
    })
    
    
    output$simulate.plot <- renderPlotly({
        if(input$Model.button > 0){
            isolate({
                # Period
                df.date <- as.Date(df[[1]])
                df.from <- which(abs(df.date-as.Date(input$model.from)) == min(abs(df.date - as.Date(input$model.from)),na.rm=TRUE))
                df.to <- which(abs(df.date-as.Date(input$model.to)) == min(abs(df.date - as.Date(input$model.to)),na.rm=TRUE))
                if(df.from < df.to){
                    tmp <- df.from
                    df.from <- df.to
                    df.to <- tmp
                }
                df.idx <- seq(df.to,df.from,as.numeric(input$model.period))
                # Scaled
                if(input$option.model == "v"){
                    df.model <- data.frame(date = as.Date(df[[1]][df.idx]),
                                           rate = as.numeric(df[[3]][df.idx]))
                }else if(input$option.model == "c"){
                    df.model <- data.frame(date = as.Date(df[[1]][df.idx]),
                                           rate = as.numeric(df[[3]][df.idx]))
                }
                
                # fit the model
                
                param <- c(0.001,  0.0162, 0.0019)#fit.model.fnc()
                
                
                model.res <- sim.model.fnc(input$option.model,df.model$rate,param)
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
        }else{
            df.idx <- seq(1,df.nrow,1)
            df.model <- data.frame(date = as.Date(df[[1]][df.idx]),
                                   rate = as.numeric(df[[3]][df.idx]))
            p <- ggplot(df.model, aes(date, rate)) +
                geom_line(data = df.model, col = "#9ecae1", size = 0.5) +
                xlab("Date") + ylab("Rate(%)") 
            p <- p + theme(
                panel.background = element_rect(fill = "white", colour = NA),
                axis.line = element_line(colour = "black"), 
                panel.grid.major = element_line(colour = "#deebf7")
                
            )
            p <- p + geom_point(colour = "#3182bd", size = 0.5)
            
            ggplotly(p) 
            
        }
    })
    
    
    output$data.table <- renderDataTable(datatable({
        df.date <- as.Date(df[[1]])
        df.from <- which(abs(df.date-as.Date(input$dt.from)) == min(abs(df.date - as.Date(input$dt.from)),na.rm=TRUE))
        df.to <- which(abs(df.date-as.Date(input$dt.to)) == min(abs(df.date - as.Date(input$dt.to)),na.rm=TRUE))
        if(df.from > df.to){
            tmp <- df.from
            df.from <- df.to
            df.to <- tmp
        }
        df <- subset(df, select = -Name)
        df <- df[c(df.from:df.to),]
        
        if(input$dt.rate.gl == "Greater Than"){
            df <- df[which(df[[2]] > as.numeric(input$dt.rate.nu)),]
        }else{
            df <- df[which(df[[2]] < as.numeric(input$dt.rate.nu)),]
        }
        
        if(input$dt.vol.gl == "Greater Than"){
            df <- df[which(as.numeric(gsub(",", "", df[[7]])) > as.numeric(input$dt.vol.nu)),]
        }else{
            df <- df[which(as.numeric(gsub(",", "", df[[7]])) < as.numeric(input$dt.vol.nu)),]
        }
        df
    }))
    
    
})



# Run the application 
shinyApp(ui = ui, server = server)
