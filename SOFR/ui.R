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
file.name <- "SOFR.xls"
if(!file.exists(file.name) || (format(file.info(file.name)$mtime, '%m%d%Y') != URL.date)){
    download.file(url = URL.content, destfile = file.name, method = "curl")
}

df <- read_xls(path = "SOFR.xls", col_name = T, skip = 3)
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

