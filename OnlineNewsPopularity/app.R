######################################################
#         UIC: Online News Popularity Data Set       #
#                Author: Zheng Liu                   #
######################################################


library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(tidyverse)


# Get the data link
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip"

# Download file
name <- "OnlineNewsPopularity"
zip.path <- paste0("~/", name, ".zip")
if(!file.exists(file.name)){
    download.file(url, destfile = zip.path, method = "curl")
}
unzip(zip.path)
# file.remove(zip.path)

# Read in Data
csv.path <- paste0("~/", name, "/", name, ".csv")
all.data <- read_csv(csv.path)


######################################################
#          UI file for the shiny application         #
#                Author: Zheng Liu                   #
######################################################

ui <- fluidPage( theme = shinytheme("yeti"),
                 # Application title
                 sidebarPanel(selectInput("weekday",
                                          label = div(style = "font-size:20px", "Weekday"),
                                          choices = list("Monday" = 1,
                                                         "Tuesday" = 2,
                                                         "Wednesday" = 3,
                                                         "Thursday" = 4,
                                                         "Friday" = 5,
                                                         "Saturday" = 6,
                                                         "Sunday" = 7)),
                              actionButton("go", 
                                           "Submit", 
                                           class = "btn btn-primary")),
                 # Show a plot of the generated distribution
                 HTML("<h2>UCI: Online News Popularity"),
                 mainPanel(
                     h2(textOutput("weekday")),
                     plotOutput("plot.1"),
                     tableOutput("table.1")
                     
                 )
)



######################################################
#      Server file for the shiny application         #
#                Author: Zheng Liu                   #
######################################################


server <- function(input, output) {
    weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    weekdayis <- paste0("weekday_is_", tolower(weekdays))
    
    hist.fun <- function(data){
        ggplot(data, aes(y = shares)) + geom_boxplot()
    }
    
    summary.fun <- function(data){
        res <- summary(data$shares)
        res <- data.frame(t(res))[-1]
        colnames(res) <- c("Statistics", "Value")
        res
    }
    
    observeEvent(input$go, {
        output$weekday <- renderText(paste("Report for", weekdays[[as.numeric(input$weekday)]]))
        
        weekday.data <- all.data %>%
            select(starts_with("weekday_is_"), shares) %>%
            filter(.data[[weekdayis[[as.numeric(input$weekday)]]]] == 1)
        
        output$plot.1 <- renderPlot({
            isolate(hist.fun(weekday.data))
        })
        
        output$table.1 <- renderTable({
            isolate(summary.fun(weekday.data))
        })
        
    })
    
    
}




# Run the application 
shinyApp(ui = ui, server = server)
