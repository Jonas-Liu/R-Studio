

####################################
author <- "Zheng Liu"              #
email <- "zliu64@ncsu.edu"         #
####################################



####################################
#        Read in raw data          #
####################################


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
df.nrow <- dim(df)[1]

df.plot <- data.frame(date = as.Date(df[[1]][df.idx]),
                      rate = as.numeric(df[[3]][df.idx]))

####################################
#            Shiny Server          #
####################################

shinyServer(function(input, output, session, options = options(warn = -1)) {
    
    output$author <- renderUI({
        fluidRow(
            column(12,
                   offset = 1,
                   br(),
                   h5(paste("Author:", author), 
                      style = "color:white;"
                   ),
                   h5(tagList("E-mail:", a(email, href = paste0("mailto:", email))),
                      style = "color:white;"
                   )
            )
        )
    })
    
    output$ui_sidebar <- renderUI({
        sidebarMenu(id = "tab",
                    
                    menuItem("Information", 
                             tabName = "info", 
                             icon = icon("file-alt")
                    ),
                    menuItem("Exploratory Data Analysis", 
                             tabName = "eda", 
                             icon = icon("chart-bar")
                    ),
                    menuItem("Modeling", 
                             tabName = "model", 
                             icon = icon("code-branch")
                    ),
                    menuItem("Data", 
                             tabName = "data", 
                             icon = icon("database")
                    ),
                    
                    uiOutput("author")
        )
    })
    
    output$ui_body <- renderUI({
        updateTabsetPanel(session, "tab", selected = "info")
        tabItems(
            tabItem_info,
            # tabItem_eda,
            # tabItem_model,
            tabItem_data
        )
    })


    # Modules
    source('tab/info.R', local = TRUE)
    # source('tab/eda.R', local = TRUE)
    # source('tab/model.R', local = TRUE)
    source('tab/data.R', local = TRUE)
    
})  