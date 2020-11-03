tabItem_data <- 
  tabItem(tabName = "data",
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
          dataTableOutput("data.table"),
          
          br(),
          column(12, align = "right",
          downloadButton("data_download",  # Download button
                         "Download")
          )
          
          
  )# tabPanle: Data


# Data table
dataInput <- reactive({
  df.date <- as.Date(df[[1]])
  df.from <- which(abs(df.date-as.Date(input$dt.from)) == min(abs(df.date - as.Date(input$dt.from)),na.rm=TRUE))[1]
  df.to <- which(abs(df.date-as.Date(input$dt.to)) == min(abs(df.date - as.Date(input$dt.to)),na.rm=TRUE))[1]
  if(df.from > df.to){
    tmp <- df.from
    df.from <- df.to
    df.to <- tmp
  }
  df <- subset(df, select = -`BENCHMARK NAME`)
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
})


output$data.table <-
  renderDataTable(
    dataInput()
  )

output$data_download <-
  downloadHandler(
    filename = "SOFR.csv",
    content = function(file){
      write.csv(dataInput(), file, row.names = FALSE)
    }
  )