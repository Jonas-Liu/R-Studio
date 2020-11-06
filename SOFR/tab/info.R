tabItem_info <- 
  tabItem(tabName = "info",
          
            navbarPage("Info",
              
              tabPanel("Data Description",
                       
                       div(includeMarkdown("info/data.md"), 
                           align = "justify")
                       
              ), # End of Data Description
              
              tabPanel("Application Description",
                       
                       div(includeMarkdown("info/app.md"),
                           align = "justify"),
                       # Statistic summaries
                       tableOutput(outputId = "info.summary"),
                       
                       # Time plots
                       plotOutput(outputId = "info.plot")
              ) # End of App Description
            
          ) # End of navbar
  )



output$info.summary <- renderTable({
  df.idx <- seq(1,df.nrow,1)
  df.plot <<- data.frame(date = as.Date(df[[1]][df.idx]),
                        rate = as.numeric(df[[3]][df.idx]))
  res <- as.numeric(summary(df.plot[,2]))
  res <- as.data.frame(cbind(t(res), sd(df.plot[,2], na.rm = T)))
  colnames(res) <- c("Minimum", "1st Quartile", "Median",
                     "Mean", "3rd Quartile", "Maximum", "Standard Deviation")
  print(res)
})

output$info.plot <- renderPlot({
  ggplot(df.plot, aes(x = date, y = rate)) +
    geom_line(size = 1, col = "steelblue") +
    theme_light() +
    labs(title = "Time Series Graph", x = "Year", y = "Rate")
}, width = 800)