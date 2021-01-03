library(shiny)
library(png)
library(gridExtra)
library(grid)
library(shinyWidgets)

UI.color <- "A8CAB0"
Score.color <- "ffaf8f"

path <- "D:/Repository/R-Studio/Texas_Holdem"
setwd(path)

# Building a deck
card <- function(shuffle=T){
    
    # Create a new deck
    cards <- NULL
    for (i in c("clubs", "hearts", "spades", "diamonds")){
        for (j in 1:13){
            cards <- append(cards,paste(i,j))
        }
    }
    
    # Shuffle the cards
    if(shuffle) sample(cards)
    else cards
}


# Plot the card
i.card <- function(card, type, is.board = TRUE){
    
    # Find the icon path
    i.path <- paste(path, "/icon/", sep = "")
    
    # Create a tmp png file to store the pic
    outfile <- paste(i.path, "tmp.png", sep = "")
    png(outfile, width = 200, height = 300, res = 800)
    
    if(is.na(card)){
        type <- "N"
        
    }else{
        if(type == "B"){
            
            suit <- strsplit(card, " ")[[1]][1]
            number <- strsplit(card, " ")[[1]][2]
            
            i.number <- paste(i.path, number, ".png", sep = "")
            i.suit <- paste(i.path, suit, ".png", sep = "")
            i.na <- paste(i.path, "NA.png", sep = "")
            
            lay <- rbind(
                c(1,1,NA,NA,NA,NA,NA),
                c(1,1,NA,NA,NA,NA,NA),
                c(NA,NA,2,2,2,NA,NA),
                c(NA,NA,2,2,2,NA,NA),
                c(NA,NA,2,2,2,NA,NA),
                c(NA,NA,NA,NA,NA,3,3),
                c(NA,NA,NA,NA,NA,3,3))
            i.png <- lapply(list(i.number, i.suit, i.number), readPNG)
            
        }else{ # Type == "U"
            i.unknown <- paste(i.path, "unknown.png", sep = "")
            
            lay <- rbind(
                c(1,1),
                c(1,1)
            )
            
            i.png <- lapply(list(i.unknown), readPNG)
        }
        
        # Crate the picture
        asGrobs <- lapply(i.png, rasterGrob)
        grid.arrange(grobs=asGrobs, layout_matrix = lay)
        grid.rect(width = .99, height = .99, gp = gpar(lwd = 0.75, col = "black", fill = NA))
        dev.off()
        
        if(is.board){ # Board card
            list(src = outfile,
                 contentType = 'image/png',
                 width = 200,
                 height = 300
            )
        }else{
            list(src = outfile,
                 contentType = 'image/png',
                 width = 150,
                 height = 225
            )
        } # End of board condition
        
    } # End of na condition
} # End of i.card


# Reset function
reset <- function(){
    turn <<- turn+1
    
    deck  <<- card()
    board <<- hand.l <<- hand.r <<- c()
    bet <<- bet.l <<- bet.r <<- 0
    
    # Draw Cards
    hand.l <<- deck[c(1,3)]
    hand.r <<- deck[c(2,4)]
    board  <<- deck[c(3:7)*2]
}


ui <- fluidPage(
    
    # Start Page
    conditionalPanel(condition = "input.start == 0",
                     column(
                         12, align = "center",
                         numericInput(
                             't.value',
                             h2("Max chips"),
                             value = 200,
                             min = 20,
                             max = 500,
                             step = 20
                         ),
                         actionButton("start", NULL,  
                                      style = "width: 600px; height: 900px;
background: url('https://upswingpoker.com/wp-content/uploads/2018/11/Hand-Rankings-Upswing-Poker.jpg');  background-size: cover; background-position: center;") 
                     )
                     
    ), # Conditional start
    
    conditionalPanel(condition = "input.start > 0",
                     
                     
                     # Board Cards
                     column(
                         12, align = "center",
                         column(1),
                         column(
                             2, align = "center",
                             imageOutput("board1")
                         ),
                         column(
                             2, align = "center",
                             imageOutput("board2")
                         ),
                         column(
                             2, align = "center",
                             imageOutput("board3")
                         ),
                         column(
                             2, align = "center",
                             imageOutput("board4")
                         ),
                         column(
                             2, align = "center",
                             imageOutput("board5")
                         ),
                         column(1),
                         style = paste0('margin-bottom:5px;border:5px solid; padding: 20px; background-color:#',UI.color,';')
                     ),
                     
                     # Players' panel
                     # Left Hand
                     column(
                         3, align = "left", offset = 0,
                         column(
                             6,align = "left",
                             actionButton("show.l",
                                          imageOutput("hand.l1"),
                                          style = paste0('width: 180px; height: 255px; border-color: #',UI.color,'; background-color:#',UI.color,';')
                             )
                         ),
                         column(
                             6,align = "left",
                             actionButton("hide.l",
                                          imageOutput("hand.l2"),
                                          style = paste0('width: 180px; height: 255px; border-color: #',UI.color,'; background-color:#',UI.color,';')
                             )
                         ),
                         style = paste0('margin-bottom:5px;border:5px solid; padding: 20px; background-color:#',UI.color,';')
                     ), # End of Left Hand (3)
                     
                     # Left Panel
                     column(
                         1, 
                         actionButton(
                             "bet.l",
                             h1("Bet"),
                             style = paste0('margin-bottom:5px;border:5px solid; padding: 50px; background-color:#',Score.color,';')
                         ),
                         
                         uiOutput("ui.l")
                         
                     ), # End of Left Panel (1)
                     
                     # Score Panel
                     column(
                         4, align = "center", 
                         column(
                             3, align = "left",
                             h1(textOutput("t.score.l"))
                         ),
                         column(
                             2, align = "center",
                             h2(textOutput("b.score.l"))
                         ),
                         column(
                             2, align = "center",
                             h1(textOutput("t.score"))
                         ),
                         column(
                             2, align = "center",
                             h2(textOutput("b.score.r"))
                         ),
                         column(
                             3, align = "right",
                             h1(textOutput("t.score.r"))
                         ),
                         column(
                             4, align = "center",
                             uiOutput('winner.l')
                         ),
                         column(
                             4, align = "center",
                             uiOutput('winner.d'),
                             
                             actionButton('more', h2("DONE"),
                                          style = paste0('border:3px solid; background-color:#',Score.color,';'))
                             
                         ),
                         column(
                             4, align = "center",
                             uiOutput('winner.r')
                         ),
                         
                         style = paste0('margin-bottom:5px;border:5px solid; padding: 20px; background-color:#',Score.color,';')
                     ), # End of Score Panel (4)
                     
                     
                     
                     # Right Panel
                     column(
                         1, 
                         actionButton(
                             "bet.r",
                             h1("Bet"),
                             style = paste0('margin-bottom:5px;border:5px solid; padding: 50px; background-color:#',Score.color,';')
                         ),
                         
                         uiOutput("ui.r")
                         
                     ), # End of Right Panel (1)
                     
                     # Right Hand
                     column(
                         3, align = "right", offset = 0,
                         column(
                             6,align = "right",
                             actionButton("show.r",
                                          imageOutput("hand.r1"),
                                          style = paste0('width: 180px; height: 255px; border-color: #',UI.color,'; background-color:#',UI.color,';')
                             )
                         ),
                         column(
                             6,align = "right",
                             actionButton("hide.r",
                                          imageOutput("hand.r2"),
                                          style = paste0('width: 180px; height: 255px; border-color: #',UI.color,'; background-color:#',UI.color,';')
                             )
                         ),
                         style = paste0('margin-bottom:5px;border:5px solid; padding: 20px; background-color:#',UI.color,';')
                     ) # End of Right Hand (3)
    )# End of Start Condition
) # End of UI


server <- function(input, output, session) {
    
    ################################
    ############ Set up ############
    ################################
    
    # Initialization
    turn <<- -1
    
    isolate({reset()})
    type <<- reactiveValues(l = "U", r = "U", b = "U", b2 = "U", b3 = "U")
    
    observeEvent(input$start,{
        
        max.l <<- max.r <<- input$t.value
        output$t.score.l <- renderText(max.l)
        output$t.score.r <- renderText(max.r)
        
        output$ui.l <- renderUI({
            numericInput(
                'b.value.l',
                label = "",
                value = 0,
                min = 0,
                max = max.l,
                step = 1
            )
        })
        
        output$ui.r <- renderUI({
            numericInput(
                'b.value.r',
                label = "",
                value = 0,
                min = 0,
                max = max.r,
                step = 1
            )
        })
        
    })
    
    
    ################################
    ###########  Start  ############
    ################################
    
    
    observe({
        if(input$more < 5*turn+5){
            if(input$more == 5*turn+1){
                isolate({type$b <- "B"}) 
                
            } 
            if(input$more == 5*turn+2){
                isolate({type$b2 <- "B"})
                
            } 
            if(input$more == 5*turn+3){
                isolate({type$b3 <- "B"})
                
                
                # Finalize the score
                output$winner.l <- renderUI({
                    actionButton('win.l',h3('WIN'),
                                 style = paste0('margin-bottom:5px;border:5px solid; padding: 5px; background-color:#',Score.color,';color:#B21200;'))
                })
                output$winner.r <- renderUI({
                    actionButton('win.r',h3('WIN'),
                                 style = paste0('margin-bottom:5px;border:5px solid; padding: 5px; background-color:#',Score.color,';color:#B21200;'))
                })
                output$winner.d <- renderUI({
                    actionButton('win.d',h3('DRAW'),
                                 style = paste0('margin-bottom:5px;border:5px solid; padding: 5px; background-color:#',Score.color,';color:#B21200;'))
                })
                
            }
            if(input$more == 5*turn+4){
                
                
                # Left Player win
                observeEvent(input$win.l,{
                    max.l <<- max.l + bet
                    bet <<- bet.l <<- bet.r <<- 0
                })
                
                # Right Player win
                observeEvent(input$win.r,{
                    max.r <<- max.r + bet
                    bet <<- bet.l <<- bet.r <<- 0
                })
                
                # Draw
                observeEvent(input$win.d,{
                    max.l <<- max.l + bet/2
                    max.r <<- max.r + bet/2
                    bet <<- bet.l <<- bet.r <<- 0
                })
                
            }
            
            
            
            output$board1 <- renderImage({
                i.card(board[1], type = type$b)
            }, deleteFile = T)
            
            output$board2 <- renderImage({
                i.card(board[2], type = type$b)
            }, deleteFile = T)
            
            output$board3 <- renderImage({
                i.card(board[3], type = type$b)
            }, deleteFile = T)
            
            output$board4 <- renderImage({
                i.card(board[4], type = type$b2)
            }, deleteFile = T)
            
            output$board5 <- renderImage({
                i.card(board[5], type = type$b3)
            }, deleteFile = T)
            
            # players' Hand
            
            # Left Player
            
            observeEvent(input$show.l,{
                type$l <- "B"
            })
            observeEvent(input$hide.l,{
                type$l <- "U"
            })
            
            output$hand.l1 <- renderImage({
                i.card(hand.l[1], type = type$l, FALSE)
            }, deleteFile = T)
            
            output$hand.l2 <- renderImage({
                i.card(hand.l[2], type = type$l, FALSE)
            }, deleteFile = T)
            
            # Right Player
            
            observeEvent(input$show.r,{
                type$r <- "B"
            })
            observeEvent(input$hide.r,{
                type$r <- "U"
            })
            output$hand.r1 <- renderImage({
                i.card(hand.r[1], type = type$r, FALSE)
            }, deleteFile = T)
            
            output$hand.r2 <- renderImage({
                i.card(hand.r[2], type = type$r, FALSE)
            }, deleteFile = T)
            
            
            ################################
            ############  Bet  #############
            ################################
            observeEvent(input$bet.l,{
                output$ui.l <- renderUI({
                    numericInput(
                        'b.value.l',
                        label = "",
                        value = 0,
                        min = 0,
                        max = max.l,
                        step = 1
                    )
                })
                bet.l <<- input$b.value.l
                max.l <<- max.l - bet.l
                bet <<- bet + bet.l
                
                
                output$t.score.l <- renderText(max.l)
                output$b.score.l <- renderText(bet.l)
                output$t.score <- renderText(bet)
                
            })
            
            
            observeEvent(input$bet.r,{
                output$ui.r <- renderUI({
                    numericInput(
                        'b.value.r',
                        label = "",
                        value = 0,
                        min = 0,
                        max = max.r,
                        step = 1
                    )
                })
                bet.r <<- input$b.value.r
                max.r <<- max.r - bet.r
                bet <<- bet + bet.r
                
                
                output$t.score.r <- renderText(max.r)
                output$b.score.r <- renderText(bet.r)
                output$t.score <- renderText(bet)
                
            })
            
            ################################
            ########### Updates ############
            ################################
            
        }else{
            
            isolate({reset()})
            
            type <<- reactiveValues(l = "U", r = "U", b = "U", b2 = "U", b3 = "U")
            
            output$winner.l <- renderUI({})
            output$winner.r <- renderUI({})
            output$winner.d <- renderUI({})
            
            
            # PASTE HERE
            output$board1 <- renderImage({
                i.card(board[1], type = type$b)
            }, deleteFile = T)
            
            output$board2 <- renderImage({
                i.card(board[2], type = type$b)
            }, deleteFile = T)
            
            output$board3 <- renderImage({
                i.card(board[3], type = type$b)
            }, deleteFile = T)
            
            output$board4 <- renderImage({
                i.card(board[4], type = type$b2)
            }, deleteFile = T)
            
            output$board5 <- renderImage({
                i.card(board[5], type = type$b3)
            }, deleteFile = T)
            
            # players' Hand
            
            # Left Player
            
            observeEvent(input$show.l,{
                type$l <- "B"
            })
            observeEvent(input$hide.l,{
                type$l <- "U"
            })
            
            output$hand.l1 <- renderImage({
                i.card(hand.l[1], type = type$l, FALSE)
            }, deleteFile = T)
            
            output$hand.l2 <- renderImage({
                i.card(hand.l[2], type = type$l, FALSE)
            }, deleteFile = T)
            
            # Right Player
            
            observeEvent(input$show.r,{
                type$r <- "B"
            })
            observeEvent(input$hide.r,{
                type$r <- "U"
            })
            output$hand.r1 <- renderImage({
                i.card(hand.r[1], type = type$r, FALSE)
            }, deleteFile = T)
            
            output$hand.r2 <- renderImage({
                i.card(hand.r[2], type = type$r, FALSE)
            }, deleteFile = T)
            
            
            ################################
            ############  Bet  #############
            ################################
            observeEvent(input$bet.l,{
                
                bet.l <<- input$b.value.l
                max.l <<- max.l - bet.l
                bet <<- bet + bet.l
                
                
                output$t.score.l <- renderText(max.l)
                output$b.score.l <- renderText(bet.l)
                output$t.score <- renderText(bet)
                
                
                output$ui.l <- renderUI({
                    numericInput(
                        'b.value.l',
                        label = "",
                        value = 0,
                        min = 0,
                        max = max.l,
                        step = 1
                    )
                })
                
            })
            
            
            observeEvent(input$bet.r,{
                
                bet.r <<- input$b.value.r
                max.r <<- max.r - bet.r
                bet <<- bet + bet.r
                
                
                output$t.score.r <- renderText(max.r)
                output$b.score.r <- renderText(bet.r)
                output$t.score <- renderText(bet)
                
                
                output$ui.r <- renderUI({
                    numericInput(
                        'b.value.r',
                        label = "",
                        value = 0,
                        min = 0,
                        max = max.r,
                        step = 1
                    )
                })
                
            })
            
        } # End of updates
        
    })
    
    
    
    
}




shinyApp(ui = ui, server = server)
