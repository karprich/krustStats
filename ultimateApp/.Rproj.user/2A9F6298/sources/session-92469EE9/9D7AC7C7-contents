#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(magrittr)
library(shinyThings)

linebreaks <- function(n){HTML(strrep(br(), n))}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Frisbee Stats"),
    hr(),
    fluidRow(
      column(3, offset=1,
    selectInput("opponent", label = "Opponent", c("Team 1", "Team 2", "Team 3", "Team 4"))),
    column(3,offset=1,
    dateInput("date", "Date", format = "mm-dd-yy"))
    ),
    # Add the Undo/Redo buttons to the UI
    #undoHistoryUI("hist", back_text = "Step Backward", fwd_text = "Step Forward"),
    linebreaks(1),
    fluidRow(column(7, offset = 2,
                    actionButton("offense", "Offense"),
                    actionButton("defense", "Defense"),
                    actionButton("oppScore", "Opponent Score"),
                    actionButton("submit", "Submit Game"))),
    linebreaks(1),
      fluidRow(
        
        column(7, offset=1,
               strong("Player 1"),
               actionButton("t1_p1_catch", "Catch"),
               actionButton("t1_p1_drop", "Drop"),
               actionButton("t1_p1_ta", "Throw Away"),
               actionButton("t1_p1_score", "Score"),
               actionButton("t1_p1_assist", "Assist"),
               actionButton("t1_p1_defense", "Defense")),
        column(7, offset=1,
               strong("Player 2"),
               actionButton("t1_p2_catch", "Catch"),
               actionButton("t1_p2_drop", "Drop"),
               actionButton("t1_p2_ta", "Throw Away"),
               actionButton("t1_p2_score", "Score"),
               actionButton("t1_p2_assist", "Assist"),
               actionButton("t1_p2_defense", "Defense")),
      column(7, offset=1,
             strong("Player 3"),
             actionButton("t1_p3_catch", "Catch"),
             actionButton("t1_p3_drop", "Drop"),
             actionButton("t1_p3_ta", "Throw Away"),
             actionButton("t1_p3_score", "Score"),
             actionButton("t1_p3_assist", "Assist"),
             actionButton("t1_p3_defense", "Defense")),
      column(7, offset=1,
             strong("Player 4"),
             actionButton("t1_p4_catch", "Catch"),
             actionButton("t1_p4_drop", "Drop"),
             actionButton("t1_p4_ta", "Throw Away"),
             actionButton("t1_p4_score", "Score"),
             actionButton("t1_p4_assist", "Assist"),
             actionButton("t1_p4_defense", "Defense")),
      column(7, offset=1,
             strong("Player 5"),
             actionButton("t1_p5_catch", "Catch"),
             actionButton("t1_p5_drop", "Drop"),
             actionButton("t1_p5_ta", "Throw Away"),
             actionButton("t1_p5_score", "Score"),
             actionButton("t1_p5_assist", "Assist"),
             actionButton("t1_p5_defense", "Defense")),
      column(7, offset=1,
             strong("Player 6"),
             actionButton("t1_p6_catch", "Catch"),
             actionButton("t1_p6_drop", "Drop"),
             actionButton("t1_p6_ta", "Throw Away"),
             actionButton("t1_p6_score", "Score"),
             actionButton("t1_p6_assist", "Assist"),
             actionButton("t1_p6_defense", "Defense"))
     
      ),
    linebreaks(1),
    mainPanel(
      width = 8,
      tableOutput('teamTable')
    ),
    
    mainPanel(
      width = 8,
      tableOutput('score')
    )
      )
 
      
    
    
    
     



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  undo_app_state <- undoHistory(
    id = "hist",
    value = reactive({
      # Value must be a reactive, but can be any structure you want
      req(!is.null(input$text))
      input$text
    }))
  
  observe({
    req(!is.null(undo_app_state())) #<< Need to update app whenever not NULL
    
    # Manually update app UI and reactive values
    updateTextInput(session, "text", value = undo_app_state())
  })

  teamTable <- reactiveVal(
    data.frame(player = c("playerOne", "playerTwo", "playerThree", "playerFour",
                          "playerFive", "playerSix"),
               Catch = as.integer(rep(0, 6)),
               Drop = as.integer(rep(0, 6)),
               TA= as.integer(rep(0, 6)),
               Score = as.integer(rep(0, 6)),
               Assist=as.integer(rep(0, 6)),
               Defense=as.integer(rep(0,6)))
  )
  
  output$teamTable = renderTable(teamTable(),
                                bordered = TRUE,
                                spacing = c('l'),
                                width = "100%",
                                striped = TRUE,
                                align = 'c',
                                rownames = FALSE)
  
  score <- reactiveVal(
    data.frame(
               Score = as.integer(rep(0, 1)),
               Opponent = as.integer(rep(0, 1)))
  )
  
  output$score = renderTable(score(),
                                 bordered = TRUE,
                                 spacing = c('l'),
                                 width = "100%",
                                 striped = TRUE,
                                 align = 'c',
                                 rownames = FALSE)
  
  script <- data.frame(test=as.character(Sys.Date()),
                                      opponent="0",
                                      play="0")
  
  #Scores
  observeEvent(input$oppScore, { 
    tmp <- score()
    tmp[1, "Opponent"] <-  tmp[1, "Opponent"]+as.integer(1)
    score(tmp)
    
  })
  # 
  # output$script = renderTable(script(),
  #                                bordered = TRUE,
  #                                spacing = c('l'),
  #                                width = "100%",
  #                                striped = TRUE,
  #                                align = 'c',
  #                                rownames = FALSE)
  
 
  
  observeEvent(input$opponent, {
    script <- bind_rows(script, data.frame(opponent=input$opponent))
  })
  
  observeEvent(input$date, {
    script <- bind_rows(script, data.frame(date=input$date))
  })
  
  #Catches
  observeEvent(input$t1_p1_catch, { 
    tmp <- teamTable()
    tmp[1, "Catch"] <-  tmp[1, "Catch"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P1_Catch"))
  })
  observeEvent(input$t1_p2_catch, { 
    tmp <- teamTable()
    tmp[2, "Catch"] <-  tmp[2, "Catch"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P2_Catch"))
  })
  observeEvent(input$t1_p3_catch, { 
    tmp <- teamTable()
    tmp[3, "Catch"] <-  tmp[3, "Catch"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P3_Catch"))
  })
  observeEvent(input$t1_p4_catch, { 
    tmp <- teamTable()
    tmp[4, "Catch"] <-  tmp[4, "Catch"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P4_Catch"))
  })
  observeEvent(input$t1_p5_catch, { 
    tmp <- teamTable()
    tmp[5, "Catch"] <-  tmp[5, "Catch"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P5_Catch"))
  })
  observeEvent(input$t1_p6_catch, { 
    tmp <- teamTable()
    tmp[6, "Catch"] <-  tmp[6, "Catch"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P6_Catch"))
  })
  
  #Drops
  observeEvent(input$t1_p1_drop, { 
    tmp <- teamTable()
    tmp[1, "Drop"] <-  tmp[1, "Drop"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P1_Drop"))
  })
  observeEvent(input$t1_p2_drop, { 
    tmp <- teamTable()
    tmp[2, "Drop"] <-  tmp[2, "Drop"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P2_Drop"))
  })
  observeEvent(input$t1_p3_drop, { 
    tmp <- teamTable()
    tmp[3, "Drop"] <-  tmp[3, "Drop"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P3_Drop"))
  })
  observeEvent(input$t1_p4_drop, { 
    tmp <- teamTable()
    tmp[4, "Drop"] <-  tmp[4, "Drop"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P4_Drop"))
  })
  observeEvent(input$t1_p5_drop, { 
    tmp <- teamTable()
    tmp[5, "Drop"] <-  tmp[5, "Drop"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P5_Drop"))
  })
  observeEvent(input$t1_p6_drop, { 
    tmp <- teamTable()
    tmp[6, "Drop"] <-  tmp[6, "Drop"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P6_Drop"))
  })
  
  #Throw Aways
  observeEvent(input$t1_p1_ta, { 
    tmp <- teamTable()
    tmp[1, "TA"] <-  tmp[1, "TA"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P1_TA"))
  })
  observeEvent(input$t1_p2_ta, { 
    tmp <- teamTable()
    tmp[2, "TA"] <-  tmp[2, "TA"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P2_TA"))
  })
  observeEvent(input$t1_p3_ta, { 
    tmp <- teamTable()
    tmp[3, "TA"] <-  tmp[3, "TA"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P3_TA"))
  })
  observeEvent(input$t1_p4_ta, { 
    tmp <- teamTable()
    tmp[4, "TA"] <-  tmp[4, "TA"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P4_TA"))
  })
  observeEvent(input$t1_p5_ta, { 
    tmp <- teamTable()
    tmp[5, "TA"] <-  tmp[5, "TA"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P5_TA"))
  })
  observeEvent(input$t1_p6_ta, { 
    tmp <- teamTable()
    tmp[6, "TA"] <-  tmp[6, "TA"]+as.integer(1)
    teamTable(tmp)
    script <- bind_rows(script, data.frame(play="P6_TA"))
  })
  
  #Score
  observeEvent(input$t1_p1_score, { 
    tmp <- teamTable()
    tmp2 <- score()
    tmp[1, "Score"] <-  tmp[1, "Score"]+as.integer(1)
    tmp2[1, "Score"] <- tmp2[1, "Score"]+as.integer(1)
    teamTable(tmp)
    score(tmp2)
    script <- bind_rows(script, data.frame(play="P1_Score"))
  })
  observeEvent(input$t1_p2_score, { 
    tmp <- teamTable()
    tmp2 <- score()
    tmp[2, "Score"] <-  tmp[2, "Score"]+as.integer(1)
    tmp2[1, "Score"] <- tmp2[1, "Score"]+as.integer(1)
    teamTable(tmp)
    score(tmp2)
    script <- bind_rows(script, data.frame(play="P2_Score"))
  })
  observeEvent(input$t1_p3_score, { 
    tmp <- teamTable()
    tmp2 <- score()
    tmp[3, "Score"] <-  tmp[3, "Score"]+as.integer(1)
    tmp2[1, "Score"] <- tmp2[1, "Score"]+as.integer(1)
    teamTable(tmp)
    score(tmp2)
    script <- bind_rows(script, data.frame(play="P3_Score"))
  })
  observeEvent(input$t1_p4_score, { 
    tmp <- teamTable()
    tmp2 <- score()
    tmp[4, "Score"] <-  tmp[4, "Score"]+as.integer(1)
    tmp2[1, "Score"] <- tmp2[1, "Score"]+as.integer(1)
    teamTable(tmp)
    score(tmp2)
    script <- bind_rows(script, data.frame(play="P4_Score"))
  })
  observeEvent(input$t1_p5_score, { 
    tmp <- teamTable()
    tmp2 <- score()
    tmp[5, "Score"] <-  tmp[5, "Score"]+as.integer(1)
    tmp2[1, "Score"] <- tmp2[1, "Score"]+as.integer(1)
    teamTable(tmp)
    score(tmp2)
    script <- bind_rows(script, data.frame(play="P5_Score"))
  })
  observeEvent(input$t1_p6_score, { 
    tmp <- teamTable()
    tmp2 <- score()
    tmp[6, "Score"] <-  tmp[6, "Score"]+as.integer(1)
    tmp2[1, "Score"] <- tmp2[1, "Score"]+as.integer(1)
    teamTable(tmp)
    score(tmp2)
    script <- bind_rows(script, data.frame(play="P6_Score"))
  })
   
}



teamOne <- tibble(team="1", players=c("A", "B", "C", "D", "E", "F"))
teamTwo <- tibble(team="2", players=c("G", "H", "I", "J", "K", "L"))
teams <- bind_rows(teamOne, teamTwo)

# Run the application 
shinyApp(ui = ui, server = server)
