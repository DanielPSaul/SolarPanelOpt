library(shiny)
library(tidyverse)
library(lpSolve)
library(shinythemes) # UI themes for shiny
library(shinydashboard)
library(DT) # Data table functionality
library(shinyalert) # Used to create the popups/alerts
library(shinyBS) # Tooltip descriptions
library(shinyjs) # 'Quit' page to prevent freezes on local machine
library(rmarkdown) # For printing shiny
library(xlsx)
library(readxl)
library(writexl)
library(rsconnect)

ui <- fluidPage(
  
  theme = shinytheme("united"),
  navbarPage(title="Solar Panel Calculator (Greenhouse)", 
             windowTitle = "Solar Panel Calculator",
             
             
             tabPanel(
               '',
               icon = icon("bolt"),
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Inputs"),
                   width = 3,
                   
                   helpText('Please only enter numeric values into the input boxes. Default values populate the input boxes.'),
                   
                   # Number of periods
                   numericInput(inputId = "n2",
                                label = "Number of Periods:", 
                                width = '200px',
                                value = 20,
                                min=0,
                                max=99999),
                   
                   # Panel costs and change over time
                   numericInput(inputId = "Pinit2",
                                label = "Panel Cost:", 
                                width = '200px',
                                value = 200,
                                min=0,
                                max=99999),
                   numericInput(inputId = "Prate2",
                                label = "Panel Cost Change:", 
                                width = '200px',
                                value = 0.98,
                                min=0,
                                max=99999),
                   
                   # Electricity cost and change over time
                   numericInput(inputId = "Einit2",
                                label = "Electricity Cost:", 
                                width = '200px',
                                value = 0.11,
                                min=0,
                                max=99999),
                   numericInput(inputId = "Erate2",
                                label = "Electricity Cost Change:", 
                                width = '200px',
                                value = 1.02,
                                min=0,
                                max=99999),
                   
                   # Demand for electricity and change over time
                   numericInput(inputId = "Dinit2",
                                label = "Electricity Demand:", 
                                width = '200px',
                                value = 10000,
                                min=0,
                                max=99999),
                   numericInput(inputId = "Drate2",
                                label = "Electricity Demand Change:", 
                                width = '200px',
                                value = 1.04,
                                min=0,
                                max=99999),
                   
                   # Electricity generated per panel and change over time
                   numericInput(inputId = "Ginit2",
                                label = "Electricity Gen per Panel:", 
                                width = '200px',
                                value = 2000,
                                min=0,
                                max=99999),
                   numericInput(inputId = "Grate2",
                                label = "Electricity Gen per Panel Change:", 
                                width = '200px',
                                value = 0.98,
                                min=0,
                                max=99999),
                   radioButtons(inputId = 'readjust_gen', 
                                label = 'Readjust generation capacity to include effect of new panels?', 
                                choiceNames = c('No','Yes'), 
                                choiceValues = c(0,1)),
                   helpText('Press \'Submit\' to perform the calculations and produce outputs.'),
                   actionButton('submit_iterative', 'Submit', width = '100px')
                 ),
                 mainPanel(
                   
                   fluidRow(
                     column(width = 3, downloadButton("download2", 'Download Results'), helpText('Select \'Download Results\' to download an Excel file of the outputs.')),
                     column(width = 3, actionButton('restart_button', 'Restart Session', icon = icon("arrows-rotate")), helpText('Select \'Restart Session\' to reset the inputs and clear outputs.'))
                   ),
    
                   hr(),
                   fluidRow(
                     column(width = 7,
                            h4('Total Cost:'),
                            textOutput('iterative_results1'),
                            hr(),
                            tableOutput('iterative_results2')),
                     column(width = 5,
                            conditionalPanel(condition = "input.readjust_gen == 1",
                                             h4('Total Cost (Adjusted):'),
                                             textOutput("iterative_results3"),
                                             hr(),
                                             tableOutput("iterative_results4")))
                   )
                   
                 )
               )
             )
    
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$restart_button, {
    session$reload()
  })


  # ITERATIVE LP CALCULATIONS
  observeEvent(input$submit_iterative, {
    
    n <-  as.numeric(input$n2)
    Pinit <- as.numeric(input$Pinit2)
    Prate <- as.numeric(input$Prate2)
    Einit <- as.numeric(input$Einit2)
    Erate <- as.numeric(input$Erate2)
    Dinit <- as.numeric(input$Dinit2)
    Drate <- as.numeric(input$Drate2)
    Ginit <- as.numeric(input$Ginit2)
    Grate <- as.numeric(input$Grate2)
    
    # Set up the vectors for the period
    P <- vector("numeric", n)
    P[1] <- Pinit
    for(i in 2:n) {
      P[i] <- P[i-1]*Prate
    }
    
    E <- vector("numeric", n)
    E[1] <- Einit
    for(i in 2:n) {
      E[i] <- E[i-1]*Erate
    }
    
    D <- vector("numeric", n)
    D[1] <- Dinit
    for(i in 2:n) {
      D[i] <- D[i-1]*Drate
    }
    
    G <- vector("numeric", n)
    G[1] <- Ginit
    for(i in 2:n) {
      G[i] <- G[i-1]*Grate
    }
    
    # Objective functions
    f.obj <- rbind(P,E)
    f.obj <-  as.vector(f.obj)
    
    # Constraints
    f.con <- matrix(0,nrow = n, ncol = n*2)
    for (i in 1:n) {
      j <-(i-1)*2 + 1
      f.con[i,j] <-  G[(i+1)/2]
      f.con[i,j+1] <- 1
    }
    
    f.rhs <- D
    f.eq <- c(rep(">=",n))
    
    # Integer number of panels
    int.vec <- seq(1,39,2)
    
    # Solve the LP problem
    results1 <- lp('min', f.obj, f.con, f.eq, f.rhs, transpose.constraints = TRUE, int.vec)
    
    output$iterative_results1 <- renderText({
      objval1 <- round(results1$objval, 2)
      objval1 <- sapply(objval1, FUN=function(x) prettyNum(x, big.mark = ","))
      paste0('$', objval1)
    })
    
    output$iterative_results2 <- renderTable({
      re_sol1 <- results1$solution
      matrix1 <- matrix(re_sol1, ncol = 2, byrow = TRUE)
      sol_df1 <- as.data.frame(matrix1)
      sol_df1 <- rownames_to_column(sol_df1, var = "index")
      names(sol_df1) <- c('Period', 'Number of Panels', 'Purchased kWh')
      sol_df1$`Number of Panels` = as.integer(sol_df1$`Number of Panels`)
      sol_df1$`Purchased kWh` <- sapply(sol_df1$`Purchased kWh`, FUN=function(x) prettyNum(x, big.mark = ",", digits = 5))
      sol_df1
    })
    
    
    # Readjust generation capacity to include effect of new panels
    if (input$readjust_gen == 1) {
      
      panel <- results1$solution[seq(1,n*2,2)]
      G.adj <- vector("numeric", n)
      G.adj <- Ginit*panel[1]
      panel.last = panel[1]
      
      for (i in 2:n) {
        if(panel[i] == panel.last) {
          G.adj[i] <- G.adj[i-1]*Grate
        }
        else {
          diff <-  panel[i] - panel.last
          panel.last <- panel[i]
          G.adj[i] <- G.adj[i-1]*Grate + diff*Ginit
        }
      }
      
      # revised output per panel
      G.adj <- G.adj/panel
      f.con <- matrix(0,nrow = n, ncol = n*2)
      for (i in 1:n) {
        j <-(i-1)*2 + 1
        f.con[i,j] <-  G.adj[(i+1)/2]
        f.con[i,j+1] <- 1
      }
      
      results2 <- lp('min', f.obj, f.con, f.eq, f.rhs, transpose.constraints = TRUE, int.vec)
      
      output$iterative_results3 <- renderText({
        objval2 <- round(results2$objval, 2)
        objval2 <- sapply(objval2, FUN=function(x) prettyNum(x, big.mark = ","))
        paste0('$', objval2)
      })
      
      output$iterative_results4 <- renderTable({
        re_sol2 <- results2$solution
        matrix2 <- matrix(re_sol2, ncol = 2, byrow = TRUE)
        sol_df2 <- as.data.frame(matrix2)
        sol_df2 <- rownames_to_column(sol_df2, var = "index")
        names(sol_df2) <- c('Period', 'Number of Panels', 'Purchased kWh')
        sol_df2$`Number of Panels` = as.integer(sol_df2$`Number of Panels`)
        sol_df2$`Purchased kWh` <- sapply(sol_df2$`Purchased kWh`, FUN=function(x) prettyNum(x, big.mark = ",", digits = 5))
        sol_df2
      })
      
    }
    
    
    # DOWNLOAD ITERATIVE LP RESULTS
    
    output$download2 <- downloadHandler(
      filename = function() {
        paste("iterative-output-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        
        # File 1
        objval1 <- results1$objval
        re_sol1 <- results1$solution
        matrix1 <- matrix(re_sol1, ncol = 2, byrow = TRUE)
        sol_df1 <- as.data.frame(matrix1)
        sol_df1 <- rownames_to_column(sol_df1, var = "index")
        names(sol_df1) <- c('Period', 'Number of Panels', 'Purchased kWh')
        sol_df1$`Total Cost ($)` <- NA
        sol_df1$`Total Cost ($)`[1] <- objval1
        sol_df1$`Total Cost ($)`[is.na(sol_df1$`Total Cost ($)`)] <- ""
        
        if (input$readjust_gen == 1) {
          
          # File 2 (readjusted)
          objval2 <- results2$objval
          re_sol2 <- results2$solution
          matrix2 <- matrix(re_sol2, ncol = 2, byrow = TRUE)
          sol_df2 <- as.data.frame(matrix2)
          sol_df2 <- rownames_to_column(sol_df2, var = "index")
          names(sol_df2) <- c('Period', 'Number of Panels', 'Purchased kWh')
          sol_df2$`Total Cost ($)` <- NA
          sol_df2$`Total Cost ($)`[1] <- objval2
          sol_df2$`Total Cost ($)`[is.na(sol_df2$`Total Cost ($)`)] <- ""
          
          write_xlsx(list("Not Adjusted" = sol_df1, "Adjusted" = sol_df2), file, col_names = TRUE, format_headers = TRUE)
          
        } else {
          
          write.xlsx(sol_df1, file, col.names = TRUE, row.names = FALSE, showNA = FALSE)
          
        } 
        
        

      }
    )
    
    
  }) # End of Iterative calculations
  
} # End of server

shinyApp(ui = ui, server = server)