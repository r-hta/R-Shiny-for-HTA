# ============== #
# Robert Smith & Paul Schneider
# University of Sheffield
# contact: rasmith3@sheffield.ac.uk
# Project: Making health economics Shiny: a tutorial
# Description: ShinyApp ui and server function - as in publication
# ============== #

# Setup ----

rm(list= ls()) # clear objects from workspace

# install.packages("shiny")     # only necessary once if you don't already have the function 'shiny' installed.
# install.packages("truncnorm") # only necessary once if you don't already have the function 'truncnorm' installed.

# load packages from library
library(truncnorm)
library(shiny)             

# source functions
source("f_gen_psa.R")
source("f_MM_sicksicker.R")
source("f_wrapper.R")


# Shiny user interface code ---- 

ui <- fluidPage(    # creates empty page
  
  # title of app
  titlePanel("Sick Sicker Model in Shiny"),   
  
  # layout is a sidebar-layout
  sidebarLayout(    
    
    sidebarPanel( # open sidebar panel
      
      # input type numeric    
      numericInput(inputId = "SI_c_Trt",      
                   label = "Treatment Cost", 
                   value = 200,              
                   min = 0,                   
                   max = 400),               
      
      numericInput(inputId = "SI_n_sim",     
                   label = "PSA runs",       
                   value = 1000,              
                   min = 0,                   
                   max = 400),               
      
      # input type slider    
      sliderInput(inputId = "SI_n_age_init",  
                  label = "Initial Age",      
                  value = 25,                
                  min = 10,                   
                  max = 80),                  
      
      # action button runs model when pressed
      actionButton(inputId = "run_model",     
                   label   = "Run model")    
      
    ),  # close sidebarPanel
    
    # open main panel    
    mainPanel(                                
      
      # heading (results table)       
      h3("Results Table"),                                 
      
      # tableOutput id = icer_table, from server    
      tableOutput(outputId = "SO_icer_table"),   
      
      # heading (Cost effectiveness plane)    
      h3("Cost-effectiveness Plane"),         
      
      # plotOutput id = SO_CE_plane, from server
      plotOutput(outputId = "SO_CE_plane")       
      
    ) # close mainpanel    
    
  ) # close sidebarlayout
  
) # close UI fluidpage





# Shiny server function ----
server <- function(input, output){   
  
  # when action button pressed ...
  observeEvent(input$run_model,       
               ignoreNULL = F, {
                 
                 # Run model function with Shiny inputs
                 df_model_res = f_wrapper(
                   c_Trt = input$SI_c_Trt,
                   n_age_init = input$SI_n_age_init,
                   n_sim = input$SI_n_sim)
                 
                 #-- CREATE COST EFFECTIVENESS TABLE --#
                 
                 # renderTable continuously updates table
                 output$SO_icer_table <- renderTable({ 
                   
                   df_res_table <- data.frame( # create dataframe
                     
                     Option =  c("Treatment","No Treatment"), 
                     
                     QALYs  =  c(mean(df_model_res$QALY_Trt),
                                 mean(df_model_res$QALY_NoTrt)),
                     
                     Costs  =  c(mean(df_model_res$Cost_Trt),
                                 mean(df_model_res$Cost_NoTrt)),
                     
                     Inc.QALYs = c(mean(df_model_res$QALY_Trt) - 
                                     mean(df_model_res$QALY_NoTrt),
                                   NA),
                     
                     Inc.Costs = c(mean(df_model_res$Cost_Trt) -
                                     mean(df_model_res$Cost_NoTrt),
                                   NA),
                     
                     ICER = c(mean(df_model_res$ICER), NA)
                     
                   ) # close data-frame
                   
                   # round the data-frame to two digits
                   df_res_table[,2:6] = round(
                     df_res_table[,2:6],digits = 2) 
                   
                   # print the results table
                   df_res_table
                   
                 }) # table plot end.
                 
                 
                 #--  CREATE COST EFFECTIVENESS PLANE --#
                 
                 # render plot repeatedly updates.
                 output$SO_CE_plane <- renderPlot({ 
                   
                   # calculate incremental costs and qalys
                   df_model_res$inc_C <- df_model_res$Cost_Trt - 
                     df_model_res$Cost_NoTrt
                   
                   df_model_res$inc_Q <- df_model_res$QALY_Trt - 
                     df_model_res$QALY_NoTrt
                   
                   # create cost effectiveness plane plot
                   
                   plot(
                     # x y are incremental QALYs Costs
                     x = df_model_res$inc_Q, 
                     y = df_model_res$inc_C,
                     
                     # label axes
                     xlab = "Incremental QALYs", 
                     ylab = "Incremental Costs", 
                     
                     # set x-limits and y-limits for plot.
                     xlim = c( min(df_model_res$inc_Q,
                                   df_model_res$inc_Q*-1),
                               max(df_model_res$inc_Q,
                                   df_model_res$inc_Q*-1)),
                     
                     ylim = c( min(df_model_res$inc_C,
                                   df_model_res$inc_C*-1),
                               max(df_model_res$inc_C,
                                   df_model_res$inc_C*-1)),
                     
                     # include y and y axis lines.
                     abline(h = 0,v=0)
                     
                   ) # CE plot end
                   
                 }) # renderplot end
                 
               }) # Observe event end
  
  
} # Server end


# Running the App ----
shinyApp(ui, server)
