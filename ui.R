library(plotly)
library(shiny)
library(ggplot2)

################################################################################
########### Samplemeta #########################################################
################################################################################


tab_about <- fluidPage(
  p("This demo was originally developed by ", a("Johan Henriksson", href="http://www.henlab.org")),
  p("Licensed under 2-clause BSD license, https://opensource.org/license/bsd-2-clause/")
)

################################################################################
########### Markov chain tab ###################################################
################################################################################


tab_chain <- fluidPage(
  fluidRow(
    column(6,
           #textAreaInput("mc", "Markov chain definition", value="s0,s1,s2 &#10; n1,1,1", rows = 10)  ,

           HTML('<textarea id="mc_def" rows="10" cols="40">s0,s1,s2&#10;0.5,0,0.5&#10;0.5,0.5,0&#10;0,0.5,0.5</textarea><br/>'),
           actionButton("mc_submit", "Create"),
           textOutput("mc_status"),
           plotOutput(outputId = "plotChain", height = "400px")

    ),
    column(6,

           textInput("mc_starting_prob", label = "Starting probabilities:"),
           actionButton("mc_sim_jumping", "Simulate jumping"),
           actionButton("mc_compute_prob", "Compute markov probabilities"),
           tableOutput("mc_sim_table")
           
           
    ),
  )
)


################################################################################
########### Total page #########################################################
################################################################################

#https://stackoverflow.com/questions/72040479/how-to-position-label-beside-slider-in-r-shiny

ui <- fluidPage(
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 100px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 300px;          /* Target width for slider */
    }
    "
  )),
  
  titlePanel("Demo of Markov chains"),

  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Markov chain", tab_chain),
                tabPanel("About", tab_about)
    )
  )
  
)



