options(shiny.sanitize.errors = FALSE)

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
########### HMM tab ############################################################
################################################################################


tab_hmm <- fluidPage(
  fluidRow(
    column(6,
           h3("Simulate HMM"),
           
           div(class = "label-left",
               sliderInput(
                 inputId = "hmm_sim_random_seed",
                 label = "Random seed:",
                 min=0,
                 max=10,
                 step = 1,
                 value=1
               ),
               
               sliderInput(
                 inputId = "hmm_sim_numsample",
                 label = "Number of samples:",
                 min=1,
                 max=5000,
                 step=1,
                 value=100
               ),
               
               sliderInput(
                 inputId = "hmm_sim_nstate",
                 label = "Number of states:",
                 min=1,
                 max=10,
                 step=1,
                 value=2
               ),
               
               
               sliderInput(
                 inputId = "hmm_sim_numemit",
                 label = "Number of symbols emitted:",
                 min=1,
                 max=20,
                 step=1,
                 value=4
               ),
               
               sliderInput(
                 inputId = "hmm_sim_stayweight",
                 label = "Tendency to stay:",
                 min=0,
                 max=10,
                 step=0.01,
                 value=1
               ),
               
           ),

           
           actionButton("hmm_generate", "Simulate HMM"),
           
           
    ),
    column(6,
           h3("Fit HMM"),
           
           div(class = "label-left",
               sliderInput(
                 inputId = "hmm_fit_random_seed",
                 label = "Random seed:",
                 min=0,
                 max=10,
                 step = 1,
                 value=1
               ),
               
               
               sliderInput(
                 inputId = "hmm_fit_nstate",
                 label = "Assume number of states:",
                 min=1,
                 max=10,
                 step=1,
                 value=2
               ),
           ),
           actionButton("hmm_fit", "Fit HMM"),
           
    ),
  ),
  fluidRow(
    column(6,
           h3("Simulated HMM"),
           plotOutput("hmm_plot_sim"),
           p("Transition matrix"),
           tableOutput("hmm_sim_trans"),
           p("Emission matrix"),
           tableOutput("hmm_sim_emit"),
    ),
    column(6,
           h3("Fitted HMM"),
           plotOutput("hmm_plot_fit"),
           p("Fitted transition matrix"),
           tableOutput("hmm_fit_trans"),
           p("Fitted emission matrix"),
           tableOutput("hmm_fit_emit"),

    ),
  ),

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
                tabPanel("HMM", tab_hmm),
                tabPanel("Markov chain", tab_chain),
                tabPanel("About", tab_about)
    )
  )
  
)



