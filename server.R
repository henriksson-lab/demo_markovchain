library(plotly)
library(Cairo)
library(HMM)
library(markovchain)
options(shiny.usecairo=T)


if(FALSE){
  install.packages("matlib")
}



if(FALSE){
  #To run this app
  library(shiny)
  runApp(".")
}


server <- function(input, output, session) {


  ##############################################################################
  ########### General functions ################################################
  ##############################################################################
  
  
  ##############################################################################
  ########### Callbacks - dataset ##############################################
  ##############################################################################
  
  
  ##############################################################################
  ########### Markov chain tab #################################################
  ##############################################################################

#  sampling_points <- reactive({
    
 # })
  
  default_matrix <- matrix(1, ncol=1, nrow=1)
  colnames(default_matrix) <- "s0"
  rownames(default_matrix) <- "s0"
  
  mc_matrix <- reactiveValues(themat = default_matrix, status="")
  
  observeEvent(input$mc_submit,{
#    mc_matrix$themat <- matrix(0, ncol=1, nrow=1)
    #comment_value$default <- input$randtext
    
    mcdef <- input$mc_def
    #mcdef <- "s0,s1,s2\n0.5,0,0.5\n0.5,0.5,0\n0,0.5,0.5"
    all_lines <- strsplit(mcdef, "\\n")[[1]]
    
    if(length(all_lines)==0){
      mc_matrix$status <- "Error: No input"
    } else {
      bycomma <- strsplit(all_lines,",")
      ncol <- length(bycomma[[1]])
      if(!all(sapply(bycomma,function(x) length(x)==ncol))){
        mc_matrix$status <- "Error: Different number of columns among lines"
      } else {
        
        therest <- bycomma[-1]
        themat <- t(sapply(therest, function(x) as.double(x)))
        colnames(themat) <- bycomma[[1]]
        rownames(themat) <- bycomma[[1]]
        themat <- as.matrix(themat)
        
        if(all(rowSums(themat)==1)){
          mc_matrix$themat <- themat
          mc_matrix$status <- "OK"

          p0 <- do.call(paste0,as.list(c("1",rep(",0", ncol-1))))
          updateTextInput(session, "mc_starting_prob",value = p0)
        } else {
          mc_matrix$status <- "Error: Each column must sum to 1"
        }
      }
      
    }
    
  })
  
  output$mc_status <- renderText({ mc_matrix$status })

  getMarkovChain <- function(){
    transprob <- mc_matrix$themat
    transmat <- new("markovchain", states = colnames(transprob), byrow = FALSE,
                    transitionMatrix = transprob, name = "Weather")
    transmat
  }
  
  output$plotChain <- renderPlot({
    transmat <- getMarkovChain()
    plot(transmat)
  })

  
  
  ## TODO handle parse error too
  getStartingProb <- function(){
    p0 <- try({
      p0 <- strsplit(input$mc_starting_prob,",")[[1]]
      p0 <- as.double(p0)
      if(length(p0)!=ncol(mc_matrix$themat)){
        p0 <- NULL
        mc_matrix$status <- "Error: Wrong length of starting probability"
      } else {
        if(sum(p0)!=1){
          p0 <- NULL
          mc_matrix$status <- "Error: Starting probability should sum to 0"
        }
      }
      p0      
    })
    if (class(p0) == "try-error") {
      p0 <- NULL
      mc_matrix$status <- "Error: Failing to parse"
    }
    p0
  }
  
  ##############################################################################
  ############ Compute probabilities
  ##############################################################################
  observeEvent(input$mc_compute_prob,{
    p0 <- getStartingProb()
    if(!is.null(p0)){
      
      themat <- getMarkovChain()
      
      numstep <- 100
      outmat <- matrix(ncol=dim(themat),nrow=numstep+1)
      colnames(outmat) <- names(themat)
      outmat[1,] <- p0
      for(i in 1:numstep){
        outmat[i+1,] <- outmat[i,]*themat
      }
      output_table$thetab <- as.data.frame(outmat)

    } else {
      output_table$thetab <- c()
    }
    
  })

  ##############################################################################
  ############ Simulate discrete jumping
  ##############################################################################
  observeEvent(input$mc_sim_jumping,{
    
    p0 <- getStartingProb()
    if(!is.null(p0)){
      
      themat <- getMarkovChain()
      
      numstep <- 100
      outmat <- matrix(0, ncol=dim(themat),nrow=numstep+1)
      colnames(outmat) <- names(themat)
      
      firsti <- sample(1:dim(themat), 1, prob = p0)
      outmat[1,firsti] <- 1
      
      for(i in 1:numstep){
        nexti <- sample(1:dim(themat), 1, prob = outmat[i,]*themat)
        outmat[i+1,nexti] <- 1
      }
      output_table$thetab <- as.data.frame(outmat)

    } else {
      output_table$thetab <- c()
    }
    
  })
  
  output_table <- reactiveValues(thetab = c())
  
  output$mc_sim_table <- renderTable(output_table$thetab)
  
  
  
}







