if(FALSE){
  options(shiny.sanitize.errors = FALSE)
  # logging level DEBUG
  logging::basicConfig(level = 10)
  # write logging output to the stderr file
  logging::addHandler(logging::writeToFile, logger = '', file = stderr())
}

library(plotly)
library(Cairo)
library(HMM)
library(markovchain)
library(depmixS4)

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

  
  mc_default_matrix <- matrix(1, ncol=1, nrow=1)
  colnames(mc_default_matrix) <- "s0"
  rownames(mc_default_matrix) <- "s0"
  
  mc_current_matrix <- reactiveValues(themat = mc_default_matrix, status="")
  
  observeEvent(input$mc_submit,{
    mcdef <- input$mc_def
    #mcdef <- "s0,s1,s2\n0.5,0,0.5\n0.5,0.5,0\n0,0.5,0.5"
    all_lines <- strsplit(mcdef, "\\n")[[1]]
    
    if(length(all_lines)==0){
      mc_current_matrix$status <- "Error: No input"
    } else {
      bycomma <- strsplit(all_lines,",")
      ncol <- length(bycomma[[1]])
      if(!all(sapply(bycomma,function(x) length(x)==ncol))){
        mc_current_matrix$status <- "Error: Different number of columns among lines"
      } else {
        
        therest <- bycomma[-1]
        themat <- t(sapply(therest, function(x) as.double(x)))
        colnames(themat) <- bycomma[[1]]
        rownames(themat) <- bycomma[[1]]
        themat <- as.matrix(themat)
        
        if(all(rowSums(themat)==1)){
          mc_current_matrix$themat <- themat
          mc_current_matrix$status <- "OK"

          p0 <- do.call(paste0,as.list(c("1",rep(",0", ncol-1))))
          updateTextInput(session, "mc_starting_prob",value = p0)
        } else {
          mc_current_matrix$status <- "Error: Each column must sum to 1"
        }
      }
      
    }
    
  })
  
  output$mc_status <- renderText({ mc_current_matrix$status })

  getMarkovChain <- function(){
    transprob <- mc_current_matrix$themat
    transmat <- new("markovchain", states = colnames(transprob), byrow = FALSE,
                    transitionMatrix = transprob, name = "Weather")
    transmat
  }
  
  output$plotChain <- renderPlot({
    transmat <- getMarkovChain()
    plot(transmat)
  })

  getStartingProb <- function(){
    p0 <- try({
      p0 <- strsplit(input$mc_starting_prob,",")[[1]]
      p0 <- as.double(p0)
      if(length(p0)!=ncol(mc_current_matrix$themat)){
        p0 <- NULL
        mc_current_matrix$status <- "Error: Wrong length of starting probability"
      } else {
        if(sum(p0)!=1){
          p0 <- NULL
          mc_current_matrix$status <- "Error: Starting probability should sum to 0"
        }
      }
      p0      
    })
    if (class(p0) == "try-error") {
      p0 <- NULL
      mc_current_matrix$status <- "Error: Failing to parse"
    }
    p0
  }
  
  ############ Action: Compute probabilities
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

  ############ Action: Simulate discrete jumping
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
  
  
  
  
  ##############################################################################
  ########### HMM tab ##########################################################
  ##############################################################################
  
  
  
  
  simulateHMM <- function(hmm_random_seed, numstate, numsample, numemit, stayweight){
    
    set.seed(hmm_random_seed)
    
    statenames <- paste("s",1:numstate)

    #Define emission probs
    emissionprob <- matrix(ncol = numemit, nrow=numstate)
    colnames(emissionprob) <- paste0("e",1:numemit)  #c("A","T","C","G")
    emissionprob[1:length(emissionprob)] <- runif(length(emissionprob))
    for(i in 1:numstate){
      emissionprob[i,] <- emissionprob[i,]/sum(emissionprob[i,])
    }
    
    #Define state transitions
    transprob <- matrix(ncol = numstate, nrow=numstate)
    transprob[1:length(transprob)] <- runif(length(transprob))
    diag(transprob) <- diag(transprob)+stayweight
    for(i in 1:numstate){
      transprob[,i] <- transprob[,i]/sum(transprob[,i])
    }
    colnames(transprob) <- statenames
    rownames(transprob) <- statenames
    
    observations <- paste(colnames(emissionprob))
    startProbs <- c(1, rep(0, numstate-1))
    
    #Simulate
    hmm <- HMM::initHMM(
      States=statenames, 
      Symbols=observations, 
      startProbs=startProbs, 
      transProbs=transprob, 
      emissionProbs=emissionprob)
    res <- HMM::simHMM(hmm, numsample)
    
    #Return
    res$transprob <- transprob
    res$emissionprob <- emissionprob
    res
  }
  
  ############ HMM data state
  simhmm <- reactiveValues(state = simulateHMM(1, 2, 10, 2, 3), fit=NULL)
  
  ############ Action: Button pressed to generate HMM
  observeEvent(input$hmm_generate,{
    simhmm$state <- simulateHMM(
      input$hmm_sim_random_seed,
      input$hmm_sim_nstate, 
      input$hmm_sim_numsample,
      input$hmm_sim_numemit,
      input$hmm_sim_stayweight
    )
  })
  
  ############ The scatter plot of the simulated data
  output$hmm_plot_sim <- renderPlot({
    
    res <- simhmm$state
    thedat <- data.frame(
      i=1:length(res$states),
      emit=res$observation,
      state=res$states
      )
    
    p1 <- ggplot(thedat, aes(i, emit))  + geom_point()
    p2 <- ggplot(thedat, aes(i, state)) + geom_point()
    egg::ggarrange(p1,p2, ncol=1)
  })
  
  output$hmm_sim_trans <- renderTable({
    res <- simhmm$state
    t(res$transprob)
  })
  output$hmm_sim_emit <- renderTable({
    res <- simhmm$state
    res$emissionprob
  })
  

  
  
  ############ Fit HMM
  fitHMM <- function(){

    set.seed(input$hmm_fit_random_seed)
    
    nstates <- input$hmm_fit_nstate
    
    thedat <- simhmm$state
    
    # Fit model
    mod <- depmixS4::depmix(obs ~ 1, data = data.frame(obs=factor(thedat$observation)), nstates = nstates, family = multinomial("identity"))
    fit.mod <- depmixS4::fit(mod)
    
    # predict the states by estimating the posterior
    est.states <- depmixS4::posterior(fit.mod, type="viterbi")
    est.states <- est.states[,-1]
    est.states <- reshape::melt(as.matrix(est.states))
    colnames(est.states) <- c("time","state","prob")
    
    ### Extract matrices
    fitted.emission <- unlist(fit.mod@response)
    fitted.emission <- t(sapply(fitted.emission, function(x) x@parameters$coefficients))
    
    fitted.trans <- fit.mod@transition
    fitted.trans <- t(sapply(fitted.trans, function(x) x@parameters$coefficients))
    
#    thedat$emissionprob
#    thedat$transprob
    
    list(
      est.states=est.states,
      fitted.emission=fitted.emission,
      fitted.trans=fitted.trans
    )
  }
  
  
  ############ Action: Button pressed to fit HMM
  observeEvent(input$hmm_fit,{
    simhmm$fit <- fitHMM()
  })
  
  ############ Plot of the HMM predicted state
  output$hmm_plot_fit <- renderPlot({
    
    fit <- simhmm$fit
    if(!is.null(fit)){
    
      p1 <- ggplot(fit$est.states, aes(time,prob, color=state)) + 
        geom_line() +
        ylab("State probability")
      
      #Pick out state with highest probability
      eststates <- fit$est.states
      eststates <- merge(eststates,sqldf::sqldf("select time, max(prob) as prob from eststates group by time"))

      p2 <- ggplot(eststates, aes(time,state)) + 
        geom_point() +
        ylab("Most likely state")
      
      #    p1 <- ggplot(thedat, aes(i, emit)) + geom_line() + geom_point()
      #    p2 <- ggplot(thedat, aes(i, state)) + geom_line() + geom_point()
      #    egg::ggarrange(p1,p2, ncol=1)  
      egg::ggarrange(p1,p2, ncol=1)
      
    } else {
      ggplot()
    }
  })
  

  
  
  output$hmm_fit_trans <- renderTable({
    res <- simhmm$fit
    res$fitted.trans
  })
  output$hmm_fit_emit <- renderTable({
    res <- simhmm$fit
    res$fitted.emission
  })
  
  
}







