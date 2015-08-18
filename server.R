library(shiny)

# Size of samples
n.obs <- 20 # unchanged during run

#options(shiny.error = browser)

shinyServer(
  
  function(input, output) {

    means <- reactive({
      set.seed(1234)
      n.sample <- as.numeric(input$sample)
      switch(
        input$dist,
        'Normal'=apply(
          matrix(
            rnorm(n.sample*n.obs, mean=as.numeric(input$mean), sd=as.numeric(input$sd)),
            nrow=n.obs, ncol=n.sample
          ), 2, mean),
        'Exponential'=apply(
          matrix(
            rexp(n.sample*n.obs, rate=as.numeric(input$rate)),
            nrow=n.obs, ncol=n.sample
          ), 2, mean),
        'Binomial'=apply(
          matrix(
            rbinom(n.sample*n.obs, size=as.numeric(input$size), prob=as.numeric(input$prob)),
            nrow=n.obs, ncol=n.sample
          ), 2, mean)
      )
     })
    
    output$means <- renderPlot({
      hist(means(), main='Histogram of means of draws', xlab='', prob=TRUE)
      true.mean=switch(
        input$dist, 
        'Normal'=as.numeric(input$mean),
        'Exponential'=1/as.numeric(input$rate),
        'Binomial'=as.numeric(input$size)*as.numeric(input$prob)
      )
      true.sd=switch(
        input$dist,
        'Normal'=as.numeric(input$sd),
        'Exponential'=1/as.numeric(input$rate),
        'Binomial'=sqrt(as.numeric(input$size)*as.numeric(input$prob)*(1-as.numeric(input$prob)))
      )/sqrt(n.obs)
      curve(dnorm(x, mean=true.mean, sd=true.sd), 
            col='red', lty=1, lwd=2, add=TRUE)
    })
    
    output$text0 <- renderText({
      sprintf('Distribution: %s; size of samples: %d', input$dist, as.numeric(input$sample))
    })
    
    output$text1 <- renderText({
      switch(
        input$dist,
        'Normal' = sprintf('Mean: %.2f; standard error: %.2f', as.numeric(input$mean), as.numeric(input$sd)),
        'Exponential' = sprintf('rate of decay: %.2f', as.numeric(input$rate)),
        'Binomial' = sprintf('Number of experiments: %d; probability of event: %.2f', as.numeric(input$size), as.numeric(input$prob))
      )
      
    })
    
    output$text2 <- renderText({
        switch(
          input$dist,
          'Normal' = sprintf('The mean is distributed as a normal with mean %.2f and standard error %.2f.', as.numeric(input$mean), as.numeric(input$sd)/sqrt(n.obs)),
          'Exponential' = sprintf('The mean is distributed as a normal with mean and standard error %.2f (both).', 1.0/as.numeric(input$rate)),
          'Binomial' = sprintf('The mean is distributed as a normal with mean %.2f and standard error %.2f.', 
                               as.numeric(input$size)*as.numeric(input$prob),
                               as.numeric(input$size)*as.numeric(input$prob)*(1-as.numeric(input$prob)))
        )
        
    })
    
  }

)
