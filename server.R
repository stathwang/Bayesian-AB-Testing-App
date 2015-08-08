library(shiny)
library(ggplot2)
library(plyr)
library(coda)

shinyServer(function(input, output){
  simul <- reactive({input$nsims})
  output$plot <- renderPlot({
    #posterior for the control group
    a1 <- input$alpha + input$cs
    b1 <- input$beta + input$cf
    
    #posterior for the test group
    a2 <- input$alpha + input$ts
    b2 <- input$beta + input$tf
    
    #beta-binomial simulation for the control group
    rb1 <- repeatable(rbeta, seed = 12345)
    m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
    
    #beta-binomial simulation for the test group
    rb2 <- repeatable(rbeta, seed = 12345)
    m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
    
    #combine into a dataframe
    control <- data.frame(data = m1, tag = "control", stringsAsFactors = FALSE)
    test <- data.frame(data = m2, tag = "test", stringsAsFactors = FALSE)
    total <- rbind(control, test)
    
    #calculate mean of each group
    group_means <- ddply(total, "tag", summarize, mean = mean(data), sd = sd(data))
    
    #control and test histograms
    g <- ggplot(total, aes(data, fill = tag)) + 
      geom_histogram(alpha = 0.5, aes(y = ..density..), position = "identity") + 
      geom_vline(data = group_means, aes(xintercept = mean, colour = tag), linetype = "dashed", size = 1) + 
      scale_fill_discrete(name = "Group", breaks = c("control", "test"), labels = c("Control", "Test")) +
      ylab("Posterior Density") + xlab("Data") + 
      theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17))
    
    if (input$gauss)
    {
      g <- g + stat_density(alpha = 0.3, adjust = input$bw_adjust, trim = TRUE, position = "identity")
      if (input$remove_hist)
      {
        g <- ggplot(total, aes(data, fill = tag)) + 
          stat_density(alpha = 0.3, adjust = input$bw_adjust, trim = FALSE, position = "identity") + 
          geom_vline(data = group_means, aes(xintercept = mean, colour = tag), linetype = "dashed", size = 1) + 
          scale_fill_discrete(name = "Group", breaks = c("control", "test"), labels = c("Control", "Test")) + 
          ylab("Posterior Density") + xlab("Data") +
          theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17))
      }
    }
    
    plot(g)
  })
  
  output$summary1 <- renderPrint({
    if (input$hpdi)
    {
      #posterior for the control group
      a1 <- input$alpha + input$cs
      b1 <- input$beta + input$cf
      rb1 <- repeatable(rbeta, seed = 12345)
      m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
      m1_hpd_int <- as.numeric(HPDinterval(as.mcmc(m1), prob = input$hpdilevel/100))
      print(m1_hpd_int)
    }
  })
  
  output$summary2 <- renderPrint({
    if (input$hpdi)
    {
      #posterior for the test group
      a2 = input$alpha + input$ts
      b2 = input$beta + input$tf
      rb2 <- repeatable(rbeta, seed = 12345)
      m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
      m2_hpd_int <- as.numeric(HPDinterval(as.mcmc(m2), prob = input$hpdilevel/100))
      print(m2_hpd_int)
    }
  })
  
  output$summary3 <- renderPrint({
    if (input$qbpi)
    {
      #posterior for the control group
      a1 <- input$alpha + input$cs
      b1 <- input$beta + input$cf
      rb1 <- repeatable(rbeta, seed = 12345)
      m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
      m1_qbp_int <- as.numeric(quantile(m1, probs = c((1-(input$qbpilevel/100))/2, (1+(input$qbpilevel/100))/2)))
      print(m1_qbp_int)
    }
  })
  
  output$summary4 <- renderPrint({
    if (input$qbpi)
    {
      #posterior for the test group
      a2 <- input$alpha + input$ts
      b2 <- input$beta + input$tf
      rb2 <- repeatable(rbeta, seed = 12345)
      m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
      m2_qbp_int <- as.numeric(quantile(m2, probs = c((1-(input$qbpilevel/100))/2, (1+(input$qbpilevel/100))/2)))
      print(m2_qbp_int)
    }
  })
  
  output$diff <- renderPlot({
    #posterior for the control group
    a1 <- input$alpha + input$cs
    b1 <- input$beta + input$cf
    
    #posterior for the test group
    a2 <- input$alpha + input$ts
    b2 <- input$beta + input$tf
    
    #beta-binomial simulation for the control group
    rb1 <- repeatable(rbeta, seed = 12345)
    m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
    
    #beta-binomial simulation for the test group
    rb2 <- repeatable(rbeta, seed = 12345)
    m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
    
    #test-control difference histogram
    test_control <- data.frame(difference = m2 - m1)
    total_mean <- mean(m2-m1)
    p <- ggplot(test_control, aes(x = difference)) + geom_histogram(alpha = 0.5, aes(y = ..density..), position = "identity") + ylab("Density") + xlab("Difference") + 
      theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12),
            axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17))
    
    if (input$gauss)
    {
      p <- p + stat_density(alpha = 0.3, adjust = input$bw_adjust, trim = TRUE, position = "identity")
      if (input$remove_hist)
      {
        p <- ggplot(test_control, aes(x = difference)) +
          ylab("Posterior Density") + xlab("Difference") + 
          stat_density(alpha = 0.3, adjust = input$bw_adjust, trim = FALSE, position = "identity") + 
          theme(legend.title = element_text(size = 15), legend.text = element_text(size = 12),
                axis.title.x = element_text(size = 17), axis.title.y = element_text(size = 17))
      }
    }
    
    plot(p)
  })
  
  output$summary5 <- renderPrint({
    if (input$hpdi)
    {
      #posterior for the control group
      a1 <- input$alpha + input$cs
      b1 <- input$beta + input$cf
      
      #posterior for the test group
      a2 <- input$alpha + input$ts
      b2 <- input$beta + input$tf
      
      #beta-binomial simulation for the control group
      rb1 <- repeatable(rbeta, seed = 12345)
      m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
      
      #beta-binomial simulation for the test group
      rb2 <- repeatable(rbeta, seed = 12345)
      m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
      
      d <- m2-m1
      test_control_hdp_int <- as.numeric(HPDinterval(as.mcmc(d), prob = input$hpdilevel/100))
      print(test_control_hdp_int)
    }
  })
  
  output$summary6 <- renderPrint({
    if (input$qbpi)
    {
      #posterior for the control group
      a1 = input$alpha + input$cs
      b1 = input$beta + input$cf
      
      #posterior for the test group
      a2 = input$alpha + input$ts
      b2 = input$beta + input$tf
      
      #beta-binomial simulation for the control group
      rb1 <- repeatable(rbeta, seed = 12345)
      m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
      
      #beta-binomial simulation for the test group
      rb2 <- repeatable(rbeta, seed = 12345)
      m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
      
      d <- m2-m1
      test_control_qbp_int <- as.numeric(quantile(d, probs = c((1-(input$qbpilevel/100))/2, (1+(input$qbpilevel/100))/2)))
      print(test_control_qbp_int)
    }
  })
  
  output$rev <- renderPrint({
    #posterior for the control group
    a1 = input$alpha + input$cs
    b1 = input$beta + input$cf
    
    #posterior for the test group
    a2 = input$alpha + input$ts
    b2 = input$beta + input$tf
    
    #beta-binomial simulation for the control group
    rb1 <- repeatable(rbeta, seed = 12345)
    m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
    
    #beta-binomial simulation for the test group
    rb2 <- repeatable(rbeta, seed = 12345)
    m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
    
    #average difference between test and control
    d <- m2-m1
    print(mean(d))
  })
  
  output$bet <- renderPrint({
    #posterior for the control group
    a1 = input$alpha + input$cs
    b1 = input$beta + input$cf
    
    #posterior for the test group
    a2 = input$alpha + input$ts
    b2 = input$beta + input$tf
    
    #beta-binomial simulation for the control group
    rb1 <- repeatable(rbeta, seed = 12345)
    m1 <- rb1(simul(), shape1 = a1, shape2 = b1)
    
    #beta-binomial simulation for the test group
    rb2 <- repeatable(rbeta, seed = 12345)
    m2 <- rb2(simul(), shape1 = a2, shape2 = b2)
    
    #probability that test performs better than control
    d <- m2-m1
    print(mean(d > 0))
  })
})
