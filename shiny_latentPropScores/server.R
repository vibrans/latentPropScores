shinyServer(
  function(input, output, session){
    ## some input variables
    n_m_cov <- reactive(input$n_m_cov)
    n_l_cov <- reactive(input$n_l_cov)
    
    ## specify covariances
    output$cov <- renderUI({
      if(n_m_cov()==1 & n_l_cov()==1){
        numericInput(inputId="cov_z1_xi1", label="Cov(Z\u2081, Xi\u2081)", value=0)
      }else if(n_m_cov()==1 & n_l_cov()==2){
        fluidRow(column(4, numericInput(inputId="cov_z1_xi1", label="Cov(Z\u2081, Xi\u2081)", value=0)),
                 column(4, numericInput(inputId="cov_z1_xi2", label="Cov(Z\u2081, Xi\u2082)", value=0)),
                 column(4, numericInput(inputId="cov_xi1_xi2", label="Cov(Xi\u2081, Xi\u2082)", value=0)))
      }else if(n_m_cov()==2 & n_l_cov()==2){
        tagList(
          fluidRow(column(4, numericInput(inputId="cov_z1_z2", label="Cov(Z\u2081, Z\u2082)", value=0)),
                   column(4, numericInput(inputId="cov_z1_xi1", label="Cov(Z\u2081, Xi\u2081)", value=0)),
                   column(4, numericInput(inputId="cov_z1_xi2", label="Cov(Z\u2081, Xi\u2082)", value=0))),
          fluidRow(column(4, numericInput(inputId="cov_xi1_xi2", label="Cov(Xi\u2081, Xi\u2082)", value=0)),
                   column(4, numericInput(inputId="cov_z2_xi1", label="Cov(Z\u2082, Xi\u2081)", value=0)),
                   column(4, numericInput(inputId="cov_z2_xi2", label="Cov(Z\u2082, Xi\u2082)", value=0))
                   )
        )
      }else if(n_m_cov()==0 & n_l_cov()==2){
        numericInput(inputId="cov_xi1_xi2", label="Cov(Xi\u2081, Xi\u2082)", value=0)
      }
    })
    

    
    output$regression <- renderUI({
      if(n_m_cov() == 1 & n_l_cov() == 1){
        tagList(
          h5("Regression E(Y|Z\u2081, Xi\u2081)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3_000")),
                   column(1, p("+")),
                   column(2, p("\u03B3_001*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_002*Xi\u2081"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, p("+ (")),
                   column(2, p("\u03B3_100")),
                   column(1, p("+")),
                   column(2, p("\u03B3_101*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_102*Xi\u2081")),
                   column(1, p(")*X"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p("SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=0.35, width='100%')))
        )
      }else if(n_m_cov() == 1 & n_l_cov() == 2){
        tagList(
          h5("Regression E(Y|Z\u2081, Xi\u2081, Xi\u2082)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3_000")),
                   column(1, p("+")),
                   column(2, p("\u03B3_001*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_002*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_003*Xi2"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(2, p("+ (\u03B3_100")),
                   column(1, p("+")),
                   column(2, p("\u03B3_101*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_102*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_103*Xi2")),
                   column(1, p(")*X"))),
          fluidRow(column(2, numericInput(inputId="gamma100", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("mean(\u03B6), ")),
                   column(2, p(", sd(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=0.35, width='100%')))
        )
      }else if(n_m_cov() == 2 & n_l_cov() == 2){
        tagList(
          h5("Regression E(Y|Z\u2081, Z\u2082, Xi\u2081, Xi\u2082)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3_000")),
                   column(1, p("+")),
                   column(2, p("\u03B3_001*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_002*Z\u2081"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=0, width='100%'))),
          # second part of baseline function in next row because of layout problems
          fluidRow(column(3),
                   column(1, p("+")),
                   column(2, p("\u03B3_003*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_004*Xi2"))),
          fluidRow(column(4),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma104", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, p("+ (")),
                   column(2, p("\u03B3_100")),
                   column(1, p("+")),
                   column(2, p("\u03B3_101*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_102*Z\u2081"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=0, width='100%'))),
          # part two of effect function because of layout problems
          fluidRow(column(4),
                   column(1, p("+")),
                   column(2, p("\u03B3_103*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_104*Xi2")),
                   column(1, p(")*X"))),
          fluidRow(column(5),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma104", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p(", SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=0.35, width='100%')))
        )
      }else if(n_m_cov() == 0 & n_l_cov() == 2){
        tagList(
          h5("Regression E(Y|Xi\u2081, Xi\u2082)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3_000")),
                   column(1, p("+")),
                   column(2, p("\u03B3_001*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_002*Xi2"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, p("+ (")),
                   column(2, p("\u03B3_100")),
                   column(1, p("+")),
                   column(2, p("\u03B3_101*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_102*Xi\u2082")),
                   column(1, p(")*X"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=0.2, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=0, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=0, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p(", SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=0.35, width='100%')))
        )
      }
    })
    
    ####### update default values depending on user's choice of preconfiguration
    ## update covariances
    observe({
      c <- input$conf
      if(c=="axel"){
        ######## manifest covariate
        # set number of manifest covariate to 1
        updateNumericInput(session, inputId="n_m_cov", value=1)
        # adapt mean and sd of manifest covariate
        updateNumericInput(session, inputId="mean_m_cov1", value=0)
        updateNumericInput(session, inputId="sd_m_cov1", value=1)

        ####### latent covariate
        # set number of latent covariate to 1
        updateNumericInput(session, inputId="n_l_cov", value=1)
        # adapt mean and sd of latent covariate
        updateNumericInput(session, inputId="mean_xi1", value=0)
        updateNumericInput(session, inputId="sd_xi1", value=1)
        # adapt intercepts of indicator 1 to 3 of only latent covariate
        lapply(1:3, function(i, l=c(0, 0.5, 0.3)) updateNumericInput(session, inputId=paste0("intercept_Y", i, "11"),
                                                                  value=l[i]))
        # adapt loadings of indicator 1 to 3 of only latent covariate
        lapply(1:3, function(i, l=c(1, 0.9, 0.8)) updateNumericInput(session, inputId=paste0("loading_Y", i, "11"),
                                                                  value=l[i]))
        # adapt indicators' SDs of only latent covariate
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("sd_e", i, "11"), value=0.3))
        # set covariance to 0
        updateNumericInput(session, inputId="cov_z1_xi1", value=0)
        
        ######## regression
        # BE AWARE: coefficients of manifest variables first and then of latent covariates
        # adapt coefficients for effect function
        lapply(0:4, function(i, l=c(0.5, 0, 0, 0, 0)) updateNumericInput(session, inputId=paste0("gamma10", i),
                                                                         value=l[i+1]))

        # adapt coefficients of regression
        lapply(0:4, function(i, l=c(0.4, 0.6, 0.7, 0, 0)) updateNumericInput(session, inputId=paste0("gamma00", i),
                                                                             value=l[i+1]))

        updateNumericInput(session, inputId="mean_ceta", value=0)
        updateNumericInput(session, inputId="sd_ceta", value=0.7)
        
        # Raykov preconfig
      }else if(c=="raykov"){
        # set number of manifest covariates to 0
        updateNumericInput(session, inputId="n_m_cov", value=0)
        
        # set number of latent covariates to 2
        updateNumericInput(session, inputId="n_l_cov", value=2)
        # adapt mean and sd of latent covariates
        lapply(1:2, function(i) updateNumericInput(session, inputId=paste0("mean_xi", i), value=0))
        lapply(1:2, function(i) updateNumericInput(session, inputId=paste0("sd_xi", i), value=0))
        
        # adapt intercepts of indicator 1 to 3 of both latent covariates (no intercepts)
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("intercept_Y", i, "11"),
                                                   value=0))
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("intercept_Y", i, "12"),
                                                   value=0))
        # adapt loadings of indicators 1 to 3 of both latent covariates
        lapply(1:3, function(i, l=c(2, 2.5, 3)) updateNumericInput(session, inputId=paste0("loading_Y", i, "11"),
                                                                   value=l[(i)]))
        lapply(1:3, function(i, l=c(2.5, 3.5, 4)) updateNumericInput(session, inputId=paste0("loading_Y", i, "12"),
                                                                     value=l[(i)]))
        # adapt indicators' SDs of both latent covariates
        lapply(1:3, function(i, l=c(0.22, 0.25, 0.27)) updateNumericInput(session, inputId=paste0("sd_e", i, "11"),
                                                                          value=l[i]))
        lapply(1:3, function(i, l=c(0.2, 0.3, 0.35)) updateNumericInput(session, inputId=paste0("sd_e", i, "12"),
                                                                        value=l[i]))
        # set covariance to 0.5
        updateNumericInput(session, inputId="cov_xi1_xi2", value=0.5)
        ######## regression
        # BE AWARE: coefficients of manifest variables first and then of latent covariates
        # adapt coefficients for effect function
        lapply(0:4, function(i, l=c(0.15, 0, 0, 0, 0)) updateNumericInput(session, inputId=paste0("gamma10", i),
                                                                          value=l[i+1]))

        # adapt coefficients of regression
        lapply(0:4, function(i, l=c(0, 0.5, 0.7, 0, 0)) updateNumericInput(session, inputId=paste0("gamma00", i),
                                                                           value=l[i+1]))
        updateNumericInput(session, inputId="mean_ceta", value=0)
        updateNumericInput(session, inputId="sd_ceta", value=0.3)

      }else if(c=="standard"){
        # set everything back
        updateNumericInput(session, inputId="n_m_cov", value=0)
        
        # set number of latent covariates to 2
        updateNumericInput(session, inputId="n_l_cov", value=1)
        # adapt mean and sd of latent covariates
        lapply(1:2, function(i) updateNumericInput(session, inputId=paste0("mean_xi", i), value=0))
        lapply(1:2, function(i) updateNumericInput(session, inputId=paste0("sd_xi", i), value=0))
        
        # adapt intercepts of indicator 1 to 3 of both latent covariates (no intercepts)
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("intercept_Y", i, "11"),
                                                   value=0))
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("intercept_Y", i, "12"),
                                                   value=0))
        # adapt loadings of indicators 1 to 3 of both latent covariates
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("loading_Y", i, "11"),
                                                                   value=1))
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("loading_Y", i, "12"),
                                                                     value=1))
        # adapt indicators' SDs of both latent covariates
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("sd_e", i, "11"),
                                                                          value=1))
        lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("sd_e", i, "12"),
                                                                        value=1))
        # set covariance to 0
        updateNumericInput(session, inputId="cov_xi1_xi2", value=0)
        ######## regression
        # BE AWARE: coefficients of manifest variables first and then of latent covariates
        # adapt coefficients for effect function
        lapply(0:4, function(i, l=c(0.15, 0, 0, 0, 0)) updateNumericInput(session, inputId=paste0("gamma10", i),
                                                                          value=l[i+1]))
        
        # adapt coefficients of regression
        lapply(0:4, function(i, l=c(0, 0.5, 0.7, 0, 0)) updateNumericInput(session, inputId=paste0("gamma00", i),
                                                                           value=l[i+1]))
        updateNumericInput(session, inputId="mean_ceta", value=0)
        updateNumericInput(session, inputId="sd_ceta", value=0.3)
      }
    })
##############################################################################################################
    # data generation
    
    N <- reactive(input$N)
    n_m_cov <- reactive(input$n_m_cov)
    n_l_cov <- reactive(input$n_l_cov)

    ### Simulation of normally distributed independent continuous variables
    data <- reactive({
      if(n_m_cov()==0 & n_l_cov()==2){
      # means
      mean_xi1 <- reactive(input$mean_xi1)
      mean_xi2 <- reactive(input$mean_xi2)
      # compute variances
      var_xi1 <- reactive(input$sd_xi1^2)
      var_xi2 <- reactive(input$sd_xi2^2)
      # get covariances
      cov_xi1_xi2 <- reactive(input$cov_xi1_xi2)
      # create dataframe
      mvrnorm(n=N, mu=c(mean_xi1(), mean_xi2),
              Sigma=matrix(c(var_xi1(), cov_xi1_xi2(), cov_xi1_xi2(), var_xi2()), nrow=2), empirical=TRUE)
      
    }else if(n_m_cov()==1 & n_l_cov()==1){
      # means
      mean_z1 <- reactive(input$mean_z1)
      mean_xi1 <- reactive(input$mean_xi1)
      # compute variances
      var_z1 <- reactive(input$sd_z1^2)
      var_xi1 <- reactive(input$sd_xi1^2)
      # get covariances
      cov_z1_xi1 <- reactive(input$cov_z1_xi1)
      # create dataframe
      mvrnorm(n=N, mu=c(mean_z1(), mean_xi1()),
              Sigma=matrix(c(var_z1, cov_z1_xi1(), cov_z1_xi1, var_xi1), nrow=2), empirical=TRUE)
      
    }else if(n_m_cov()==1 & n_l_cov()==2){
      # means
      mean_z1 <- reactive(input$mean_z1)
      mean_xi1 <- reactive(input$mean_xi1)
      mean_xi2 <- reactive(input$mean_xi2)
      # compute variances
      var_z1 <- reactive(input$sd_z1^2)
      var_xi1 <- reactive(input$sd_xi1^2)
      var_xi2 <- reactive(input$sd_xi2^2)
      # get covariances
      cov_z1_xi1 <- reactive(input$cov_z1_xi1)
      cov_z1_xi2 <- reactive(input$cov_z1_xi2)
      cov_xi1_xi2 <- reactive(input$cov_xi1_xi2)
      # create dataframe
      mvrnorm(n=N, mu=c(mean_z1(), mean_xi1(), mean_xi2()),
              Sigma=matrix(c(var_z1(), cov_z1_xi1(), cov_z1_xi2(),
                             cov_z1_xi1(), var_xi1(), cov_xi1_xi2(),
                             cov_z1_xi2(), cov_xi1_xi2(), var_xi2), nrow=3), empirical=TRUE)
      
    }else if(n_m_cov()==2 & n_l_cov()==1){
      # means
      mean_z1 <- reactive(input$mean_z1)
      mean_z2 <- reactive(input$mean_z2)
      mean_xi1 <- reactive(input$mean_xi1)
      # compute variances
      var_z1 <- reactive(input$sd_z1^2)
      var_z2 <- reactive(input$sd_z2^2)
      var_xi1 <- reactive(input$sd_xi1^2)
      # get covariances
      cov_z1_z2 <- reactive(input$cov_z1_z2)
      cov_z1_xi1 <- reactive(input$cov_z1_xi1)
      cov_z2_xi1 <- reactive(input$cov_z2_xi1)
      # create dataframe
      mvrnorm(n=N, mu=c(mean_z1(), mean_z2(), mean_xi1()),
              Sigma=matrix(c(var_z1(), cov_z1_z2(), cov_z1_xi1(),
                             cov_z1_z2(), var_z2(), cov_z2_xi1(),
                             cov_z1_xi1(), cov_z2_xi1(), var_xi1()), nrow=3), empirical=TRUE)
      
    }else if(n_m_cov()==2 & n_l_cov()==2){
      # means
      mean_z1 <- reactive(input$mean_z1)
      mean_z2 <- reactive(input$mean_z2)
      mean_xi1 <- reactive(input$mean_xi1)
      mean_xi2 <- reactive(input$mean_xi2)
      # compute variances
      var_z1 <- reactive(input$sd_z1^2)
      var_z2 <- reactive(input$sd_z2^2)
      var_xi1 <- reactive(input$sd_xi1^2)
      var_xi2 <- reactive(input$sd_xi2^2)
      # get covariances
      cov_z1_xi1 <- reactive(input$cov_z1_xi1)
      cov_z1_xi2 <- reactive(input$cov_z1_xi2)
      cov_xi1_xi2 <- reactive(input$cov_xi1_xi2)
      # create dataframe
      mvrnorm(n=N, mu=c(mean_z1(), mean_z2(), mean_xi1(), mean_xi2),
              Sigma=matrix(c(var_z1(), cov_z1_z2(), cov_z1_xi1(), cov_z1_xi2(),
                             cov_z1_z2(), var_z2(), cov_z2_xi1(), cov_z2_xi2(),
                             cov_z1_xi1(), cov_z2_xi1(), var_xi1(), cov_xi1_xi2(),
                             cov_z1_xi2(), cov_z2_xi2(), cov_xi1_xi2(), var_xi2()), nrow=4), empirical=TRUE)
    }
    })
    #Xi1 = data[, 1]  
    #Xi2 = data[, 2]  
    #output$table <- renderTable("t", head(data()))
    
    
    # 3. New idea: using lavaan to estimate treatment effect with latent PSs
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  }
)