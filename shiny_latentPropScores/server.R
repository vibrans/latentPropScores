shinyServer(
  function(input, output, session){
    ## some input variables
    n_m_cov <- reactive(input$n_m_cov)
    n_l_cov <- reactive(input$n_l_cov)
    
    
    ## layout of regression for UI
    output$regression <- renderUI({
      if(n_m_cov() == 1 & n_l_cov() == 1){
        tagList(
          h5("Regression E(Y|Z\u2081, Xi\u2081)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3\u2080\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2082*Xi\u2081"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, p("+ (")),
                   column(2, p("\u03B3\u2081\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2082*Xi\u2081")),
                   column(1, p(")*X"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p("SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=v$v_sd_ceta, width='100%')))
        )
      }else if(n_m_cov() == 1 & n_l_cov() == 2){
        tagList(
          h5("Regression E(Y|Z\u2081, Xi\u2081, Xi\u2082)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3\u2080\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_\u2080\u2080\u2082*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2083*Xi\u2082"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma003", label=NULL, value=v$v_gamma003, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(2, p("+ (\u03B3\u2081\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2082*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2083*Xi\u2082")),
                   column(1, p(")*X"))),
          fluidRow(column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=v$v_gamma103, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p(", SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=v$v_sd_ceta, width='100%')))
        )
      }else if(n_m_cov() == 2 & n_l_cov() == 2){
        tagList(
          h5("Regression E(Y|Z\u2081, Z\u2082, Xi\u2081, Xi\u2082)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3\u2080\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2082*Z\u2081"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%'))),
          # second part of baseline function in next row because of layout problems
          fluidRow(column(3),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2083*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2084*Xi\u2082"))),
          fluidRow(column(4),
                   column(2, numericInput(inputId="gamma003", label=NULL, value=v$v_gamma003, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma004", label=NULL, value=v$v_gamma004, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, p("+ (")),
                   column(2, p("\u03B3\u2081\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2082*Z\u2081"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%'))),
          # part two of effect function because of layout problems
          fluidRow(column(4),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2083*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2084*Xi2")),
                   column(1, p(")*X"))),
          fluidRow(column(5),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=v$v_gamma103, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma104", label=NULL, value=v$v_gamma104, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p(", SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=v$v_sd_ceta, width='100%')))
        )
      }else if(n_m_cov() == 2 & n_l_cov() == 1){
        tagList(
          h5("Regression E(Y|Z\u2081, Z\u2082, Xi\u2081)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3\u2080\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3_\u2080\u2080\u2082*Z\u2082")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2083*Xi\u2081"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma003", label=NULL, value=v$v_gamma003, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(2, p("+ (\u03B3\u2081\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2081*Z\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2082*Z\u2082")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2083*Xi\u2081")),
                   column(1, p(")*X"))),
          fluidRow(column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=v$v_gamma103, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p(", SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=v$v_sd_ceta, width='100%')))
        )
      }else if(n_m_cov() == 0 & n_l_cov() == 2){
        tagList(
          h5("Regression E(Y|Xi\u2081, Xi\u2082)"),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, p("\u03B3\u2080\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2081*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2080\u2080\u2082*Xi2"))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, p("+ (")),
                   column(2, p("\u03B3\u2081\u2080\u2080")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2081*Xi\u2081")),
                   column(1, p("+")),
                   column(2, p("\u03B3\u2081\u2080\u2082*Xi\u2082")),
                   column(1, p(")*X"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, p("+ (")),
                   column(2, p("Mean(\u03B6), ")),
                   column(2, p(", SD(\u03B6)")),
                   column(1, p(")"))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="mean_ceta", label=NULL, value=0, width='100%')),
                   column(2, numericInput(inputId="sd_ceta", label=NULL, value=v$v_sd_ceta, width='100%')))
        )
      }
    })
    
    ## specify covariances
    output$cov <- renderUI({
      if(n_m_cov()==1 & n_l_cov()==1){
        numericInput(inputId="cov_z1_xi1", label="Cov(Z\u2081, Xi\u2081)", value=v$v_cov_z1_xi1)
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
      }else if(n_m_cov()==2 & n_l_cov()==1){
        tagList(
          fluidRow(column(4, numericInput(inputId="cov_z1_z2", label="Cov(Z\u2081, Z\u2082)", value=0)),
                   column(4, numericInput(inputId="cov_z1_xi1", label="Cov(Z\u2081, Xi\u2081)", value=0)),
                   column(4, numericInput(inputId="cov_z2_xi1", label="Cov(Z\u2082, Xi\u2081)", value=0)))
        )
      }else if(n_m_cov()==0 & n_l_cov()==2){
        numericInput(inputId="cov_xi1_xi2", label="Cov(Xi\u2081, Xi\u2082)", value=v$v_cov_xi1_xi2)
      }
    })
    
    
    ####################### default values depending on choice of preconfiguration ########################
     v <- reactiveValues()
     observeEvent(input$conf, {
       c <- input$conf
       if(c=="standard"){
         ######## manifest covariate
         #!# problem: two strategies for updating the default values: for ui layout specifications with *update
         #!# luckily possible because no circuits are buildt
         #!# for layout specifications on server no updating but rather differing "reactive values"
         #!# => probably not very elegently

         # set number of manifest covariate to 1
         updateNumericInput(session, inputId="n_m_cov", value=1)
         # adapt mean and sd of manifest covariate
         updateNumericInput(session, inputId="mean_z1", value=0)
         updateNumericInput(session, inputId="sd_z1", value=1)
         ####### latent covariate
         # set number of latent covariate to 1
         updateNumericInput(session, inputId="n_l_cov", value=1)
         # adapt mean and sd of latent covariate
         updateNumericInput(session, inputId="mean_xi1", value=0)
         updateNumericInput(session, inputId="sd_xi1", value=1)
         # indentifier of indicator variables: Yitz
         # i ... index of indicator
         # t ... time point
         # z ... index of latent covariate
         # adapt intercepts of indicator 1 to 3 of only latent covariate
         lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("intercept_Y", i, "11"),
                                                                       value=0))
         # adapt loadings of indicator 1 to 3 of only latent covariate
         lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("loading_Y", i, "11"),
                                                                       value=1))
         # adapt indicators' SDs of only latent covariate
         lapply(1:3, function(i) updateNumericInput(session, inputId=paste0("sd_e", i, "11"), value=0.3))
         # set covariance to 0
         v$v_cov_z1_xi1 <- 0

         ######## regression
         # BE AWARE: coefficients of manifest covariates first and then of latent covariates
         # adapt coefficients of baseline function g0
         #!# problem: lapply(0:2, function(i, l=c(0, 1, 1)) v[[paste0("v$v_gamma00", i)]] <- l[i+1])
         #!# -> 1. get("v$v_gamma000") necessary to get value / v$v_gamma000 not working;
         #!# assignment of vectorelement l[i] not possible (don't know why)
         v$v_gamma000 <- 0
         v$v_gamma001 <- 1
         v$v_gamma002 <- 1
         v$v_gamma003 <- 1
         v$v_gamma004 <- 1
         # adapt coefficients for effect function g1 (treatmenteffect 0.15; no interaction cov*X)
         v$v_gamma100 <- 0.2
         v$v_gamma101 <- 0
         v$v_gamma102 <- 0
         v$v_gamma103 <- 0
         v$v_gamma104 <- 0
         # adapt mean and standard deviance
         v$v_mean_ceta <-  0
         v$v_sd_ceta <-  0.35

       }else if(c=="axel"){
         #!# problem: redundant code
         ######## manifest covariate
         # set number of manifest covariate to 1
         updateNumericInput(session, inputId="n_m_cov", value=1)
         # adapt mean and sd of manifest covariate
         updateNumericInput(session, inputId="mean_z1", value=0)
         updateNumericInput(session, inputId="sd_z1", value=1)
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
         v$v_cov_z1_xi1 <- 0

        ######## regression
        # BE AWARE: coefficients of manifest covariates first and then of latent covariates
        # adapt coefficients of baseline function g0
         v$v_gamma000 <- 0
         v$v_gamma001 <- 0.6
         v$v_gamma002 <- 0.7
        # adapt coefficients for effect function g1
         v$v_gamma100 <- 0.5
         v$v_gamma101 <- 0
         v$v_gamma102 <- 0
        # adapt mean and standard deviance
         v$v_mean_ceta <- 0
         v$v_sd_ceta <- 0.7

       }else if(c=="raykov"){
        ######## manifest covariate
        # set number of manifest covariate to 1
         updateNumericInput(session, inputId="n_m_cov", value=0)
        ####### latent covariate
        # set number of latent covariate to 2
        updateNumericInput(session, inputId="n_l_cov", value=2)
        # adapt mean and sd of latent covariates
        v$v_mean_xi1 <- 0
        v$v_sd_xi1 <- 1
        v$v_mean_xi2 <- 0
        v$v_sd_xi2 <- 1
        # adapt intercepts of indicators 1 to 3 of both latent covariates (no intercepts)
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
         v$v_cov_xi1_xi2 <- 0.5

        ######## regression
        # BE AWARE: coefficients of manifest covariates first and then of latent covariates
        # adapt coefficients of baseline function g0
         v$v_gamma000 <- 0
         v$v_gamma001 <- 0.5
         v$v_gamma002 <- 0.7
        # adapt coefficients for effect function g1
         v$v_gamma100 <- 0.15
         v$v_gamma101 <- 0
         v$v_gamma102 <- 0
        # adapt mean and standard deviance
        v$v_mean_ceta <- 0
        v$v_sd_ceta <- 0.3
       }
     })
    
##############################################################################################################
    # data generation
    
    N <- reactive(input$N)

    ### Simulation of normally distributed independent continuous variables
     #!# problem: please create nicer error message for semidefiniteness
    df <- reactive({
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
      setNames(data.frame(mvrnorm(n=N(), mu=c(mean_xi1(), mean_xi2()),
              Sigma=matrix(c(var_xi1(), cov_xi1_xi2(), cov_xi1_xi2(), var_xi2()), nrow=2), empirical=TRUE)),
              c("Xi1", "Xi2"))

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
      setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_xi1()),
              Sigma=matrix(c(var_z1(), cov_z1_xi1(), cov_z1_xi1(), var_xi1()), nrow=2), empirical=TRUE)),
              c("Z1", "Xi1"))


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
      setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_xi1(), mean_xi2()),
              Sigma=matrix(c(var_z1(), cov_z1_xi1(), cov_z1_xi2(),
                             cov_z1_xi1(), var_xi1(), cov_xi1_xi2(),
                             cov_z1_xi2(), cov_xi1_xi2(), var_xi2()), nrow=3), empirical=TRUE)),
              c("Z1", "Xi1", "Xi2"))

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
      setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_z2(), mean_xi1()),
              Sigma=matrix(c(var_z1(), cov_z1_z2(), cov_z1_xi1(),
                             cov_z1_z2(), var_z2(), cov_z2_xi1(),
                             cov_z1_xi1(), cov_z2_xi1(), var_xi1()), nrow=3), empirical=TRUE)),
              c("Z1", "Z2", "Xi1"))

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
      cov_z1_z2 <- reactive(input$cov_z1_z2)
      cov_z2_xi1 <- reactive(input$cov_z2_xi1)
      cov_z2_xi2 <- reactive(input$cov_z2_xi2)
      cov_xi1_xi2 <- reactive(input$cov_xi1_xi2)
      # create dataframe
      setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_z2(), mean_xi1(), mean_xi2()),
              Sigma=matrix(c(var_z1(), cov_z1_z2(), cov_z1_xi1(), cov_z1_xi2(),
                             cov_z1_z2(), var_z2(), cov_z2_xi1(), cov_z2_xi2(),
                             cov_z1_xi1(), cov_z2_xi1(), var_xi1(), cov_xi1_xi2(),
                             cov_z1_xi2(), cov_z2_xi2(), cov_xi1_xi2(), var_xi2()), nrow=4), empirical=TRUE)),
              c("Z1", "Z2", "Xi1", "Xi2"))
    }
      # if(n_l_cov()>0){
      #   df$Y111 <- input$loading_Y111*df$Xi1 + rnorm(N(), 0, input$sd_e111)
      #   df$Y211 <- rnorm(N(), 0, input$sd_e211)
      #   df$Y311 <- rnorm(N(), 0, input$sd_e311)
      # }
      # if(n_l_cov()>1){
      #   df$Y112 <- rnorm(N(), 0, input$sd_e112)
      #   df$Y212 <- rnorm(N(), 0, input$sd_e212)
      #   df$Y312 <- rnorm(N(), 0, input$sd_e312)
      # }
    })

    ########## indicator variables for latent covariates
      # data <- reactive({
      #   if(n_l_cov()>0){
      #     df()$Y111 <- input$loading_Y111*df()$Xi1 + rnorm(n, 0, input$sd_e111)
      #     df()$Y211 <- rnorm(n, 0, input$sd_e211)
      #     df()$Y311 <- rnorm(n, 0, input$sd_e311)
      #   }
      #   if(n_l_cov()>1){
      #     df()$Y112 <- rnorm(n, 0, input$sd_e112)
      #     df()$Y212 <- rnorm(n, 0, input$sd_e212)
      #     df()$Y312 <- rnorm(n, 0, input$sd_e312)
      #   }
      # })
      
      

    
    
    
    

    
    
    # Messung von Xi1 durch messfehlerbehaftete Kovariaten
    # Z11 <- 2*Xi1 + ceta11
    # Z12 <- 2.5*Xi1 + ceta12
    # Z13 <- 3*Xi1 + ceta13
    # 
    # # Messung von Xi2 durch messfehlerbehaftete Kovariaten
    # Z21 <- 2.5*Xi2 + ceta21
    # Z22 <- 3.5*Xi2 + ceta22
    # Z23 <- 4*Xi2 + ceta23
    

    ############################################ Raykov's idea ###############################################
    
    
    ################################# proved EffectLiteR approach ############################################
    
    
    
    ################################# new latent Propensity Score approach ###################################
    ## using lavaan to estimate treatment effect with latent PSs

    
    
    
    
    
    
    
    output$t <- renderTable(
      head(df())
    )
    
    
    
    
    
    
    
    
  }
)