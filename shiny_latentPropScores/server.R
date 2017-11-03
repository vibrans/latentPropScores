shinyServer(
  function(input, output, session){
    ## some input variables
    n_m_cov <- reactive(input$n_m_cov)
    n_l_cov <- reactive(input$n_l_cov)
    
    ## layout for tabPanel conditional treatment probability
    output$PrX <- renderUI({
      if(n_m_cov()==1 & n_l_cov()==1){
        tagList(
          withMathJax(
            helpText('$$ P(X|Z1, Xi1) = \\frac{exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 \\xi_1)}
                                        {1+exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 \\xi_1)}\\!$$')),
          fluidRow(column(2),
                   column(2, numericInput(inputId="alpha0", label=div('$$\\alpha_0$$'), value=v$v_alpha0)),
                   column(2, numericInput(inputId="alpha1", label=div('$$\\alpha_1$$'), value=v$v_alpha1)),
                   column(2, numericInput(inputId="alpha2", label=div('$$\\alpha_2$$'), value=v$v_alpha2)))
                   
        )
      }else if(n_m_cov()==1 & n_l_cov()==2){
        tagList(
          withMathJax('$$ P(X|Z1, Xi1, Xi2) = \\frac{exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 \\xi_1 + \\alpha_3 \\xi_2)}
                     {1+exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 \\xi_1 + \\alpha_3 \\xi_2)}\\!$$'),
          fluidRow(column(2),
                   column(2, numericInput(inputId="alpha0", label=div('$$\\alpha_0$$'), value=v$v_alpha0)),
                   column(2, numericInput(inputId="alpha1", label=div('$$\\alpha_1$$'), value=v$v_alpha1)),
                   column(2, numericInput(inputId="alpha2", label=div('$$\\alpha_2$$'), value=v$v_alpha2)),
                   column(2, numericInput(inputId="alpha3", label=div('$$\\alpha_3$$'), value=v$v_alpha3)))
        )
      }else if(n_m_cov()==2 & n_l_cov()==2){
        tagList(
          withMathJax(
            helpText('$$ P(X|Z1, Z2, Xi1, Xi2) = \\frac{exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 Z_2 + \\alpha_3 \\xi_1 + \\alpha_4 \\xi_2)}
                     {1+exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 Z_2 +\\alpha_3 \\xi_1 + \\alpha_4 \\xi_2)}\\!$$')),
          fluidRow(column(1),
                   column(2, numericInput(inputId="alpha0", label=div('$$\\alpha_0$$'), value=v$v_alpha0)),
                   column(2, numericInput(inputId="alpha1", label=div('$$\\alpha_1$$'), value=v$v_alpha1)),
                   column(2, numericInput(inputId="alpha2", label=div('$$\\alpha_2$$'), value=v$v_alpha2)),
                   column(2, numericInput(inputId="alpha3", label=div('$$\\alpha_3$$'), value=v$v_alpha3)),
                   column(2, numericInput(inputId="alpha4", label=div('$$\\alpha_4$$'), value=v$v_alpha4)))
        )
      }else if(n_m_cov()==2 & n_l_cov()==1){
        tagList(
          withMathJax(
            helpText('$$ P(X|Z1, Z2, Xi1) = \\frac{exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 Z_2 + \\alpha_3 \\xi_1)}
                     {1+exp(\\alpha_0 + \\alpha_1 Z_1 + \\alpha_2 Z_2 + \\alpha_3 \\xi_2)}\\!$$')),
          fluidRow(column(2),
                   column(2, numericInput(inputId="alpha0", label=div('$$\\alpha_0$$'), value=v$v_alpha0)),
                   column(2, numericInput(inputId="alpha1", label=div('$$\\alpha_1$$'), value=v$v_alpha1)),
                   column(2, numericInput(inputId="alpha2", label=div('$$\\alpha_2$$'), value=v$v_alpha2)),
                   column(2, numericInput(inputId="alpha3", label=div('$$\\alpha_3$$'), value=v$v_alpha3)))
        )
      }else if(n_m_cov()==0 & n_l_cov()==2){
        tagList(
          withMathJax(
            helpText('$$ P(X|Xi1, Xi2) = \\frac{exp(\\alpha_0 + \\alpha_1 \\xi_1 + \\alpha_2 \\xi_2)}
                     {1+exp(\\alpha_0 + \\alpha_1 \\xi_1 + \\alpha_2 \\xi_2)}\\!$$')),
          fluidRow(column(1),
                   column(2, numericInput(inputId="alpha0", label=div('$$\\alpha_0$$'), value=v$v_alpha0)),
                   column(1),
                   column(2, numericInput(inputId="alpha1", label=div('$$\\alpha_1$$'), value=v$v_alpha1)),
                   column(1),
                   column(2, numericInput(inputId="alpha2", label=div('$$\\alpha_2$$'), value=v$v_alpha2)))
        )

      }
    })
    # leads to validation of inputs from tab 'PrX' even if user does not open the tab
    outputOptions(output, "PrX", suspendWhenHidden = FALSE)
    
    
    ## layout of tabPanel regression
    output$regression <- renderUI({
      if(n_m_cov() == 1 & n_l_cov() == 1){
        tagList(
          h5('Regression E(Y|Z\u2081, Xi\u2081)'),
          p("Y ="),
          br(),
          # baseline function g0 (regression in group X=0)
          fluidRow(column(2, withMathJax('$$\\gamma_{000}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{001}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{002}\\xi_1$$'))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$\\gamma_{100}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{101}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{102}\\xi_1$$')),
                   column(1, div('$$)*X$$'))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$Mean(\\zeta), $$')),
                   column(2, div('$$SD(\\zeta)$$')),
                   column(1, div('$$)$$'))),
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
          fluidRow(column(2, withMathJax('$$\\gamma_{000}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{001}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{002}\\xi1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{003}\\xi_2$$'))),
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
          fluidRow(column(2, div('$$+ (\\gamma_{100}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{101}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{102}\\xi_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{103}\\xi_2$$')),
                   column(1, div('$$)*X$$'))),
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
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$Mean(\\zeta), $$')),
                   column(2, div('$$SD(\\zeta), $$')),
                   column(1, div('$$)$$'))),
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
          fluidRow(column(2, withMathJax('$$\\gamma_{000}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{001}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{002}Z_2$$'))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%'))),
          # second part of baseline function in next row because of layout problems
          fluidRow(column(3),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{003}\\xi_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{004}\\xi_2$$'))),
          fluidRow(column(4),
                   column(2, numericInput(inputId="gamma003", label=NULL, value=v$v_gamma003, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma004", label=NULL, value=v$v_gamma004, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$+ (\\gamma_{100}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{101}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{102}Z_2$$'))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%'))),
          # part two of effect function because of layout problems
          fluidRow(column(4),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{103}\\xi_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{104}\\xi_2$$')),
                   column(1, div('$$)X$$'))),
          fluidRow(column(5),
                   column(2, numericInput(inputId="gamma103", label=NULL, value=v$v_gamma103, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma104", label=NULL, value=v$v_gamma104, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$Mean(\\zeta), $$')),
                   column(2, div('$$SD(\\zeta)$$')),
                   column(1, div('$$)$$'))),
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
          fluidRow(column(2, withMathJax('$$\\gamma_{000}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{001}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{002}Z_2$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{003}\\xi_1$$'))),
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
          fluidRow(column(2, div('$$+ (\\gamma_{100}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{101}Z_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{102}Z_2$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{103}*Xi_1$$')),
                   column(1, div('$$)*X$$'))),
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
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$Mean(\\zeta), $$')),
                   column(2, div('$$SD(\\zeta)$$')),
                   column(1, div('$$)$$'))),
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
          fluidRow(column(2, withMathJax('$$\\gamma_{000}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{001}\\xi_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{002}\\xi_2$$'))),
          fluidRow(column(2, numericInput(inputId="gamma000", label=NULL, value=v$v_gamma000, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma001", label=NULL, value=v$v_gamma001, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma002", label=NULL, value=v$v_gamma002, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # effect function g1 (difference to group X=0)
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$\\gamma_{100}$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{101}\\xi_1$$')),
                   column(1, div('$$+$$')),
                   column(2, div('$$\\gamma_{102}\\xi_2$$')),
                   column(1, div('$$)*X$$'))),
          fluidRow(column(1),
                   column(2, numericInput(inputId="gamma100", label=NULL, value=v$v_gamma100, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma101", label=NULL, value=v$v_gamma101, width='100%')),
                   column(1),
                   column(2, numericInput(inputId="gamma102", label=NULL, value=v$v_gamma102, width='100%'))),
          tags$hr(style="border-color: purple;"),
          br(),
          # residual
          fluidRow(column(1, div('$$+ ($$')),
                   column(2, div('$$Mean(\\zeta), $$')),
                   column(2, div('$$SD(\\zeta)$$')),
                   column(1, div('$$)$$'))),
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
         ########## conditional treatment probability
         v$v_alpha0 <- 0
         v$v_alpha1 <- 1
         v$v_alpha2 <- 1
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
         
         ########## conditional treatment probability
         v$v_alpha0 <- 0
         v$v_alpha1 <- 1
         v$v_alpha2 <- 1
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
         ########## conditional treatment probability
         v$v_alpha0 <- 0
         v$v_alpha1 <- 0.4
         v$v_alpha2 <- 0.6
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
    link <- reactive(input$link)

    ### Simulation of normally distributed independent continuous variables
     #!# problem: please create nicer error message for semidefiniteness
    df1 <- reactive({
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
        df <- setNames(data.frame(mvrnorm(n=N(), mu=c(mean_xi1(), mean_xi2()),
                Sigma=matrix(c(var_xi1(), cov_xi1_xi2(), cov_xi1_xi2(), var_xi2()), nrow=2), empirical=TRUE)),
                c("Xi1", "Xi2"))
        logit <- input$alpha0 + input$alpha1*df$Xi1 + input$alpha2*df$Xi2
        df$PrX <- exp(logit)/(1+exp(logit))
        if(link()=="probit"){
          df$X <- rbinom(N(), 1, rnorm(logit))
        }else if(link()=="logit"){
          df$X <- rbinom(N(), 1, df$PrX)
        }
        return(df)

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
      df <- setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_xi1()),
              Sigma=matrix(c(var_z1(), cov_z1_xi1(), cov_z1_xi1(), var_xi1()), nrow=2), empirical=TRUE)),
              c("Z1", "Xi1"))
      a <- reactive(input$alpha0)
      print(a())
      df$logit <- input$alpha0 + input$alpha1*df$Z1 + input$alpha2*df$Xi2
      print(head(df))
      # df$PrX <- exp(logit)/(1+exp(logit))
      # if(link()=="probit"){
      #   df$X <- rbinom(N(), 1, rnorm(logit))
      # }else if(link()=="logit"){
      #   df$X <- rbinom(N(), 1, df$PrX)
      # }
      # return(df)


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
      df <- setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_xi1(), mean_xi2()),
              Sigma=matrix(c(var_z1(), cov_z1_xi1(), cov_z1_xi2(),
                             cov_z1_xi1(), var_xi1(), cov_xi1_xi2(),
                             cov_z1_xi2(), cov_xi1_xi2(), var_xi2()), nrow=3), empirical=TRUE)),
              c("Z1", "Xi1", "Xi2"))
      logit <- input$alpha0 + input$alpha1*df$Z1 + input$alpha2*df$Xi1 + input$alpha3*df$Xi2
      df$PrX <- exp(logit)/(1+exp(logit))
      if(link()=="probit"){
        df$X <- rbinom(N(), 1, rnorm(logit))
      }else if(link()=="logit"){
        df$X <- rbinom(N(), 1, df$PrX)
      }
      return(df)

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
      df <- setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_z2(), mean_xi1()),
              Sigma=matrix(c(var_z1(), cov_z1_z2(), cov_z1_xi1(),
                             cov_z1_z2(), var_z2(), cov_z2_xi1(),
                             cov_z1_xi1(), cov_z2_xi1(), var_xi1()), nrow=3), empirical=TRUE)),
              c("Z1", "Z2", "Xi1"))
      logit <- input$alpha0 + input$alpha1*df$Z1 + input$alpha2*df$Z2 + input$alpha3*df$Xi1
      df$PrX <- exp(logit)/(1+exp(logit))
      if(link()=="probit"){
        df$X <- rbinom(N(), 1, rnorm(logit))
      }else if(link()=="logit"){
        df$X <- rbinom(N(), 1, df$PrX)
      }
      return(df)

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
      df <- setNames(data.frame(mvrnorm(n=N(), mu=c(mean_z1(), mean_z2(), mean_xi1(), mean_xi2()),
              Sigma=matrix(c(var_z1(), cov_z1_z2(), cov_z1_xi1(), cov_z1_xi2(),
                             cov_z1_z2(), var_z2(), cov_z2_xi1(), cov_z2_xi2(),
                             cov_z1_xi1(), cov_z2_xi1(), var_xi1(), cov_xi1_xi2(),
                             cov_z1_xi2(), cov_z2_xi2(), cov_xi1_xi2(), var_xi2()), nrow=4), empirical=TRUE)),
              c("Z1", "Z2", "Xi1", "Xi2"))
      # simulate X
      logit <- input$alpha0 + input$alpha1*df$Z1 + input$alpha2*df$Z2 + input$alpha3*df$Xi1 + input$alpha4*df$Xi2
      df$PrX <- exp(logit)/(1+exp(logit))
      if(link()=="probit"){
        df$X <- rbinom(N(), 1, rnorm(logit))
      }else if(link()=="logit"){
        df$X <- rbinom(N(), 1, df$PrX)
      }
      return(df)
      }
    })


    ########## indicator variables for latent covariates
      data <- reactive({
        if(n_l_cov()>0){
          df1a <- df1()
          df1a$Y111 <- input$loading_Y111*df1a$Xi1 + rnorm(N(), 0, input$sd_e111)
          df1a$Y211 <- input$loading_Y211*df1a$Xi1 + rnorm(N(), 0, input$sd_e211)
          df1a$Y311 <- input$loading_Y311*df1a$Xi1 + rnorm(N(), 0, input$sd_e311)
        }
        if(n_l_cov()>1){
          df1a$Y112 <- input$loading_Y112*df1a$Xi2 + rnorm(N(), 0, input$sd_e112)
          df1a$Y212 <- input$loading_Y212*df1a$Xi2 + rnorm(N(), 0, input$sd_e212)
          df1a$Y312 <- input$loading_Y312*df1a$Xi2 + rnorm(N(), 0, input$sd_e312)
        }
        

        return(df1a)
      })
      


    ############################################ Raykov's idea ###############################################
    
    
    ################################# proved EffectLiteR approach ############################################
    
    
    
    ################################# new latent Propensity Score approach ###################################
    ## using lavaan to estimate treatment effect with latent PSs

    
    
    
    
    
    
    
    output$t <- renderTable({
      head(data())
    })
    output$text <- renderPrint({
      input$alpha1

    })
    
    
  }
)
