library(shiny)
# Estimating Causal Effects by Adjusting for Propensity Scores computed from Latent Variables

shinyUI(fluidPage(
  titlePanel(h3("Schätzung kausaler Effekte durch Adjustierung für latente Propensity Scores")),
  
  sidebarLayout(
    sidebarPanel(
      
      ############# sample size ##################
      sliderInput(inputId="N", label="Stichprobengröße", value=10000, min=1000, max=1000000),
      br(),
      ############# preconfiguration #############
      radioButtons(inputId="conf", label="Wähle Voreinstellungen", 
                   choices=c("Axel (eine latente, eine manifeste Kovariate, manifeste AV, keine Interaktion)"="axel", 
                             "Raykov (zwei latente Kovariaten, die kovariieren, manifeste AV, keine Interaktion)"="raykov", 
                             "Standard (keine Vorgaben)"="standard")),
      
      ## number of repetitions of simulations ##
      radioButtons(inputId="n_rep", label="Wähle Anzahl der Simulationen", 
                   choices=c("1"="gramm", "1000"="dp","50 000"="z", "100 000"="dz")), # dp ... Doppelpfund, # dz ... Doppelzentner
      
      tabsetPanel(
        ############# independent variables #############
        tabPanel('Unabhängige Variablen',
                 br(),
                 # number of latent covariates
                 fluidRow(column(6, h4("Anzahl latenter Kovariaten")),
                          column(6, numericInput("n_l_cov", label=NULL, value=1,
                                                 min = 1, max = 2, width='60%'))),
                 conditionalPanel(
                   condition = "input.n_l_cov > 0",
                   h4(strong("Latente Kovariate 1")),
                   fluidRow(column(6, numericInput(inputId="mean_xi1", label=h5("Mittelwert von Xi1"),
                                                   value=0, width='100%', min=100, max=100)),
                            column(6, sliderInput(inputId="sd_xi1", label=h5("Standardabweichung von Xi1"),
                                                  value=1, min=0.001, max=4))
                   ),
                   h5(strong("Indikator Y111 ")),
                   fluidRow(column(3, numericInput(inputId="intercept_Y111",
                                                   label=h5("Intercept Y111"), value=0, width='100%')),
                            column(3, numericInput(inputId="loading_Y111",
                                                   label=h5("Ladung Y111"), value=1, width='100%')),
                            column(6, numericInput(inputId="mean_e111",
                                                   label=h5("Mean(\u03B5)"), value=0, width='100%'),
                                      sliderInput(inputId="sd_e111",
                                               label=h5("SD(\u03B5)"), value=1, min=0.001, max=3, width='100%'))),
                   h5(strong("Indikator Y211 ")),
                   fluidRow(column(3, numericInput(inputId="intercept_Y211",
                                                   label=h5("Intercept Y211"), value=0, width='100%')),
                            column(3, numericInput(inputId="loading_Y211",
                                                   label=h5("Ladung Y211"), value=1, width='100%')),
                            column(6, numericInput(inputId="mean_e211",
                                                   label=h5("Mean(\u03B5)"), value=0, width='100%'),
                                   sliderInput(inputId="sd_e211",
                                               label=h5("SD(\u03B5)"), value=1, min=0.001, max=3, width='100%'))),
                   h5(strong("Indikator Y311 ")),
                   fluidRow(column(3, numericInput(inputId="intercept_Y311",
                                                   label=h5("Intercept Y311"), value=0, width='100%')),
                            column(3, numericInput(inputId="loading_Y311",
                                                   label=h5("Ladung Y311"), value=1, width='100%')),
                            column(6, numericInput(inputId="mean_e311",
                                                   label=h5("Mean(\u03B5)"), value=0, width='100%'),
                                   sliderInput(inputId="sd_e311",
                                               label=h5("SD(\u03B5)"), value=1, min=0.001, max=3, width='100%')))
                 ),
                 conditionalPanel(
                   condition = "input.n_l_cov > 1",
                   h4(strong("Latente Kovariate 2")),
                   fluidRow(column(6, numericInput(inputId="mean_xi2", label="Mittelwert von Xi2",
                                                   value=0, width='100%', min=100, max=100)),
                            column(6, sliderInput(inputId="sd_xi2", label=h5("Standardabweichung von Xi2"),
                                                  value=1, min=0.001, max=4))
                   ),
                   h5(strong("Indikator Y112 ")),
                   fluidRow(column(3, numericInput(inputId="intercept_Y112",
                                                   label=h5("Intercept Y112"), value=0, width='100%')),
                            column(3, numericInput(inputId="loading_Y112",
                                                   label=h5("Ladung Y112"), value=1, width='100%')),
                            column(6, numericInput(inputId="mean_e112",
                                                   label=h5("Mean(\u03B5)"), value=0, width='100%'),
                                   sliderInput(inputId="sd_e112",
                                               label=h5("SD(\u03B5)"), value=1, min=0.001, max=3, width='100%'))),
                   h5(strong("Indikator Y212 ")),
                   fluidRow(column(3, numericInput(inputId="intercept_Y212",
                                                   label=h5("Intercept Y212"), value=0, width='100%')),
                            column(3, numericInput(inputId="loading_Y212",
                                                   label=h5("Ladung Y212"), value=1, width='100%')),
                            column(6, numericInput(inputId="mean_e212",
                                                   label=h5("Mean(\u03B5)"), value=0, width='100%'),
                                   sliderInput(inputId="sd_e212",
                                               label=h5("SD(\u03B5)"), value=1, min=0.001, max=3, width='100%'))),
                   h5(strong("Indikator Y312 ")),
                   fluidRow(column(3, numericInput(inputId="intercept_Y312",
                                                   label=h5("Intercept Y312"), value=0, width='100%')),
                            column(3, numericInput(inputId="loading_Y312",
                                                   label=h5("Ladung Y312"), value=1, width='100%')),
                            column(6, numericInput(inputId="mean_e312",
                                                   label=h5("Mean(\u03B5)"), value=0, width='100%'),
                                   sliderInput(inputId="sd_e312",
                                               label=h5("SD(\u03B5)"), value=1, min=0.001, max=3, width='100%')))
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 
                 # number of manifest covariates
                 fluidRow(column(6, h4("Anzahl manifester Kovariaten")),
                          column(6, numericInput("n_m_cov", label=NULL, value=1,
                                                 min = 0, max = 2, width='60%'))),
                 conditionalPanel(
                   condition = "input.n_m_cov > 0",
                   h4(strong("Manifeste Kovariate 1")),
                   fluidRow(column(6, numericInput(inputId="mean_z1", label="Mittelwert", value=0)),
                            column(6, numericInput(inputId="sd_z1", label="Standardabweichung", value=1)))
                 ),
                 br(),
                 br(),
                 conditionalPanel(
                   condition = "input.n_m_cov > 1",
                   h4(strong("Manifeste Kovariate 2")),
                   fluidRow(column(6, numericInput(inputId="mean_z2", label="Mittelwert", value=0)),
                            column(6, numericInput(inputId="sd_z2", label="Standardabweichung", value=1)))
                 ),
                 br(),
                 ## variance-covariance-matrix
                 # tags$textarea(id="foo", rows=2, cols=5, "1 \t 0.5 \n 0.5 \t 1")
                 h4("Gib die Kovarianzen zwischen den unabhängigen Variablen an"),
                  uiOutput("cov")

       ),

       ############# Regression #############
       # always manifest continuous covariates first and then latent continuous covariates
       # f.ex.: 1 manifest and 1 latent covariate -> gamma001 and gamma101 is for manifest covariate and 
       # gamma002 and gamma102 is for latent covariate
       tabPanel('Regression',
                h5("Regression"),
                uiOutput("regression")
       )
       #         h5("Effektfunktion g1"),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 0 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(2, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Xi2")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(2, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Xi1")))
       #          ),
       # 
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma103", label="\u03B3_103", value=0, width='100%')),
       #                     column(1, p("Xi2")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma103", label="\u03B3_103", value=0, width='100%')),
       #                     column(1, p("Xi1")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma103", label="\u03B3_103", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma104", label="\u03B3_104", value=0, width='100%')),
       #                     column(1, p("Xi2")))
       #          ),
       #          ## specifying the regression E(Eta|X, Z, Xi)
       #          h5("gesamte Regression"),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 0 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(2, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(2, p("Xi2 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(2, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(2, p("Xi1 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma003", label="\u03B3_003", value=0, width='100%')),
       #                     column(2, p("Xi2 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma003", label="\u03B3_003", value=0, width='100%')),
       #                     column(2, p("Xi1 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma003", label="\u03B3_003", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma004", label="\u03B3_004", value=0, width='100%')),
       #                     column(2, p("Xi2 + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          )
       
       # tabPanel('Regression',
       #          # effect function g1 (anyway no categorical variables and restricted to one treatment variable)
       #          h5("Effektfunktion g1"),
       #          conditionalPanel(
       #            condition = "input.n_m_cov > 0 & input.n_l_cov > 0",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(2, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Xi2")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(2, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Xi1")))
       #          ),
       #          
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma103", label="\u03B3_103", value=0, width='100%')),
       #                     column(1, p("Xi2")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma103", label="\u03B3_103", value=0, width='100%')),
       #                     column(1, p("Xi1")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma100", label="\u03B3_100", value=0.2, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma101", label="\u03B3_101", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma102", label="\u03B3_102", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma103", label="\u03B3_103", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma104", label="\u03B3_104", value=0, width='100%')),
       #                     column(1, p("Xi2")))
       #          ),
       #          ## specifying the regression E(Eta|X, Z, Xi)
       #          h5("gesamte Regression"),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 0 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(2, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(2, p("Xi2 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(2, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(2, p("Xi1 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 1 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma003", label="\u03B3_003", value=0, width='100%')),
       #                     column(2, p("Xi2 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 1",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma003", label="\u03B3_003", value=0, width='100%')),
       #                     column(2, p("Xi1 + g1*X + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          ),
       #          conditionalPanel(
       #            condition = "input.n_m_cov == 2 & input.n_l_cov == 2",
       #            fluidRow(column(2, numericInput(inputId="gamma000", label="\u03B3_000", value=0, width='100%')),
       #                     column(1, p("+")),
       #                     column(2, numericInput(inputId="gamma001", label="\u03B3_001", value=0, width='100%')),
       #                     column(1, p("Z1 +")),
       #                     column(2, numericInput(inputId="gamma002", label="\u03B3_002", value=0, width='100%')),
       #                     column(1, p("Z2 +")),
       #                     column(2, numericInput(inputId="gamma003", label="\u03B3_003", value=0, width='100%')),
       #                     column(1, p("Xi1 +")),
       #                     column(2, numericInput(inputId="gamma004", label="\u03B3_004", value=0, width='100%')),
       #                     column(2, p("Xi2 + (")),
       #                     column(2, numericInput(inputId="mean_ceta", label="m", value=0, width='100%')),
       #                     column(1, p(",")),
       #                     column(2, numericInput(inputId="sd_ceta", label="sd", value=0.35, width='100%')),
       #                     column(1, p(")")))
       #          )
       # 
       # 
       # )
      )
    ),
    mainPanel(
      #tableOutput("t")
    )
  )
)
)
