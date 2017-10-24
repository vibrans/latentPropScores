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
                   h4(strong("Latente Kovariate Xi\u2081")),
                   fluidRow(column(6, numericInput(inputId="mean_xi1", label=h5(strong("Mean(Xi\u2081)")),
                                                   value=0, width='100%', min=100, max=100)),
                            column(6, sliderInput(inputId="sd_xi1", label=h5(strong("SD(Xi\u2081)")),
                                                  value=1, min=0.001, max=4))
                   ),
                   h3(strong("Y\u2081\u2081\u2081 =")),
                   fluidRow(column(2, h4("\u03BD\u2081\u2081\u2081")),
                            column(1, h4("+")),
                            column(2, h4("\u03BB\u2081\u2081\u2081*Xi\u2081")),
                            column(1, h4("+")),
                            column(1, h4("(")),
                            column(2, h4("Mean(\u03B5\u2081\u2081\u2081), ")),
                            column(2, h4("SD(\u03B5\u2081\u2081\u2081)")),
                            column(1, h4(")"))),
                   fluidRow(column(2, numericInput(inputId="intercept_Y111",
                                                   label=NULL, value=0, width='100%')),
                            column(1),
                            column(2, numericInput(inputId="loading_Y111",
                                                   label=NULL, value=1, width='100%')),
                            column(2),
                            column(2, numericInput(inputId="mean_e111",
                                                   label=NULL, value=0, width='100%')),
                            column(2, numericInput(inputId="sd_e111",
                                               label=NULL, value=1, min=0.001, max=3, width='100%'))),
                   h3(strong("Y\u2082\u2081\u2081 =")),
                   fluidRow(column(2, h4("\u03BD\u2082\u2081\u2081")),
                            column(1, h4("+")),
                            column(2, h4("\u03BB\u2082\u2081\u2081*Xi\u2081")),
                            column(1, h4("+")),
                            column(1, h4("(")),
                            column(2, h4("Mean(\u03B5\u2082\u2081\u2081), ")),
                            column(2, h4("SD(\u03B5\u2082\u2081\u2081)")),
                            column(1, h4(")"))),
                   fluidRow(column(2, numericInput(inputId="intercept_Y211",
                                                   label=NULL, value=0, width='100%')),
                            column(1),
                            column(2, numericInput(inputId="loading_Y211",
                                                   label=NULL, value=1, width='100%')),
                            column(2),
                            column(2, numericInput(inputId="mean_e211",
                                                   label=NULL, value=0, width='100%')),
                            column(2, numericInput(inputId="sd_e211",
                                               label=NULL, value=1, min=0.001, max=3, width='100%'))),
                   h3(strong("Y\u2083\u2081\u2081 =")),
                   fluidRow(column(2, h4("\u03BD\u2083\u2081\u2081")),
                            column(1, h4("+")),
                            column(2, h4("\u03BB\u2083\u2081\u2081*Xi\u2081")),
                            column(1, h4("+")),
                            column(1, h4("(")),
                            column(2, h4("Mean(\u03B5\u2083\u2081\u2081), ")),
                            column(2, h4("SD(\u03B5\u2083\u2081\u2081)")),
                            column(1, h4(")"))),
                   fluidRow(column(2, numericInput(inputId="intercept_Y311",
                                                   label=NULL, value=0, width='100%')),
                            column(1),
                            column(2, numericInput(inputId="loading_Y311",
                                                   label=NULL, value=1, width='100%')),
                            column(2),
                            column(2, numericInput(inputId="mean_e311",
                                                   label=NULL, value=0, width='100%')),
                            column(2, numericInput(inputId="sd_e311",
                                               label=NULL, value=1, min=0.001, max=3, width='100%')))
                 ),
                 br(),
                 conditionalPanel(
                   condition = "input.n_l_cov > 1",
                   h4(strong("Latente Kovariate Xi\u2082")),
                   fluidRow(column(6, numericInput(inputId="mean_xi2", label="Mean(Xi\u2082)",
                                                   value=0, width='100%', min=100, max=100)),
                            column(6, sliderInput(inputId="sd_xi2", label=h5("SD(Xi\u2082)"),
                                                  value=1, min=0.001, max=4))
                   ),
                   h3(strong("Y\u2081\u2081\u2082")),
                   fluidRow(column(2, h4("\u03BD\u2081\u2081\u2082")),
                            column(1, h4("+")),
                            column(2, h4("\u03BB\u2081\u2081\u2082*Xi\u2082")),
                            column(1, h4("+")),
                            column(1, h4("(")),
                            column(2, h4("Mean(\u03B5\u2081\u2081\u2082), ")),
                            column(2, h4("SD(\u03B5\u2081\u2081\u2082)")),
                            column(1, h4(")"))),
                   fluidRow(column(2, numericInput(inputId="intercept_Y112",
                                                   label=NULL, value=0, width='100%')),
                            column(1),
                            column(2, numericInput(inputId="loading_Y112",
                                                   label=NULL, value=1, width='100%')),
                            column(2),
                            column(2, numericInput(inputId="mean_e112",
                                                   label=NULL, value=0, width='100%')),
                            column(2, numericInput(inputId="sd_e112",
                                               label=NULL, value=1, min=0.001, max=3, width='100%'))),
                   h3(strong("Y\u2082\u2081\u2082")),
                   fluidRow(column(2, h4("\u03BD\u2082\u2081\u2082")),
                            column(1, h4("+")),
                            column(2, h4("\u03BB\u2082\u2081\u2082*Xi\u2082")),
                            column(1, h4("+")),
                            column(1, h4("(")),
                            column(2, h4("Mean(\u03B5\u2082\u2081\u2082), ")),
                            column(2, h4("SD(\u03B5\u2082\u2081\u2082)")),
                            column(1, h4(")"))),
                   fluidRow(column(2, numericInput(inputId="intercept_Y212",
                                                   label=NULL, value=0, width='100%')),
                            column(1),
                            column(2, numericInput(inputId="loading_Y212",
                                                   label=NULL, value=1, width='100%')),
                            column(2),
                            column(2, numericInput(inputId="mean_e212",
                                                   label=NULL, value=0, width='100%')),
                            column(2, numericInput(inputId="sd_e212",
                                                   label=NULL, value=1, min=0.001, max=3, width='100%'))),
                   h3(strong("Y\u2083\u2081\u2082")),
                   fluidRow(column(2, h4("\u03BD\u2083\u2081\u2082")),
                            column(1, h4("+")),
                            column(2, h4("\u03BB\u2083\u2081\u2082*Xi\u2082")),
                            column(1, h4("+")),
                            column(1, h4("(")),
                            column(2, h4("Mean(\u03B5\u2083\u2081\u2082), ")),
                            column(2, h4("Sd(\u03B5\u2083\u2081\u2082)")),
                            column(1, h4(")"))),
                   fluidRow(column(2, numericInput(inputId="intercept_Y312",
                                                   label=NULL, value=0, width='100%')),
                            column(1),
                            column(2, numericInput(inputId="loading_Y312",
                                                   label=NULL, value=1, width='100%')),
                            column(2),
                            column(2, numericInput(inputId="mean_e312",
                                                   label=NULL, value=0, width='100%')),
                            column(2, numericInput(inputId="sd_e312",
                                                   label=NULL, value=1, min=0.001, max=3, width='100%')))
                 ),
                 br(),
                 br(),
                 br(),
                 br(),
                 tags$hr(style="border-color: purple;"),
                 # number of manifest covariates
                 fluidRow(column(6, h4("Anzahl manifester Kovariaten")),
                          column(6, numericInput("n_m_cov", label=NULL, value=1,
                                                 min = 0, max = 2, width='60%'))),
                 conditionalPanel(
                   condition = "input.n_m_cov > 0",
                   h4(strong("Manifeste Kovariate Z\u2081")),
                   fluidRow(column(6, numericInput(inputId="mean_z1", label="Mean(Z\u2081)", value=0)),
                            column(6, numericInput(inputId="sd_z1", label="SD(Z\u2081)", value=1)))
                 ),
                 br(),
                 br(),
                 conditionalPanel(
                   condition = "input.n_m_cov > 1",
                   h4(strong("Manifeste Kovariate Z\u2082")),
                   fluidRow(column(6, numericInput(inputId="mean_z2", label="Mean(Z\u2082)", value=0)),
                            column(6, numericInput(inputId="sd_z2", label="SD(Z\u2082)", value=1)))
                 ),
                 br(),
                 ## variance-covariance-matrix
                 h4("Gib die Kovarianzen zwischen den unabhängigen Variablen an:"),
                  uiOutput("cov")

       ),

       ############# Regression #############
       # always manifest continuous covariates first and then latent continuous covariates
       # f.ex.: 1 manifest and 1 latent covariate -> gamma001 and gamma101 is for manifest covariate and 
       # gamma002 and gamma102 is for latent covariate
       tabPanel('Regression',
                uiOutput("regression")
       )
      )
    ),
    mainPanel(
      #tableOutput("t")
    )
  )
)
)
