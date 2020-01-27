

build_ui <- function(no.days=1, mfg.min=1, mfg.max=601,
                     ymax450=1.0, ymax650=0.07) {

  navbarPage(
    "100% testing of an ELISA production run",

    # The Description pane
    tabPanel(
      "Description",
      fluidPage(
        fluidRow(
          column(3),
          column(6, includeMarkdown(get_description_file()) ),
          column(3)
        )
      ),
    ),

    tabPanel("By Batch",
       sidebarLayout(
         sidebarPanel(
           sliderInput("day", "Day",
                       value=1,
                       min=1,
                       max=no.days,
                       step=1, round=TRUE),
           radioButtons("shift", "Shift",
                        choices=c("Day"="Day", "Evening"="Evening"),
                        selected="Day", inline=TRUE),
           radioButtons("wl", "Wavelength",
                        choices=c("450 nm"=450, "650 nm"=650),
                        selected=450, inline=TRUE),
           sliderInput("run.yrange", "OD range",
                       min=0, max=ymax450, value=c(0, ymax450), round=-2)
           ),
         mainPanel(
           plotOutput("runplot")
           )
         )
      ),


    tabPanel("By Mfg Order",
       sidebarLayout(
         sidebarPanel(

           conditionalPanel(
              condition="input.tabset == 'visplate'",
              sliderInput("mfgplate", "Mfg plate",
                          min=mfg.min, max=mfg.max, value=mfg.min,
                          step=1, round=TRUE
                          # animate=animationOptions(playButton=TRUE,
                          #                          pauseButton=TRUE)
                          ),
              sliderInput("odscale", "OD scale",
                          min=0, max=ymax450, value=c(0, ymax450)),
              selectInput("palette", "Palette",
                          choices=c("Seq - Greens"="Greens",
                                    "Seq - Blues"="Blues",
                                    "Seq - Reds"="Reds",
                                    "Div - Spectral"="Spectral")
                          )
             ),

           conditionalPanel(
             condition="input.tabset == 'mfg.mfgorder.tab'",
             textInput("mfg.mfgorder.wells", 'Wells (eg. "A1-B12, A11-H12")',
                       value="A1-H12"),
             sliderInput("mfg.xrange", "Mfg plate range",
                         min=1, max=mfg.max, value=c(mfg.min, mfg.max),
                         step=1, round=TRUE),
             sliderInput("mfg.yrange", "OD range",
                         min=0, max=ymax450, value=c(0,ymax450),
                         round=-2),
             numericInput("mfg.nplates", "Number of plates per plot",
                          value=200, min=1, max=(mfg.max - mfg.min + 1),
                          step=5),
             numericInput("mfg.bandsize", "Number of plates per band",
                          value=0, min=0, max=mfg.max, step=1),
             radioButtons( "mfg.mfgorder.type", "Plot type",
                          choices=c("None"="none",
                                    "Points"="points",
                                    "Lines"="parcoords"),
                          selected="points" ),
             radioButtons( "mfg.mfgorder.layers", "Averages",
                           choices=c("None"="none",
                                     "Means"="means",
                                     "Connected means"="connmean",
                                     "Medians"="medians",
                                     "Connected medians"="connmed"),
                           selected="none" ),
             radioButtons(
               "mfg.mfgorder.coloring", "Coloring",
               choices=c("None"="none",
                         "By run"="byrun",
                         "By well"="bywell"),
               selected=c("byrun"))
           ), # mfgplot.tab

           conditionalPanel(
             condition="input.tabset == 'mfgvar'",
             textInput("wells", 'Wells (eg. "A1-B12, A11-H12")',
                       value="A1-H12"),
             sliderInput("mfgrange", "Mfg plate range",
                         min=1, max=mfg.max, value=c(1,mfg.max),
                         step=1, round=TRUE),
             sliderInput("odrange", "OD range",
                         min=0, max=ymax450, value=c(0,ymax450),
                         round=-2),
             checkboxGroupInput("geoms", "OD layers",
                          choices=c("Points"="points",
                                    "Par. coord."="parcoord",
                                    "Means"="means",
                                    "Connected means"="connect"),
                          selected=c("points")),
             radioButtons("varplot", "Variability plot",
                          choices=c("St. Dev."="sd", "% CV"="cv",
                                    "Range"="range", "% Range"="relrange",
                                    "None"="none"),
                          selected="cv"),
             checkboxGroupInput("options", "Options",
                                choices=c("Jitter"="jitter", "Run coloring"="color.runs"))
             ),

           conditionalPanel(
             condition="input.tabset == 'gradplot'",
             radioButtons("gradplottype", "Plot type",
                          choices=c("Points"="points", "Rays"="rays"),
                          selected="points", inline=TRUE) #,
#              radioButtons("gradoptions", "Options",
#                           choices=c("Histogram"="histogram", "Density"="density",
#                                     "None"="none"),
#                           selected="histogram")
            ),

           conditionalPanel(
             condition="input.tabset == 'mfg.active.tab'",
             sliderInput("mfg.active.xrange", "Mfg plate range",
                         min=1, max=mfg.max, value=c(mfg.min, mfg.max),
                         step=1, round=TRUE),
             sliderInput("mfg.active.yrange", "OD range",
                         min=0, max=ymax450, value=c(0,ymax450),
                         round=-2),
             radioButtons(
               "mfg.active.coloring", "Coloring",
               choices=c("None"="none",
                         "By run"="byrun",
                         "By well"="bywell"),
               selected=c("byrun"))
           )

          ),

         mainPanel(
           tabsetPanel(
             tabPanel(title="Plate",
                      plotOutput("visplate"),
                      value="visplate"),
             tabPanel(title="Mfg order",
                      tags$div(
                        #style="overflow-y:scroll; height:500px",
                        uiOutput("mfg.mfgorder.ui") ),
                      value="mfg.mfgorder.tab"),
             tabPanel(title="Variability",
                      plotOutput("mfgvar"),
                      value="mfgvar"),
             tabPanel(title="OD gradients",
                      plotOutput("gradplot", width="500px", height="500px"),
                      value="gradplot"),
             tabPanel(title="Interactive",
                      ggvis::ggvisOutput("mfgActivePlot"),
                      dataTableOutput("mfg.active.data"),
                      value="mfg.active.tab"),
             id="tabset"
             )
           )

         )  # sidebarLayout


       ),  # tabPanel

    tabPanel("Embedded MSA",
       sidebarLayout(
         sidebarPanel(
           # Include plot options here...

           ),
         mainPanel(
           plotOutput("msaplot")
           )
         )
       )

  )  # navbarPage

}
