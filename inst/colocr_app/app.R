library(shiny)
library(shinyBS)
library(imager)
library(colocr)

# Define UI for application that draws a histogram
ui <- navbarPage(title = 'colocr',
                 tabPanel('Main',
                          sidebarLayout(
                            sidebarPanel(
                              tags$h3('Input Panel'),
                              tags$p('Get started by uploading the merge image. Then adjust the
                                      parameters to fit the regions of interest. Finally, assign a
                                      name to probe used in this image to be used in the output'),
                              tags$hr(),
                              fileInput('image1', 'Merge Image', multiple = TRUE),
                              bsTooltip('image1',
                                        'Upload one or more merge images.',
                                        'right', options = list(container = "body")),
                              tags$hr(),
                              sliderInput('threshold', 'Threshold', 1, 100, 50, 1),
                              bsTooltip('threshold',
                                        'Choose a threshold for excluding the background.',
                                        'right', options = list(container = "body")),
                              sliderInput('shrink', 'Shrink', 1, 10, 5, 1),
                              bsTooltip('shrink',
                                        'Shrink the selected area by eroding the bounderies around it.',
                                        'right', options = list(container = "body")),
                              sliderInput('grow', 'Grow', 1, 10, 5, 1),
                              bsTooltip('grow',
                                        'Grow the selected area by dilating the bounderies around it.',
                                        'right', options = list(container = "body")),
                              sliderInput('fill', 'Fill', 1, 10, 5, 1),
                              bsTooltip('fill',
                                        'Remove holes in the selected area.',
                                        'right', options = list(container = "body")),
                              sliderInput('clean', 'Clean', 1, 10, 5, 1),
                              bsTooltip('clean',
                                        'Remove small isolated parts in the selected area.',
                                        'right', options = list(container = "body")),
                              sliderInput('tolerance', 'Tolerance', 0, .99, .1, .1),
                              bsTooltip('tolerance',
                                        'Set value to determine which two neighboring pixels are in same selected area.',
                                        'right', options = list(container = "body")),
                              numericInput('roi_num', 'ROI Num', 1, 1, 50, 1),
                              bsTooltip('roi_num',
                                        'Select the number of regions. Default is one.',
                                        'right', options = list(container = "body")),
                              tags$hr(),
                              textInput('name', 'Probe Name'),
                              bsTooltip('name',
                                        'Enter the name of the probe used for staining. A name for all images or one for each separated by a comma.',
                                        'right', options = list(container = "body"))
                              ),
                            mainPanel(
                              fluidRow(
                                tags$h2('What are the different tabs for?'),
                                tags$br(),
                                tags$p('Each of the below tabs provide a view of your image, data
                                       and analysis output. The different tabs are connected and are
                                       updated automatically whenever the input panel is used.'),
                                tags$li('Select ROI: Choose regions of interst by adjusting the input
                                        parameters.'),
                                tags$li('Pixel Intensities: Check the scatter and density distribution
                                         of the pixel intensities from the two channels.'),
                                tags$li('Tabular Output: View the different colocalization
                                        co-efficients in tabular format.'),
                                tags$li('Graph View: View the co-localization co-efficients in graphical
                                        format.'),
                                tags$br(),
                                tags$br(),
                                tabsetPanel(
                                  tabPanel('Select ROI',
                                           plotOutput("image_plot"),
                                           textOutput('cor')
                                  ),
                                  tabPanel('Pixel Intensities', plotOutput('scatter')),
                                  tabPanel('Tabular View',
                                           tags$br(),
                                           tags$h3('Co-localization stats table.'),
                                           actionButton('add', 'Add'),
                                           actionButton('remove', 'Remove'),
                                           downloadButton('download_stats', 'Download Table'),
                                           tableOutput('tab'),
                                           tags$br(),
                                           tags$h3('Input parameters'),
                                           actionButton('add2', 'Add'),
                                           actionButton('remove2', 'Remove'),
                                           downloadButton('download_inputs', 'Download Table'),
                                           tableOutput('tab2')
                                  ),
                                  tabPanel('Graph View', plotOutput('res_plot'))
                                )
                                )
                                )
                          )),
                 tabPanel('GitHub',
                          "Comments, issues and contributions are welcomed.",
                          tags$a(href='https://github.com/MahShaaban/colocr_app',
                                 'https://github.com/MahShaaban/colocr_app')),
#                 tabPanel('About',
#                          includeMarkdown('README.md')),
                 tabPanel('Contact us',
                          tags$p('Department of Biochemistry and Convergence Medical Sciences
                                 Institute of Health Sciences,'),
                          tags$p('Gyeonsange National University School of Medicine'),
                          tags$p('861 Beongil 15 jinju-daero, jinju, Gyeongnam 660-751,'),
                          tags$p('Republic of Korea'),
                          tags$p('Mob:+82-10-4045-1767')))


# Define server
server <- function(input, output) {
  # intiate interactive values
  values <- reactiveValues()

  # load images
  img1 <- reactive({
    if(length(input$image1$datapath) > 1) {
      lapply(input$image1$datapath, load.image)
    } else {
      load.image(input$image1$datapath)
    }
  })

  # calculate the pixset
  px <- reactive({
    roi_select(img1(),
               threshold = input$threshold,
               shrink = input$shrink,
               grow = input$grow,
               fill = input$fill,
               clean = input$clean,
               n = input$roi_num)
  })


  # calculate correlations
  corr <- reactive({
    roi_test(px(), type = 'both')
  })

  # choose ROI view
  ## plot images
  output$image_plot <- renderPlot({
    req(input$image1)

    n <- length(input$image1$name) * 2

    par(mfrow=c(n,2), mar = rep(1, 4))
    roi_show(px())
  })

  ## text output of the calculated correlations
  output$cor <- renderText({
    req(input$image1)

    if(length(input$image1$name) > 1) {
      name <- input$image1$name
      pcc <- 0
      moc <- 0
      for(i in 1:length(name)) {
        pcc[i] <- round(mean(corr()[[i]]$pcc, na.rm = TRUE), 2)
        moc[i] <- round(mean(corr()[[i]]$moc, na.rm = TRUE), 2)
      }
      paste(name, ": Average PCC:", pcc,
            "and",
            "Average MOC:", moc)
    } else {
      paste("Average PCC:", round(mean(corr()$pcc, na.rm = TRUE), 2),
            " and ",
            "Average MOC:", round(mean(corr()$moc, na.rm = TRUE), 2))
    }

  })

  # quality control view
  output$scatter <- renderPlot({
    req(input$image1)
    n <- length(input$image1$name) * 2
    par(mfrow=c(n,2), mar = c(4,4,1,1))
    roi_check(px())
  })

  # tabular view
  ## co-localization stats table
  values$df = data.frame()

  ## add button
  observeEvent((input$add), {
    img_n <- length(input$image1$name)
    if(img_n > 1) {
      newdf <- corr()
      name <- unlist(strsplit(input$name, ','))
      if(length(name) != img_n) {
        name <- rep(name[1], img_n)
      }

      for(i in 1:img_n) {
        newdf[[i]] <- cbind(name = name[i],
                            image = input$image1$name[i],
                            roi = rownames(newdf[[i]]),
                            newdf[[i]])
      }

      newLine <- Reduce(rbind, newdf)
    } else {
      newLine <- cbind(name = input$name,
                       image = input$image1$name,
                       roi = rownames(corr()),
                       corr())
    }

    values$df <- rbind(values$df, newLine)
  })

  ## remove button
  observeEvent((input$remove), {
    n <- nrow(values$df)
    values$df <- values$df[-n, ]
  })

  ## table
  output$tab <- renderTable({values$df})

  # input parameters table
  values$df2 = data.frame()

  ## add button
  observeEvent((input$add2), {
    newLine <- data.frame(image = input$image1$name,
                          threshold = input$threshold,
                          shrink = input$shrink,
                          grow = input$grow,
                          fill = input$fill,
                          clean = input$clean,
                          tolerance = input$tolerance,
                          roi_num = input$roi_num)
    values$df2 <- rbind(values$df2, newLine)
  })

  ## remove button
  observeEvent((input$remove2), {
    n <- nrow(values$df2)
    values$df2 <- values$df2[-n, ]
  })

  ## table
  output$tab2 <- renderTable({values$df2})

  ## download buttons
  output$download_stats <- downloadHandler(
    filename = function() {
      format(Sys.time(), 'stats_%y.%m.%d_%H.%M.%S.csv')
    },
    content = function(con) {
      write.csv(values$df, con, row.names = FALSE)
    }
  )

  output$download_inputs <- downloadHandler(
    filename = function() {
      format(Sys.time(), 'inputs_%y.%m.%d_%H.%M.%S.csv')
    },
    content = function(con) {
      write.csv(values$df2, con, row.names = FALSE)
    }
  )

  ## graph view
  output$res_plot <- renderPlot({
    req(input$image1)

    if(nrow(values$df) >= 1) {
      par(mfrow = c(1, 2))
      x <- as.numeric(values$df$name)
      plot(x, values$df$pcc,
           type = 'n', xaxt = 'n',
           xlab = '', ylab = 'PCC',
           ylim = c(0,1))
      points(x, y = jitter(values$df$pcc), pch = 16)
      axis(1, unique(x), labels = unique(values$df$name))

      plot(x, values$df$moc,
           type = 'n', xaxt = 'n',
           xlab = '', ylab = 'MOC',
           ylim = c(0,1))
      points(x, y = jitter(values$df$moc), pch = 16)
      axis(1, unique(x), labels = unique(values$df$name))
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)

