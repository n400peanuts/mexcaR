# Welcome to mexcaR

# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

rm(list = ls())

# SOURCE CUSTOM FUNCTIONS  ----------------------------------------------------

source('tools/mexca_fun.R')
# UI - GENERAL --------------------------------------------------------------

ui <- fluidPage(
  useShinyFeedback(),
  titlePanel("mexcaR"),
  fluidRow(
    column(2,
           wellPanel(
             div(img(src = "mexca_logo_minimal.png",
                     width = "70px", height = "70px"), style="text-align: center;")
           ),
           wellPanel(
             h4("Upload mexca output"),
             fileInput('your_csv',"choose csv file", accept = c('.csv'))
           ),
           wellPanel(
             h4("Upload video"),
             fileInput('your_mp4',"choose mp4 file", accept = c('.mp4'))
           ),
           wellPanel(
             h4("Demo"),
             selectInput('demo_dt', 'Choose a demo data', 
                         choices = c('Clinton Vs Trump debate (2016)' = c('data/debate_output.csv'))
             )
           ),
           wellPanel(
             h4("Filter"),
             uiOutput("slider")
           )
    ),
    column(4,
           plotOutput('frame_plot'),
           wellPanel(
             span(textOutput('text_output'),
                  style='color:black'
             )
           )
    ),
    column(5,
           plotOutput('au')
    )
  )
)


# SERVER LOGIC  --------------------------------------------------------------
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2) # to accept files up to 30MB
  
  
  load_df <- reactive({
    
    #   req(input$your_csv)
    #   req(input$your_mp4)
    if (is.null(input$your_csv$datapath)) {
      video_file <- c('data/debate.mp4')
      
      df <- load_input(video_path = video_file, mexca_path = input$demo_dt)
      
    } else {
      
      csv_file <- input$your_csv
      mp4_file <- input$your_mp4
      
      ext_1 <- tools::file_ext(csv_file); ext_2 <- tools::file_ext(mp4_file)
      
      req(csv_file); req(mp4_file)
      validate(need(ext_1 == 'csv', 'Please upload a csv file')); validate(need(ext_2 == 'mp4', 'Please upload a mp4 file'))
      
      mexca_file <- csv_file$datapath
      video_file <- mp4_file$datapath
      
      
      df <- load_input(video_path = video_file, mexca_path = mexca_file)
    }
    return(df)
  })
  
  
  tidy_mexca <- reactive({
    mexca_data <- load_df()
    
    options(warn=-1)
    mexca_data_clean <- prepare_dataframe(mexca_data)
    options(warn=0)
    
    return(mexca_data_clean)
  })
  
  tidy_speech <- reactive({
    mexca_data_clean <- tidy_mexca()
    predicted_speech <- unpack_speech_dialogue(mexca_data_clean)
    
    return(predicted_speech)
  })
  
  frame_image <- reactive({
    mexca_data_clean <- tidy_mexca()
    draw_face_boxes_on_frame(mexca_data_clean, keep_video_frames = F, facial_landamarks = T)
    
    imgs <- list.files('video_frames_annotated', full.names = T)
    selected_frame <- input$frame_selector
    png::readPNG(imgs[selected_frame]) -> img
    img
  })
  
  au_interactive <- reactive({
    mexca_data_clean <- tidy_mexca()
    selected_frame <- input$frame_selector
    plot_AUs(mexca_data_clean[mexca_data_clean$frame == selected_frame,])
  })
  
  speech_interactive <- reactive({
    
    predicted_speech <- tidy_speech()
    
    selected_frame <- input$frame_selector
    # word window of 15 frames
    frame_seq <- seq(selected_frame-15,selected_frame+15)
    
    as.character(predicted_speech%>%
                   filter(frame %in% frame_seq)%>%
                   pull(text_token) %>% droplevels() %>% unique())
  })
  
  floor_frame <- reactive({
    floor(av_video_info(input$your_mp4$datapath)$video['frames'])
  })
  
  floor_frame_demo <- reactive({
    floor(av_video_info('data/debate.mp4')$video['frames'])
  })
  
  output$frame_plot <- renderPlot(ggimage(frame_image()), res = 96)
  output$au <- renderPlot(au_interactive(), res = 96)
  output$text_output <- renderText(paste0(speech_interactive()))
  output$slider <- renderUI({
    
    if(!is.null(input$your_mp4)){
      sliderInput("frame_selector", h5("Frame number"), min = 1, max = floor_frame(), value = 1, step = 1, animate = animationOptions(loop = F, interval = 800))
    }
    sliderInput("frame_selector", h5("Frame number"), min = 1, max = floor_frame_demo(), value = 1, step = 1, animate = animationOptions(loop = F, interval = 800))
    
  })
  
}

# RUN APP  --------------------------------------------------------------
shinyApp(ui = ui, server = server)
