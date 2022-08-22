# Welcome to mexcaR

# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

rm(list = ls())

# SOURCE CUSTOM FUNCTIONS  ----------------------------------------------------

source('tools/mexca_fun.R')
# UI - GENERAL --------------------------------------------------------------

ui <- fillPage(
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
             h4("Filter"),
             uiOutput("slider")
           )
    ),
    column(4,
           withLoader(plotOutput('frame_plot'), type = 'html', loader = 'loader3'),
           wellPanel(
             span(textOutput('text_output'),
                  style='color:black'
             )
           ),
    ),
    column(5,
           withLoader(plotOutput('au'), type = 'html', loader = 'loader3')
    )
  )
)


# SERVER LOGIC  --------------------------------------------------------------
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2) # to accept files up to 30MB
  
  input_name <- reactive(input$your_mp4$name)
  
  load_df <- reactive({
    
    req(input$your_csv)
    req(input$your_mp4)
    
    csv_file <- input$your_csv
    mp4_file <- input$your_mp4
    
    ext_1 <- tools::file_ext(csv_file); ext_2 <- tools::file_ext(mp4_file)
    
    validate(need(ext_1 == 'csv', 'Please upload a csv file')); validate(need(ext_2 == 'mp4', 'Please upload a mp4 file'))
    
    mexca_file <- csv_file$datapath
    video_file <- mp4_file$datapath
    
    df <- load_input(video_path = video_file, mexca_path = mexca_file, image_folder_name = input_name())
    
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
    
    if(is.null(input$your_csv)){
      mexca_data_clean <- read.csv('data/debate_output_tidy.csv')
    } else {
      mexca_data_clean <- tidy_mexca()
    }
    
    predicted_speech <- unpack_speech_dialogue(mexca_data_clean)
    return(predicted_speech)
  })
  
  annotate_images <- reactive({
    mexca_data_clean <- tidy_mexca()
    draw_face_boxes_on_frame(mexca_data_clean, keep_video_frames = F, facial_landamarks = T, image_folder_name = input_name())
  })
  
  plot_images <- reactive({
    
    if (is.null(input$your_csv)){
      imgs <- list.files('video_frames_annotated_demo', full.names = T)
      
    } else {
      annotate_images()
      images_folder_name <- tools::file_path_sans_ext(input_name())
      imgs <- list.files(paste0('video_frames_annotated_', images_folder_name), full.names = T)
    }
    
    selected_frame <- input$frame_selector
    png::readPNG(imgs[selected_frame]) -> img
    return(img)
  })
  
  
  au_interactive <- reactive({
    
    if (is.null(input$your_csv)){
      mexca_data_clean <- read.csv('data/debate_output_tidy.csv')

    } else {
      mexca_data_clean <- tidy_mexca()
    }
    
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
                   pull(text_token) %>% unique())
  })
  
  output$frame_plot <- renderPlot(ggimage(plot_images()), res = 96)
  output$au <- renderPlot(au_interactive(), res = 96)
  output$text_output <- renderText(paste0(speech_interactive()))
  output$slider <- renderUI({
    
    if(is.null(input$your_mp4)){
      maximum_frame <- floor(av_video_info('data/debate.mp4')$video['frames'])
    } else {
      maximum_frame <- floor(av_video_info(input$your_mp4$datapath)$video['frames'])
    }
    
    sliderInput("frame_selector", h5("Frame number"), min = 1, max = maximum_frame, value = 1, step = 1, animate = animationOptions(loop = F, interval = 900))
  })
  
}

# RUN APP  --------------------------------------------------------------
shinyApp(ui = ui, server = server)
