# Welcome to mexcaR

# INSTALL DEPENDENCIES ----------------------------------------------------

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

rm(list = ls())

# SOURCE CUSTOM FUNCTIONS  ----------------------------------------------------

source('R/utils.R')
source('R/ui.R')
# UI - GENERAL --------------------------------------------------------------

ui <- fillPage(
  titlePanel("mexcaR"),
  ui_minimal,
  ui_display_data
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
    draw_face_boxes_on_frame(mexca_data_clean, keep_video_frames = F, facial_landamarks = T, image_folder_name = input_name(), session)
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
    imgs <- gtools::mixedsort(imgs) #reorder the frames by last number
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
    frame_seq <- seq(selected_frame-15,selected_frame)
    
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
    
    sliderInput("frame_selector", h5("Frame number"), min = 1, max = maximum_frame, value = 1, step = 1, animate = animationOptions(loop = F, interval = 800))
  })
  
  output$download_tsv <- downloadHandler(
    filename = function() {
      if(!is.null(input$your_mp4)){
        paste0(tools::file_path_sans_ext(input_name()), '.tsv')
      } else {
        paste0('debate_demo_processed','.tsv')
      }
    },
    content = function(file) {
      if(!is.null(input$your_mp4)){
        vroom::vroom_write(tidy_mexca(), file)
      } else {
        vroom::vroom_write(read.csv('data/debate_output_tidy.csv'), file)
      }
    }
  )
  
  output$download_mp4 <- downloadHandler(
    filename = function() {
      if(!is.null(input$your_mp4)){
        paste0(tools::file_path_sans_ext(input_name()), 'annotated.mp4')
      } else {
        paste0('debate_demo_annotated','.mp4')
      }
    },
    content = function(file) {
      if(!is.null(input$your_mp4)){
        maximum_frame <- floor(av_video_info(input$your_mp4$datapath)$video['framerate'])
        images_folder_name <- tools::file_path_sans_ext(input_name())
        imgs <- list.files(paste0('video_frames_annotated_', images_folder_name), full.names = T)
        av::av_encode_video(input = gtools::mixedsort(imgs), 
                            output = file, framerate = maximum_frame)
      } else {
        maximum_frame <- floor(av::av_video_info('data/debate.mp4')$video['framerate'])
        imgs <- list.files('video_frames_annotated_demo', full.names = TRUE)
        av::av_encode_video(input = gtools::mixedsort(imgs), 
                            output = file, framerate = maximum_frame)
      }
    }
  )
  
  output$preview <- DT::renderDataTable(DT::datatable({
    if (is.null(input$your_csv)){
      mexca_data_clean <- read.csv('data/debate_output_tidy.csv')
      
    } else {
      mexca_data_clean <- tidy_mexca()
    }
    mexca_data_clean
    
  }, 
  options = list(scrollY = "250px",
                 scrollX = T,
                 deferRender = TRUE,
                 scroller = TRUE,
                 paging = TRUE,
                 autoWidth = TRUE,
                 dom = 'lBfrtip',
                 fixedColumns = TRUE), 
  rownames = FALSE))
  
  output$plot_name <- renderText({
    if(!is.null(input$your_mp4)){
      title <- tools::file_path_sans_ext(input_name())
    } else {
      title <- 'Clinton vs Trump debate (2016)'
    }
    title
  })
}

# RUN APP  --------------------------------------------------------------
shinyApp(ui = ui, server = server)


