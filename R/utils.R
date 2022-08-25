# prepare_dataframe (tidy-up mexca output) ------------------------------------------------------------------
prepare_dataframe <- function(dataframe){
  dataframe <- unpack_face_box(dataframe)
  dataframe <- unpack_face_aus(dataframe)
  dataframe <- unpack_face_landmarks(dataframe)
  
  dataframe <- dataframe |>
    mutate_at(vars(FaceRectX,FaceRectY,FaceRectWidth,FaceRectHeight,AU01, AU02, AU04, AU06, AU07, AU10, AU12, AU14, AU15, AU17, AU23, AU24), as.numeric) |>
    mutate_at(vars(speaker_id, text_token), as.factor) |>
    mutate(across(where(is.numeric), round, 3)) 
  return(dataframe)
}

# unpack_face_box ------------------------------------------------------------------
# takes the dataframe as input and returns four columns with the x-y-width-height coordinates' of the face_box column
unpack_face_box <- function(dataframe){
  str_replace_all(dataframe$face_box,fixed("]"), "") -> dataframe$face_box_temp
  str_replace_all(dataframe$face_box_temp,fixed("[ "), "") -> dataframe$face_box_temp
  str_replace_all(dataframe$face_box_temp,fixed("["), "") -> dataframe$face_box_temp
  
  dataframe |>
    separate(face_box_temp, c("FaceRectX","FaceRectY","FaceRectWidth","FaceRectHeight"), "[ ]{1,}") -> dataframe
  dataframe$face_box <- NULL
  return(dataframe)
}


# unpack face_aus  ------------------------------------------------------------------
# takes the dataframe as input and returns 12 columns with the AUs (NOTE: this is based on JAANET model)

unpack_face_aus <- function(dataframe){
  str_replace_all(dataframe$face_aus,fixed("\n"), "") -> dataframe$face_aus_temp
  str_replace_all(dataframe$face_aus_temp,fixed("["), "") -> dataframe$face_aus_temp
  str_replace_all(dataframe$face_aus_temp,fixed("]"), "") -> dataframe$face_aus_temp
  
  dataframe |>
    separate(face_aus_temp, c("AU01", "AU02", "AU04", "AU06", "AU07", "AU10", "AU12", "AU14", "AU15", "AU17", "AU23", "AU24"), "\\s+") -> dataframe
  dataframe$face_aus <- NULL
  return(dataframe)
}

# unpack face-landmarks ------------------------------------------------------------------

unpack_face_landmarks <- function(dataframe){
  temp_face_landmarks <- dataframe|>
    select(face_landmarks) 
  
  j <- temp_face_landmarks$face_landmarks     
  
  j<- str_extract_all(j, "(?<=\\[).+?(?=\\])", simplify = T)
  
  for(x in 1:nrow(j)){
    j[x,] <- str_replace_all(j[x,],fixed("["), "")
    j[x,] <- str_squish(j[x,])
    
  }
  
  j<- as.data.frame(j)
  old_names <- colnames(j)
  new_names <- paste0("fl",gsub("[^0-9.-]", "", old_names))
  colnames(j) <- new_names
  dataframe <- bind_cols(dataframe,j)
  dataframe$face_landmarks <- NULL
  rm(j, temp_face_landmarks, new_names, old_names)
  return(dataframe)
}

# unpack speech/dialogue  ------------------------------------------------------------------

unpack_speech_dialogue <- function(dataframe){
  dataframe%>%
    select(frame, text_token) -> predicted_text
  return(predicted_text)
  
}

# load files ------------------------------------------------------------------

load_input <- function(video_path, mexca_path, image_folder_name){
  
  out_df <- read.csv(mexca_path)
  output_folder <- paste0('video_frames_', tools::file_path_sans_ext(image_folder_name))
  
  print('converting video to image sequence')
  dir.create(output_folder)
  framerate <- av_video_info(video_path)$video["framerate"]
  av_video_images(video = video_path, destdir = output_folder, format = 'png', fps = framerate) 
  
  return(out_df)
}


# draw_face_boxes_on_frame ------------------------------------------------------------------

draw_face_boxes_on_frame <- function(mexca_output, facial_landamarks = T, keep_video_frames = F, image_folder_name = NULL, session){
  
  box_minimal <- droplevels(mexca_output[,c('frame', 'time', 'FaceRectX', 'FaceRectY', 'FaceRectWidth', 'FaceRectHeight', names(mexca_output) [grepl( "fl" , names( mexca_output ) )])])
  frames_output_path <- paste0('video_frames_',tools::file_path_sans_ext(image_folder_name))
  
  imgs <- list.files(frames_output_path, full.names = T)
  seq_frames <- 1:length(imgs)
  
  if(length(imgs) == 0){
    stop(paste0('Folder within ', frames_output_path, ' is empty. You may want to load the video first'))
  }
  
  fl_folders <- names(box_minimal) [grepl( "fl" , names( box_minimal ) )]
  
  x <- setNames(data.frame(matrix(ncol = length(fl_folders), nrow = nrow(box_minimal))), c(fl_folders))
  y <- setNames(data.frame(matrix(ncol = length(fl_folders), nrow = nrow(box_minimal))), c(fl_folders))
  x$frame <- box_minimal$frame
  y$frame <- box_minimal$frame
  
  # store x and y coordinates of facial_landmarks
  if (facial_landamarks == T){
    progress <- Progress$new(session, min=0, max=max(seq_frames))
    on.exit(progress$close())
    progress$set(message = 'Extract annotations')
    
    for (i in 1:nrow(box_minimal)){
      progress$set(value = i)
      out_temp <- box_minimal[i,]%>% discard(~all(is.na(.) | . ==""))
      facial_landmarks <- out_temp[ , grepl( "fl" , names( out_temp ) ) ]
      facial_landmarks_df <- setNames(data.frame(facial_landmarks), c(names(out_temp) [grepl( "fl" , names( out_temp ) )]))
      seq_len_fl <- length(colnames(facial_landmarks_df))
      
      if(seq_len_fl != 0){
        for (landmark in colnames(facial_landmarks_df)){
          x[i,landmark] <- strsplit(facial_landmarks_df[,landmark], "\\s+")[[1]][1]
          y[i,landmark] <- strsplit(facial_landmarks_df[,landmark], "\\s+")[[1]][2]            
        }
      }
    }
  }
  
  names_frame_annotated_folder <- paste0("video_frames_annotated_",tools::file_path_sans_ext(image_folder_name))
  dir.create(names_frame_annotated_folder)
  
  img <- purrr::map(imgs, image_read)
  
  print('Overlaying facial landmarks and face boxes on image sequence')
  
  progress <- Progress$new(session, min=0, max=max(seq_frames))
  on.exit(progress$close())
  progress$set(message = 'Annotate video',
               detail = 'This may take a while...')
  
  for (i in seq_frames){
    progress$set(value = i)
    img_plot <- image_draw(img[[i]])
    
    # draw face box
    rect(box_minimal[box_minimal$frame == i,]$FaceRectX, box_minimal[box_minimal$frame == i,]$FaceRectY, box_minimal[box_minimal$frame == i,]$FaceRectWidth, box_minimal[box_minimal$frame == i,]$FaceRectHeight, border = "red", lty = "dashed", lwd = 5)
    
    
    # draw facial landmarks
    if(!all(is.na(x[x$frame == i,]))){ 
      facial_landmarks_columns <- names(x[x$frame == i,]) [grepl("fl" , names( x[x$frame == i,]))]
      
      for(col in facial_landmarks_columns){
        
        symbols(x[x$frame == i,][, col], y[y$frame == i,][, col], circles = rep(2.5, nrow(y[y$frame == i,])),
                bg = 7, inches = FALSE, add = TRUE)
        
      }
    }
    dev.off()
    image_write(img_plot, paste0(names_frame_annotated_folder,"/frame_", i,'.png'))
    
  }
  
  if (keep_video_frames == F){
    unlink('video_frames', recursive = T, force = T)
  }
  
  
}

# plot AUs ------------------------------------------------------------------

plot_AUs <- function(dataframe){
  action_out <- dataframe[,c("frame","time","speaker_id","face_id","text_token","AU01", "AU02", "AU04", "AU06", "AU07", "AU10", "AU12", "AU14", "AU15", "AU17", "AU23", "AU24")]
  action_out$speaker_id <- as.character(action_out$speaker_id)
  
  # create dataset for plotting
  action_out.long <-action_out%>%
    # put all AUs into one column called AU and their values into au_activation
    pivot_longer(cols = c("AU01", "AU02", "AU04", "AU06", "AU07", "AU10", "AU12", "AU14", "AU15", "AU17", "AU23", "AU24"),
                 names_to = c("au"),
                 values_to = "au_activation")%>%
    # adjust type column
    mutate(au = as.factor(au),
           speaker_id = as.factor(speaker_id))%>%
    # convert all empty strings into NAs
    mutate(across(where(is.character), ~ na_if(.,"")))
  
  action_out.long %>%
    ggplot(aes(au, au_activation)) +
    geom_col(fill = '#bdbdbd') +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1, by = 0.2)) +
    theme_bw() +
    labs(x = '', y = 'intensity', title = paste0('Intensity and presence of AUs')) 
  
}

# mexcaR cleanup: delete temporary folders -----------------------------------------------------
cleanup <- function(frame_folder, annotated_frame_folder){
  unlink(frame_folder, recursive = T, force = T)
  unlink(annotated_frame_folder, recursive = T, force = T)
}

# download video ------------------------------------------------------------------------------
download_video <- function(input, file, input_datapath) {
  
  maximum_frame <- floor(av_video_info(input_datapath)$video['framerate'])
  
  images_folder_name <- tools::file_path_sans_ext(input)
  
  imgs <- list.files(paste0('video_frames_annotated_', images_folder_name), full.names = T)
  
  av::av_encode_video(input = gtools::mixedsort(imgs), 
                      output = file, framerate = maximum_frame)
  
}
