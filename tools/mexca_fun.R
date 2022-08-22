
# prepare_dataframe tidy-up mexca output ------------------------------------------------------------------
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
# takes the dataframe as input and returns 12 columns with the AUs (based on JAANET model)

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

# write_video ------------------------------------------------------------------
# function to create and save a video from a set of images
write_video <- function(inpath, outpath){
  ## list file names and read in
  imgs <- list.files(inpath, full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined)
  
  ## save to disk
  image_write_video(image = img_animated,
                    path = outpath)
}

# load files ------------------------------------------------------------------

load_input <- function(video_path, mexca_path, image_folder_name){
  
  out_df <- read.csv(mexca_path)
  
  framerate <- av_video_info(video_path)$video["framerate"]
  
  output_folder <- paste0('video_frames_', tools::file_path_sans_ext(image_folder_name))
  
  dir.create(output_folder)
  av_video_images(video = video_path, destdir = output_folder, format = 'png', fps = framerate) 
  
  return(out_df)
}


# draw_face_boxes_on_frame ------------------------------------------------------------------

draw_face_boxes_on_frame <- function(mexca_output, facial_landamarks = T, keep_video_frames = F, image_folder_name = NULL){

  box_minimal <- droplevels(mexca_output[,c('frame', 'time', 'FaceRectX', 'FaceRectY', 'FaceRectWidth', 'FaceRectHeight', names(mexca_output) [grepl( "fl" , names( mexca_output ) )])])
  frames_output_path <- paste0('video_frames_',tools::file_path_sans_ext(image_folder_name))
  
  imgs <- list.files(frames_output_path, full.names = T)
  seq_frames <- 1:length(imgs)
  
  if(length(imgs) == 0){
    stop(paste0('Folder within ', frames_output_path, ' is empty. You may want to load the video first'))
  }
  
  fl_folders <- names(box_minimal) [grepl( "fl" , names( box_minimal ) )]
  
  seq_frames <- 1:length(imgs)
  x <- setNames(data.frame(matrix(ncol = length(fl_folders), nrow = nrow(box_minimal))), c(fl_folders))
  y <- setNames(data.frame(matrix(ncol = length(fl_folders), nrow = nrow(box_minimal))), c(fl_folders))
  
  # draw facial_landmarks
  if (facial_landamarks == T){
    for (i in seq_frames){
      out_temp <- box_minimal[i,]%>% discard(~all(is.na(.) | . ==""))
      facial_landmarks <- out_temp[ , grepl( "fl" , names( out_temp ) ) ]
      facial_landmarks_df <- setNames(data.frame(facial_landmarks), c(names(out_temp) [grepl( "fl" , names( out_temp ) )]))
      seq_len_fl <- length(colnames(facial_landmarks_df))
      
      if(seq_len_fl != 0){
        for (landmark in colnames(facial_landmarks_df)){
          x[i,landmark] <- strsplit(facial_landmarks_df[1,landmark], "\\s+")[[1]][1]
          y[i,landmark] <- strsplit(facial_landmarks_df[1,landmark], "\\s+")[[1]][2]
        }
      }
    }
  }
  
  names_frame_annotated_folder <- paste0("video_frames_annotated_",tools::file_path_sans_ext(image_folder_name))
  dir.create(names_frame_annotated_folder)
  
  img <- purrr::map(imgs, image_read)
  
  for (i in seq_frames){
    img_plot <- image_draw(img[[i]])
    
    # draw face box
    rect(box_minimal$FaceRectX[i], box_minimal$FaceRectY[i], box_minimal$FaceRectWidth[i], box_minimal$FaceRectHeight[i], border = "red", lty = "dashed", lwd = 5)
    
    # draw facial landmarks
    if(!all(is.na(x[i,]))){ 
      for(col in colnames(x[i,])){
        symbols(x[i,col], y[i,col], circles = c(2.5),
                bg = 7, inches = FALSE, add = TRUE)
      }
    }
    image_write(img_plot, paste0(names_frame_annotated_folder,"/frame_", i,'.png'), quality = 300)
    dev.off()
  }
  
  if (keep_video_frames == F){
    unlink('video_frames', recursive = T, force = T)
  }
  
  
}


# draw_face_boxes_on_video ------------------------------------------------------------------
# draw_face_boxes_on_video draws on top of the video specified in 'video_input_path' face boxes and facial landmarks taken from the mexca's output.
# saves the video in the 'video_output_path' as specified in 'name_video_file'

draw_face_boxes_on_video <- function(dataframe, video_input_path, video_output_path, name_video_file = 'example.mp4' ,facial_landamarks = F, write_video = T, keep_video_frames = F){
  
  box_minimal <- droplevels(dataframe[,c('frame', 'time', 'FaceRectX', 'FaceRectY', 'FaceRectWidth', 'FaceRectHeight', names(out) [grepl( "fl" , names( out ) )])])
  framerate <- av_video_info(video_input_path)$video["framerate"]
  
  ## unpack video in a series of png files ##
  frames_output_path <- '_frames/'
  list.files(frames_output_path, full.names = T) -> frames_folder
  
  if(length(list.files(frames_output_path)) != 0){
    stop(paste0('frames within ', frames_output_path, ' already exists'))
  }
  
  # save frames
  dir.create("_frames")
  
  print('processing video')
  av_video_images(video = video_input_path, destdir = frames_output_path, format = 'png', fps = framerate) #fps=.2 output one image for every 5 s
  
  imgs <- list.files(frames_output_path)
  seq_frames <- 1:length(imgs)
  
  ## visualize face boxes and facial landmarks ##
  imgs <- list.files(frames_output_path)
  fl_folders <- names(box_minimal) [grepl( "fl" , names( box_minimal ) )]
  
  seq_frames <- 1:length(imgs)
  x <- setNames(data.frame(matrix(ncol = length(fl_folders), nrow = nrow(box_minimal))), c(fl_folders))
  y <- setNames(data.frame(matrix(ncol = length(fl_folders), nrow = nrow(box_minimal))), c(fl_folders))
  
  if (facial_landamarks == T){
    for (i in seq_frames){
      out_temp <- box_minimal[i,]%>% discard(~all(is.na(.) | . ==""))
      facial_landmarks <- out_temp[ , grepl( "fl" , names( out_temp ) ) ]
      facial_landmarks_df <- setNames(data.frame(facial_landmarks), c(names(out_temp) [grepl( "fl" , names( out_temp ) )]))
      seq_len_fl <- length(colnames(facial_landmarks_df))
      
      if(seq_len_fl != 0){
        for (landmark in colnames(facial_landmarks_df)){
          x[i,landmark] <- strsplit(facial_landmarks_df[1,landmark], "\\s+")[[1]][1]
          y[i,landmark] <- strsplit(facial_landmarks_df[1,landmark], "\\s+")[[1]][2]
        }
      }
    }
  }
  
  
  if (facial_landamarks == T){print('Drawing face boxes and face landmarks on video')} else {print('Drawing face boxes on video')}
  # set up the progress bar
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = max(seq_frames), # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")   # Character used to create the bar
  
  dir.create("_gif")
  for (i in seq_frames){
    setTxtProgressBar(pb, i)
    img <- image_read(paste0(frames_output_path,imgs[i]))
    img_plot <- image_draw(img)
    
    # draw face box
    rect(box_minimal$FaceRectX[i], box_minimal$FaceRectY[i], box_minimal$FaceRectWidth[i], box_minimal$FaceRectHeight[i], border = "red", lty = "dashed", lwd = 5)
    
    # draw facial landmarks
    if(!all(is.na(x[i,]))){ 
      for(col in colnames(x[i,])){
        symbols(x[i,col], y[i,col], circles = c(2.5),
                bg = 7, inches = FALSE, add = TRUE)
      }
    }
    image_write(img_plot, paste0("_gif/", imgs[i]), quality = 300)
    dev.off()
  }
  
  close(pb) # Close the progress bar connection
  
  print('saving video')
  write_video(inpath = "_gif", outpath = paste0(video_output_path, name_video_file))
  
  if (keep_video_frames == F){
    unlink('_gif', recursive = T, force = T)
    unlink(frames_output_path, recursive = T, force = T)
  }
}


# plot AUs ------------------------------------------------------------------
plot_AUs_animated <- function(dataframe){
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
    labs(x = '', y = 'Activation intensity', title = paste0('Trump vs Clinton debate - frame: {frame}')) +
    transition_time(frame) +
    ease_aes('linear')
  
}

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
    labs(x = '', y = 'Activation intensity', title = paste0('Intensity and presence of AUs')) 
  
}

