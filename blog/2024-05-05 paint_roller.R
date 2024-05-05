library(tidyverse)
library(gridExtra)

# Letters denote rows, numbers denote columns.
# For example, the sequence "A3" means paint the first row, then paint
# the third column.

# Generate a random sequence for painting a board of given dimensions.
generate_random_sequence <- function(row_count, col_count) {
  characters <- c(LETTERS[1:row_count], as.character(1:col_count))
  paste(sample(characters), collapse = "")
}

# Check if sequence is valid. Doesn't have to be length row_count + col_count,
# but each character must be valid and cannot have duplicates.
is_valid_sequence <- function(sequence, row_count, col_count) {
  characters <- strsplit(sequence, "")[[1]]
  valid_characters <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890", "")[[1]]
  if (any(duplicated(characters))) {
    print("Sequence cannot have duplicated characters!")
    return(FALSE)
  } else {
    for (ch in characters) {
      if (!(ch %in% valid_characters)) {
        print("Characters must be uppercase letters or numbers!")
        return(FALSE)
      } else if (ch %in% LETTERS) {
        row_index <- match(ch, LETTERS)
        if (row_index > row_count) {
          print(paste0("Letters must be between A and ", LETTERS[row_count], "!"))
          return(FALSE)
        }
      } else {
        col_index <- as.numeric(ch)
        if (col_index > col_count) {
          print(paste0("Numbers must be between 1 and ", col_count, "!"))
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
}

# Paint board according to the sequence
paint_board <- function(sequence, row_count, col_count) {
  if (!is_valid_sequence(sequence, row_count, col_count))
    stop("Sequence is invalid")
  characters <- strsplit(sequence, "")[[1]]
  
  board <- matrix(0, nrow = row_count, ncol = col_count)
  for (ch in characters) {
    if (ch %in% LETTERS) {
      row_to_paint <- match(ch, LETTERS)
      board[row_to_paint, ] <- row_to_paint
    } else {
      col_to_paint <- as.numeric(ch)
      board[, col_to_paint] <- row_count + col_to_paint
    }
  }
  board
}

# Test if two boards match
check_board <- function(actual_board, expected_board) {
  if (nrow(actual_board) != nrow(expected_board)) stop("# rows don't match!")
  if (ncol(actual_board) != ncol(expected_board)) stop("# columns don't match!")
  return(all(actual_board == expected_board))
}

# Check if two sequences give the same board. Needed because two different
# sequences can give the same board
check_sequence <- function(actual_sequence, expected_sequence, row_count, col_count) {
  actual_board <- paint_board(actual_sequence, row_count, col_count)
  expected_board <- paint_board(expected_sequence, row_count, col_count)
  check_board(actual_board, expected_board)
}

# Convert paint board (matrix) into a data frame, to be used for making image.
convert_board_to_df <- function(board) {
  df <- expand.grid(row = 1:nrow(board), column = 1:ncol(board))
  df$val <- as.vector(board)
  df
}

# Get data frame representing rollers, to be used for making image.
get_df_for_rollers <- function(sequence, row_count, col_count) {
  if (!is_valid_sequence(sequence, row_count, col_count))
    stop("Sequence is invalid")
  characters <- strsplit(sequence, "")[[1]]
  
  text_offset <- 0.4
  
  # rollers for empty board
  df <- data.frame(
    row = c(1:row_count, rep(0, col_count)),
    column = c(rep(0, row_count), 1:col_count),
    val = 1:(row_count + col_count),
    label = c(LETTERS[1:row_count], as.character(1:col_count)),
    label_x = c(rep(0.5 - text_offset, row_count), 1:col_count + 0.5),
    label_y = c(1:row_count + 0.5, rep(0.5 - text_offset, col_count))
  )
  
  for (ch in characters) {
    if (ch %in% LETTERS) {
      df[df$label == ch, "column"] <- df[df$label == ch, "column"] + col_count + 1
      df[df$label == ch, "label_x"] <- df[df$label == ch, "label_x"] + col_count + 1
    } else {
      df[df$label == ch, "row"] <- df[df$label == ch, "row"] + row_count + 1
      df[df$label == ch, "label_y"] <- df[df$label == ch, "label_y"] + row_count + 1
    }
  }
  df
}

# Convenience function for making text result image
make_text_plot <- function(text_to_print, text_color) {
  df <- data.frame(x = 0, y = 0, label = text_to_print)
  text_color <- text_color
  ggplot(df) +
    geom_text(aes(x = x, y = y, label = label),
              col = text_color, fontface = "bold", size = 5) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")
}

# make plot of the board along with the rollers
make_image <- function(sequence, row_count, col_count, plot_title = NULL,
                       color_list = NULL) {
  # paint the board
  board <- paint_board(sequence, row_count, col_count)
  
  # convert boards into data frames
  board_df <- convert_board_to_df(board)
  roller_df <- get_df_for_rollers(sequence, row_count, col_count)
  
  # get color list and add white to it
  # need to make adjustments to color values to accommodate white
  if (is.null(color_list)) color_list <- rainbow(row_count + col_count)
  color_list <- c("white", color_list)
  board_df$val <- color_list[board_df$val + 1]
  roller_df$val <- color_list[roller_df$val + 1]
  
  padding <- 0.1
  roller_padding <- 0.3
  ggplot() +
    geom_rect(aes(xmin = column + padding, xmax = column + 1 - padding,
                  ymin = -(row + padding), ymax = -(row + 1 - padding), fill = val),
              data = board_df, col = "black") +
    geom_rect(aes(xmin = column + roller_padding, xmax = column + 1 - roller_padding,
                  ymin = -(row + roller_padding), ymax = -(row + 1 - roller_padding), fill = val),
              data = roller_df, col = "black") +
    geom_text(aes(x = label_x, y = -label_y, label = label), data = roller_df) +
    labs(title = plot_title) +
    scale_fill_identity() +
    coord_fixed(xlim = c(0, col_count + 2), ylim = c(-(row_count + 2), 0)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          legend.position = "none")
}

# Print series of images to show how the board evolves
show_entire_sequence <- function(sequence, row_count, col_count,
                                 color_list = NULL) {
  plot_list <- list(
    make_image("", row_count, col_count, color_list,
               plot_title = "Sequence:")
  )
  
  for (i in 1:nchar(sequence)) {
    subsequence <- substr(sequence, 1, i)
    plot_list[[i + 1]] <- make_image(subsequence, row_count, col_count, color_list,
                                     plot_title = paste("Sequence:", subsequence))
  }
  
  grid.arrange(grobs = plot_list, ncol = round(1.5 * sqrt(nchar(sequence))))
}

# Simulate a game of paint roller
play_game <- function(row_count, col_count, color_list = NULL) {
  # get color list
  if (is.null(color_list)) {
    color_list <- rainbow(row_count + col_count)
  } else {
    if (length(color_list) != row_count + col_count)
      stop("color_list should have length `row_count + col_count`!")
  }
  
  # create and print start and end boards
  start_image <- make_image("", row_count, col_count, plot_title = "Start",
                            color_list = color_list)
  true_sequence <- generate_random_sequence(row_count, col_count)
  end_image <- make_image(true_sequence, row_count, col_count, plot_title = "End",
                          color_list = color_list)
  grid.arrange(start_image, end_image, ncol = 2)
  
  is_sequence_correct <- FALSE
  while (!is_sequence_correct) {
    # get valid input sequence, end game if input is Q
    while (TRUE) {
      input_sequence <- readline(paste0("Enter guess (Q to quit): "))
      if (input_sequence == "Q" || 
          is_valid_sequence(input_sequence, row_count, col_count))
        break
    }
    if (input_sequence == "Q") break
    
    # create boards for the input sequence
    is_sequence_correct <- check_sequence(input_sequence, true_sequence,
                                          row_count, col_count)
    if (is_sequence_correct) {
      text_to_print <- paste("Correct, you win!\nSequence:", true_sequence)
      text_color <- "black"
    } else {
      text_to_print <- "Incorrect, try again!"
      text_color <- "red"
    }
    text_image <- make_text_plot(text_to_print, text_color)
    input_image <- make_image(input_sequence, row_count, col_count,
                              plot_title = paste("Sequence:", input_sequence),
                              color_list = color_list)
    
    grid.arrange(start_image, end_image, text_image, input_image, ncol = 2)
  }
  
  if (!is_sequence_correct) {
    # If we are here, it's because we quit the game
    text_image <- make_text_plot(
      text_to_print = paste("Try again next time!\nSequence:", true_sequence),
      text_color = "red")
    grid.arrange(start_image, end_image, text_image, ncol = 2)
  }
}

# For reproducing header example
show_entire_sequence("1B3A2C", row_count = 3, col_count = 3,
                     color_list = c("green", "cyan", "magenta", "red", "orange", "yellow"))

# play game
set.seed(1)
play_game(row_count = 4, col_count = 4)

# play game with custom colors
set.seed(1)
play_game(row_count = 3, col_count = 4,
          color_list = c("green", "cyan", "magenta", "red", "orange", "yellow", "black"))

# draw a single board
make_image("AC1", row_count = 4, col_count = 3)

# show a sequence of steps
show_entire_sequence("A3BC21", row_count = 4, col_count = 4)
