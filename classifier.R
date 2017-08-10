library(ssoftveR)
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
script_dir <- dirname(frame_files[[length(frame_files)]])
source(paste(script.dir, 'mines_predictors.R', sep='/'))

# we read the saved model from a file
mines_model <- readRDS(paste(script.dir, 'mines_model.RDS', sep='/'))

#' Read a board from a given image
#'
#' @param im - the image from which to extract the board
#' @return The minesweeper board (matrix)
read_board <- function(im) {
  # choose the predictors for the model
  predictors <- c("x_arc_length", "y_arc_length")

  # resize the image to get better results
  im <- im %>% resize(780, 780)

  # Extract the fields from the image. We take the boundaries from the decolored
  # image, and extract the fields from a binarized image (process_img)
  fields <- extract_fields(process_img(im),
                           get_boundaries(decolor(im), prob = 0.95))
  # calculate predictor values for each extracted field
  preds <- get_field_predictors(predictors, fields)

  # predict the class of each field using the loaded model
  classes <- predict(mines_model, preds)

  # return board matrix, we do only 9x9 boards
  matrix(unname(classes), ncol = 9)
}
