library(ssoftveR)
script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'mines_predictors.R', sep='/'))

mines_model <- readRDS(paste(script.dir, 'mines_model.RDS', sep='/'))

read_board <- function(im) {
  predictors <- c("row_dominant_mode", "col_dominant_mode")

  fields <- extract_fields(process_img(im), get_boundaries(decolor(im)))
  preds <- get_field_predictors(predictors, fields)

  classes <- predict(mines_model, preds)

  # return board matrix, we do only 9x9 boards
  matrix(unname(classes), ncol = 9)
}
