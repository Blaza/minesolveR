library(ssoftveR)
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
script_dir <- dirname(frame_files[[length(frame_files)]])
source(paste(script.dir, 'neighbours.R', sep='/'))

#' Generate a minesweeper board
#'
#' @param dims - a vector of matrix dimensions to use for the board
#'               if one number is given, it creates a square matrix
#' @param mines - the number of mines to put in the board
#' @return A valid minesweeper board of dimension dim and mine count of 'mines'
generate_board <- function(dims, mines) {
  if (length(dims) == 1) dims <- c(dims, dims)

  # initialise board as character vector
  board <- character(prod(dims))

  # populate mines at random
  board[sample(1 : (prod(dims)), mines)] <- "m"

  # turn board to matrix
  dim(board) <- dims

  # get neighbourhoods matrix for the board
  neighbours <- get_neighbour_matrix(board)

  # get indices of elements which are not mines, we'll populate them accoridngly
  non_mines <- which(board != "m")

  # subset neighbourhoods matrix to take only neighbourhoods of fields which
  # are not mines
  non_mines_neig <- neighbours[non_mines, ]

  # finally we populate the non-mine fields, by getting the number of mine
  # fields in the neighbourhoods of non-mine fields
  board[non_mines] <- rowSums(non_mines_neig == "m", na.rm = TRUE)

  board
}


#' Test if a board is valid
#'
#' @param board - the minesweeper board to test
#' @param mines - the number of mines the board should have (optional)
#' @return A logical indicating whether the board is valid
valid_board <- function(board, mines =  NULL) {
  # This is basically a inverse process of generate_board.

  # get neighbourhoods matrix for the board
  neighbours <- get_neighbour_matrix(board)

  # get fields which are open (not closed) and not mines
  open_non_mines <- which(board != "m" & board != "z" & board != "n")
  # ... and their neighbourhoods
  open_non_mines_neig <- neighbours[open_non_mines, ]

  # get the number of mines surrounding each field
  mine_counts <- rowSums(open_non_mines_neig == "m", na.rm = TRUE)
  # get the number of closed fields surrounding each field
  closed_counts <- rowSums(open_non_mines_neig == "z", na.rm = TRUE)

  # a board is valid iff the number of mines surrounding a field is the same
  # as the number written on the field or if there are closed fields, the sum
  # of mines and closed fields must be higher than the number on the field
  valid <- all(mine_counts <= board[open_non_mines] &
               board[open_non_mines] <= mine_counts + closed_counts)

  if(!is.null(mines)) {
    valid <- valid && sum(board == "m") <= mines &&
                      sum(board %in% c("m", "z")) >= mines
  }

  valid
}


#' Hide a number of fields at random
#'
#' @param board - the (fully opened) board in which to hide fields
#' @param field_count - the number of fields to hide in addition to hiding the
#'                      mine fields
#' @param A board with hidden mines and field_count fields
hide_random <- function(board, field_count) {
  # get indices of fields which are not mines
  non_mines <- which(board != "m")
  # get a sample of non-mine fields to hide
  hide_inds <- sample(non_mines, field_count)

  # hide mines
  board[board == "m"] <- "z"
  # hide chosen fields
  board[hide_inds] <- "z"

  board
}

