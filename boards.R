library(ssoftveR)
script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'neighbours.R', sep='/'))

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

