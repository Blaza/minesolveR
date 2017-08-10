library(magrittr)
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
script_dir <- dirname(frame_files[[length(frame_files)]])
source(paste(script_dir, 'boards.R', sep='/'))

#' Mark fields which are surely not mines
#'
#' @param board - the board to solve
#' @return A board with marked sure non-mine fields
find_non_mines <- function(board) {
  if (!valid_board(board)) {
    attr(board, "invalid") <- TRUE
    return(board)
  }

  neimat <- get_neighbour_matrix(board)

  mine_counts <- rowSums(neimat == "m", na.rm = TRUE)

  # get indices of fields which are open (not closed) and not mines
  open_non_mines <- which(board != "m" & board != "z" & board != "n")

  # get fields which can be solved, as the number of mines is the same as the
  # number written on the field
  solvable <- board[open_non_mines] == mine_counts[open_non_mines]
  # .. and using that get indices in the original board (also rows in neimat)
  solv_rows <- open_non_mines[solvable]

  # get coordinate dataframe of fields which are closed but are in solvable rows
  # first get all closed fields coordinates
  closed_coords <- as.data.frame(which(neimat == "z", arr.ind = TRUE))
  # in solvable rows are fields which the first (row) coordinate is in solv_rows
  coords <- closed_coords[closed_coords[ , 1] %in% solv_rows, ]

  # update the neigbourhood matrix with value "n" indicating that there is no
  # mine there for fields with the coords coordinates
  neimat <- update_neighbour_matrix(neimat, "n", coords)

  # return board with marked non mine fields
  board_from_neighbour(neimat)
}


#' Mark fields which are surely mines
#'
#' @param board - the board to solve
#' @return A board with marked sure mines
find_sure_mines <- function(board) {
  # This function is basically the same as find_non_mines, just the logic of
  # checking solvable fields is different
  if (!valid_board(board)) {
    attr(board, "invalid") <- TRUE
    return(board)
  }

  neimat <- get_neighbour_matrix(board)

  mine_counts <- rowSums(neimat == "m", na.rm = TRUE)
  closed_counts <- rowSums(neimat == "z", na.rm = TRUE)

  # get indices of fields which are open (not closed) and not mines
  open_non_mines <- which(board != "m" & board != "z" & board != "n")

  # get fields which can be solved, as the number of mines + the number of
  # closed fields is the same as the number written on the field
  solvable <- board[open_non_mines] == mine_counts[open_non_mines] +
                                       closed_counts[open_non_mines]
  # .. and using that get indices in the original board (also rows in neimat)
  solv_rows <- open_non_mines[solvable]

  # get coordinate dataframe of fields which are closed but are in solvable rows
  # first get all closed fields coordinates
  closed_coords <- as.data.frame(which(neimat == "z", arr.ind = TRUE))
  # in solvable rows are fields which the first (row) coordinate is in solv_rows
  coords <- closed_coords[closed_coords[ , 1] %in% solv_rows, ]

  # update the neigbourhood matrix with value "m" indicating that there is
  # surely a mine there for fields with the coords coordinates
  neimat <- update_neighbour_matrix(neimat, "m", coords)

  # return board with marked mine fields
  board_from_neighbour(neimat)
}


#' Solve the board using basic logic
#'
#' Solves all fields that it can by using the basic rules of the game
#'
#' @param board - the board to solve
#' @param mines - the number of mines on the board
#' @return A (partially) solved board
basic_solve <- function(board, mines) {
  # create (for now) empty partially solved board the same size as board
  partial <- character(prod(dim(board)))
  dim(partial) <- dim(board)

  # until we converge to the same board, keep doing find_non_mines/sure_mines
  while (any(board != partial)) {
    partial <- board
    board <- board %>% find_non_mines %>% find_sure_mines
  }

  # check if we got a valid board for specified number of mines
  if (!valid_board(board, mines)) {
    attr(board, "invalid") <- TRUE
    return(board)
  }

  # check if maybe we found all mines, then all other fields are not mines
  if (mines == sum(board == "m"))
    board[board == "z"] <- "n"

  # check if maybe we found all non-mines, then all other fields are mines
  if (mines - sum(board == "m") == sum(board == "z"))
    board[board == "z"] <- "m"

  # return (partially) solved board
  board
}


#' Solve the board using contradiction logic
#'
#' Solves all fields that it can by trying to put a mine (or non mine) to each
#' field and seeing if that leads to an invalid board after basic solving
#'
#' @param board - the board to solve
#' @param mines - the number of mines on the board
#' @return A (partially) solved board
contradiction_solve <- function(board, mines) {
  if (!valid_board(board)) {
    attr(board, "invalid") <- TRUE
    return(board)
  } else if (sum(board == "z") == 0) {
    # the board is already solved
    return(board)
  }

  neimat <- get_neighbour_matrix(board)

  # get border closed fields (those which have some information around them)
  # first get closed field indices
  closed <- which(board == "z")
  # then number of non_closed fields around them
  non_closed_counts <- rowSums(neimat != "z", na.rm = TRUE)[closed]
  # and get the indices of fields which are closed but have a non-closed around
  border_ind <- closed[non_closed_counts != 0]

  # we'll sort the fields in order to first check those with most information
  border_ind <- border_ind[order(non_closed_counts[non_closed_counts != 0])]

  # try to find which fields cannot be mines by contradiction (if we put a mine
  # there, we get an invalid board after solving, so there can't be a mine)
  # at the same time we check the inverse, if we get a contradiction by putting
  # "n", then there must be a mine there
  # copy the board first
  cp <- board
  for (i in 1 : length(border_ind)) {
    # only try this field if it is closed (it can become solved during the loop)
    if (cp[border_ind[i]] == "z") {
      # set the current element to "m" and see if the board becomes invalid
      cp[border_ind[i]] <- "m"
      if (!is.null(attr(basic_solve(cp, mines), "invalid"))) {
        # if invalid, set that element to "not mine"
        board[border_ind[i]] <- "n"
        # solve the new board as much as you can
        board <- basic_solve(board, mines)
      } else {
        # set the current element to "n" and see if the board becomes invalid
        cp[border_ind[i]] <- "n"
        if (!is.null(attr(basic_solve(cp, mines), "invalid"))) {
          # if invalid, set that element to "not mine"
          board[border_ind[i]] <- "m"
          # solve the new board as much as you can
          board <- basic_solve(board, mines)
        }
      }

      # reset cp to new board
      cp <- board
    }
  }

  # return the newest, solved board
  board
}


#' Solve the board using basic and contradiction logic
#'
#' Solves all fields that it can
#'
#' @param board - the board to solve
#' @param mines - the number of mines on the board
#' @return A (partially) solved board
solve_board <- function(board, mines) {
  board %>% basic_solve(mines) %>% contradiction_solve(mines)
}


#' Get the probabilities of mines being in closed fields
#'
#' @param board - the board to use
#' @param mines - the number of mines in the board
#' @param n - the sample size to use for probabiliy estimation
#' @param pre_solve - whether to run solve_board at the beginning
#' @return A matrix of the same dimension as board, with each element being
#'         the probability that a mine is on the respective field
get_mine_probs <- function(board, mines, n = 1e3, pre_solve = FALSE) {
  if (pre_solve)
    board <- solve_board(board, mines)

  # initialise matrix which will hold probabilities
  prob_matrix <- numeric(prod(dim(board)))
  dim(prob_matrix) <- dim(board)

  # get how many mines are left to be found
  mines_left <- mines - sum(board == "m")

  # get indices of closed fields
  closed_inds <- which(board == "z")

  # create a sample of n configurations of spreading the remaining mines on the
  # remaining closed fields. A configuration is a vector of inndices of fields
  # where to put the mines.
  mine_smp <- replicate(n, sample(closed_inds, mines_left), simplify = FALSE)

  # get the logical vector indicating whether the suggested configuration in
  # each mine_smp element gives a valid board
  valid_smp <- sapply(mine_smp, function(inds, board, mines) {
                        board[inds] <- "m"
                        board[board == "z"] <- "n"
                        valid_board(board, mines)
               }, board, mines)

  # To get the probabilities of mines being in each field, we subset mine_smp
  # to get only valid board samples and then unlist the subset into a vector
  # after which we get the number of appearances of each index in valid boards
  # and dividing that by the number of valid samples to get the probabilities
  prob_mines <- table(unlist(mine_smp[valid_smp])) / sum(valid_smp)

  # Populate the probabilty matrix with caluclated probabilities
  prob_matrix[as.numeric(names(prob_mines))] <- prob_mines

  # the probability of a field with a mine is 1...
  prob_matrix[board == "m"] <- 1
  # ... and of a field which is not a mine (nor closed) is 0
  prob_matrix[!(board %in% c("m", "z"))] <- 0

  prob_matrix
}

