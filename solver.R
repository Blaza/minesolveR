library(magrittr)
script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'boards.R', sep='/'))

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


solve_board <- function(board, mines) {
  board %>% basic_solve(mines) %>% contradiction_solve(mines)
}


# probabiliy that a board is valid if there is a minne put
valid_probs <- function(board, mines, n = 1e3, pre_solve = FALSE) {
  if (pre_solve)
    board <- solve_board(board, mines)

  # initialise a matrix which will hold probailities
  prob_board <- numeric(prod(dim(board)))
  dim(prob_board) <- dim(board)

  closed_inds <- which(board == "z")

  probs <- sapply(closed_inds, function(ind, board, mines, n) {
               # set the current field to a mine
               board[ind] <- "m"
               mines_left <- mines - sum(board == "m")

               # get values indicating whether the board is valid if we set a
               # mine at ind
               vals <- replicate(n, {
                           # get indices of closed fields
                           closed <- which(board == "z")
                           # set remaining mines randomly on those closed fields
                           board[sample(closed, mines_left)] <- "m"
                           # set all other fields to non-mine
                           board[board == "z"] <- "n"

                           valid_board(board, mines)
                       })
               # return the probability of there being a mine, i.e. the number
               # of times the board was valid given a mine was there
               mean(vals)
           }, board, mines, n)

  # populate fields with probabilities of there being a mine
  prob_board[closed_inds] <- probs

  # on sure non-mines, the probability of a mine is 0
  prob_board[!(board %in% c("m", "z"))] = 0

  # on sure mines, the probability of a mine is 1
  prob_board[board == "m"] = 1

  round(prob_board, 2)
}

