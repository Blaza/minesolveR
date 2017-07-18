get_neighbour_matrix <- function(board) {
  # get each fields' neighbourhood in rows (we slide the matrix in all
  # directions and combine them into a matrix using slide_matrix from ssoftveR)
  directions <- expand.grid(row = -1 : 1, col = -1 : 1)
  neighbours <- slide_matrix(board, directions, simplify = TRUE)

  # add information about the original board size
  attr(neighbours, "board_dim") <- dim(board)

  # We'll reverse the vectors written as columns names, because they represent
  # the direction in which we did the sliding, while we want it here to
  # represent the direction in which to get to the given element from the middle
  # element of the neighbourhood
  colnames(neighbours) <- rev(colnames(neighbours))

  neighbours
}


get_index_structure <- function(neimat) {
  h <- attr(neimat, "board_dim")[1]
  w <- attr(neimat, "board_dim")[2]

  dirs <- colnames(neimat)

  index_structure <- sapply(1 : (h * w), function(i) {
                         sapply(dirs, function(dir) {
                             # reject out of bounds values
                             if (is.na(neimat[i, which(dirs == dir)]))
                               return(NA)

                             # extract row and col coordinates of direction from
                             # the colname string (of the form "(i, j)" )
                             regex <- "^\\((-?\\d),\\ ?(-?\\d)\\)$"
                             row <- as.numeric(gsub(regex, "\\1", dir))
                             col <- as.numeric(gsub(regex, "\\2", dir))

                             # the index in the original matrix is the current
                             # index + the row coordinate of direction + height*
                             # col coordinate of direction. This is because
                             # matrices are stored column-wise, so each row move
                             # is just addition, while column move needs to
                             # go over the whole current column to get to the
                             # wanted column
                             i + row + h * col
                         })
                     })

  # we return transpose to match the dimension of neimat
  t(index_structure)
}


update_neighbour_matrix <- function(neimat, value, coords) {
  index_structure <- get_index_structure(neimat)

  covered_indices <- na.omit(index_structure[as.matrix(coords)])

  indices <- which(index_structure %in% covered_indices)

  neimat[indices] <- value

  neimat
}


board_from_neighbour <- function(neimat) {
  h <- attr(neimat, "board_dim")[1]
  matrix(neimat[ , 5], nrow = h)
}

