script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'solver.R', sep='/'))
source(paste(script.dir, 'classifier.R', sep='/'))

best_move_gen <- function() {
  move_stack <- data.frame(row, col)

  function(im) {
    # if there are no moves in the stack, try to find some
    if (dim(move_stack)[1] == 0) {
      board <- read_board(im)
      # first try basic solving
      board <- basic_solve(board, 10)
      move_stack <<- which(board == "n", arr.ind = TRUE)
      print("Basic solving")
      # if that gives no results, try contradiction solving too
      if(dim(move_stack)[1] == 0) {
        print("Contradiction solving")
        board <- solve_board(board, 10)
        move_stack <<- which(board == "n", arr.ind = TRUE)
      }
    }

    # if move_stack still empty, pick the first closed and hope for the best
    if (dim(move_stack)[1] == 0) {
      print("Guessing")
      move_stack <<- which(board == "z", arr.ind = TRUE)[1, , drop = FALSE]
    }
    
    # get the first element from the stack and remove it from the stack
    next_move <<- move_stack[1, , drop = FALSE]
    move_stack <<- move_stack[-1, , drop = FALSE]

    # return next move
    next_move
  }
}

best_move <- best_move_gen()

