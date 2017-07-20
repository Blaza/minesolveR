suppressWarnings(library(imager, warn.conflicts = FALSE, quietly=TRUE))
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
source(paste(script.dir, 'player.R', sep='/'))

fname <- function(i) {
  paste0("screens/screen",i,".png")
}

make_move <- function(i, j, cnt, anim=FALSE, wait_time=100) {
  system2("automine.exe", c(i, j, fname(cnt), ifelse(anim,"anim","noanim"), wait_time), stdout = TRUE)
}

best_move <- best_move_gen()

moves <- list(c(1,1),c(9,9),c(1,9),c(9,1))

i <- 1
result <- make_move(5,5,i,anim=TRUE)

while(result == "ok") {
  im <- load.image(fname(i))
  ij <- best_move(im)
  print(ij)
  i <- i + 1
  result <- make_move(ij[1], ij[2], i)
}


print(result)