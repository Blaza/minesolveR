script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'solver.R', sep='/'))

#' Get probabilities of solving a board for a determined number of closed fields
#'
#' @param auth_file - the Google Compute Engine Service account key json file
#'                    to use for authentication. Full path should be given.
#' @param n - the number of boards to create to approximate the probabilities
get_solving_probs <- function(auth_file, n = 160) {
  # if an auth file is given, use it
  if(!missing(auth_file))
    Sys.setenv("GCE_AUTH_FILE" = auth_file)
  # set up project and zone for gce
  Sys.setenv("GCE_DEFAULT_PROJECT_ID" = "ssoftver")
  Sys.setenv("GCE_DEFAULT_ZONE" = "europe-west1-b")
  library(googleComputeEngineR)
  library(future)

  # specify the number of closed fields for each dimension and generate n
  # boards for each dimension
  setup_dims <- list("d9x9" = list(boards = replicate(n, generate_board(9, 10),
                                                      simplify = FALSE),
                                   hidden = seq(3, 30, 3)),
                     "d16x16" = list(boards = replicate(n,
                                                        generate_board(16, 40),
                                                        simplify = FALSE),
                                   hidden = seq(25, 70, 5)))

  # Setting up the GCE instances
  cat("Launching VMs...\n")
  # get the docker image we'll use for instances
  gce_img_tag <- gce_tag_container("ssoftver", container_url = "eu.gcr.io")
  # we use preemtible instances to get a much lower price
  preemptible <- list(preemptible = TRUE)

  # Create the instances for 9x9 and 16x16 board dimensions.
  # We create 2 instances (virtual machines), one will do the sampling and
  # calculation for 9x9 boards (with 4 cpus, running in parallel) and the other
  # for 16x16 boards (with 20 cpus, parallel). We chose 4 and 20 cpus after a
  # bit of experimentation and turned out that that works nicely and finishes
  # all jobs in about 10-15 minutes
  vms <- list("d9x9" = gce_vm("d9x9", cpus = 4, memory = 3840,
                              template = "r-base",
                              dynamic_image = gce_img_tag,
                              scheduling = preemptible,
                              predefined_type = "custom-4-3840"),
              "d16x16" = gce_vm("d16x16", cpus = 20, memory = 18432,
                              template = "r-base",
                              dynamic_image = gce_img_tag,
                              scheduling = preemptible,
                              predefined_type = "custom-20-18432"))

  cat("Setting up SSH...\n")
  # we set up SSH access for the instances
  vms <- lapply(vms, gce_ssh_setup)

  cat("Waiting for docker to pull the image...\n")
  # We must wait a bit while docker pulls the ssoftver image into the instance
  # and then we can proceed to use that image
  # We are checking every 5 seconds whether all of the vms have a docker image
  # starting with the name gce_img_tag ("eu.gcr.io/ssoftver/ssoftver"). After
  # all instances have that image, we can proceed further.
  while (!all(sapply(vms, function(vm) {
                any(startsWith(docker_cmd(vm, "images", capture_text = TRUE),
                               gce_img_tag))
              }))) {
    Sys.sleep(5)
  }

  cat("Making a future plan...\n")
  # We are creating a future (that's a package) plan that specifies how the
  # instances should be used and what kind of execution we want for e.g.
  # future_lapply (sequential or parallel basically).
  # We are making a cluster of 2 machines (given by vms) and on each machine
  # we use a 'multiprocess' plan, which means we are running the jobs in
  # parallel on each machine.
  docker_im <- "eu.gcr.io/ssoftver/ssoftver"
  plan(list(tweak(cluster, workers = as.cluster(vms, docker_image = docker_im)),
            multiprocess))

  cat("Running samples...\n")

  # Now we do the probability estimation
  # First future_lapply splits the job into two and sends each board dimension
  # to it's own vm.
  # The second future_lapply splits the generated n boards into several
  # processes given by the number of cpus in the instance and does the
  # calculation for each board.
  future_lapply(setup_dims, function(dim) {
      brd <- future_lapply(dim$boards, function(board) {
                 # For each board we get a logical vector indicating whether we
                 # were able to solve the board after closing the specified
                 # number of fields (given by dim$hidden)
                 h <- sapply(dim$hidden, function(hide_count) {
                          mines <- sum(board == "m")
                          solv <- solve_board(hide_random(board, hide_count),
                                              mines)

                          # return a logical indicating if all fields are solved
                          # and the board is still valid
                          all(solv != "z") && valid_board(solv, mines)
                      })
                 names(h) <- as.character(dim$hidden)

                 h
             })

      # put the data into a data.frame (each board results in its column) and
      # take row means (to get the percent of successful solvings for each
      # number of hidden mines)
      rowMeans(data.frame(brd))
  }) -> ret # note that we're putting the result into the ret variable
            # gotta love R multi directional assignment operator

  # Finally we must stop the GCE instances as they are not needed anymore.
  lapply(vms, gce_vm_stop)

  # return the future_lapply result
  ret
}

