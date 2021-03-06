library(nnet)
library(imager)
library(ssoftveR)
frame_files <- lapply(sys.frames(), function(x) x$ofile)
frame_files <- Filter(Negate(is.null), frame_files)
script_dir <- dirname(frame_files[[length(frame_files)]])
source(paste(script_dir, 'mines_predictors.R', sep='/'))

files <- Sys.glob(paste(script_dir, "mines_img/*.png", sep = '/'))
images <- lapply(files, load.image)

cat("Loaded images\n")

ext_fields <- lapply(images, function(im)
                     extract_fields(im, get_boundaries(decolor(im))))

fields_combined <- do.call(c, ext_fields)

fields <- lapply(fields_combined, function(field) field$image)
cat("Extracted fields for manual entry\n")

# free up memory, we only need fields
rm(ext_fields)
rm(fields_combined)

# Manual setting classes for training set
tr_fname <- paste(script_dir, "tr_cls.RDS", sep = '/')
tr_cls <- character(length(fields))
if (file.exists(tr_fname)) {
  tr_cls_tmp <- readRDS(tr_fname)
  tr_cls[1:length(tr_cls_tmp)] <- tr_cls_tmp
}

cat("Beginning manual entry\n")
for (i in 1:length(tr_cls)) {
  if(nchar(tr_cls[i]) != 1) {
    cat(paste("Entry", i, "of", length(tr_cls)), "\n")
    plot(fields[[i]])
    tr_cls[i] <- readline(prompt = "Enter class abbr: ")
    saveRDS(tr_cls, tr_fname)
  }
}
cat("Finished manual entry\n")

# Calculate predictors for fields

predictors <- c("x_arc_length", "y_arc_length")

ext_fields <- lapply(images, function(im) {
                   im <- im %>% resize(780, 780)
                   extract_fields(process_img(im),
                                  get_boundaries(decolor(im), prob = 0.95))
              })

fields <- do.call(c, ext_fields)
cat("Extracted fields for predictor calculation\n")

# free up memory, we only need fields
#rm(images)
rm(ext_fields)

tr_preds <- get_field_predictors(predictors, fields, FALSE)
cat("Calculated predictors\n")


cat("Creating multinom model\n")
# genetare training dataframe
dat <- cbind(tr_preds, class = tr_cls)

mines_model <- multinom(class ~ . , data = dat, maxit = 1000)

#saveRDS(mines_model, paste(script_dir, "mines_model.RDS", sep = '/'))
cat("ALL DONE\n")

