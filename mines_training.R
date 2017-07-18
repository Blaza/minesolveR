library(imager)
library(ssoftveR)
script.dir <- dirname(sys.frame(1)$ofile)
source(paste(script.dir, 'mines_predictors.R', sep='/'))
# see mines_predictors.R
decolor <- function(im) {
  thr <- im %>% threshold
  as.cimg(3 * thr[ , , 1, 1] + 2 * thr[ , , 1, 2] + thr[ , , 1, 3])
}

files <- Sys.glob("~/projects/stats/ss4/mines_img/*.png")
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
tr_fname <- "~/projects/stats/ss4/tr_cls.RDS"
tr_cls <- character(length(fields))
if (file.exists(tr_fname))
  tr_cls <- readRDS(tr_fname)

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

predictors <- c("white_area", "widths_variance")

process_img <- function(im) im %>% isoblur(., 2.16) %>% binary

ext_fields <- lapply(images, function(im)
                   extract_fields(process_img(im), get_boundaries(decolor(im))))

fields <- do.call(c, ext_fields)
cat("Extracted fields for predictor calculation\n")

# free up memory, we only need fields
#rm(images)
rm(ext_fields)

tr_preds <- get_field_predictors(predictors, fields, FALSE)
cat("Calculated predictors\n")

