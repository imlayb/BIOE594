library(foreach)
library(stringr)
library(doSNOW)

importFaceMatrix <- function(verbose = FALSE, datadir = "rawdata") {
  cl <- makeCluster(4)
  registerDoSNOW(cl)
  infiles <- Sys.glob(file.path(datadir, "*"))
  if (verbose) {
    pb <- txtProgressBar(max = length(infiles), style = 3)
    progress <- function(n)
      setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
  }
  else{
    opts <- NULL
  }
  dat <-
    foreach(raw_file = infiles,
            .combine = "rbind",
            .options.snow = opts) %dopar% {
              finfo <- file.info(raw_file)
              rb_file <- file(raw_file, "rb")
              on.exit(close(rb_file))
              v <-
                as.integer(readBin(
                  rb_file,
                  raw(),
                  size = 1,
                  n = finfo$size,
                  endian = "little"
                ))
              return(v)
            }
  sample_names <- str_remove(infiles, "[:alpha:]+\\/")
  dimnames(dat)[[1]] <- sample_names
  content_bearing <- rowSums(dat) > 0
  dat <- dat[content_bearing, ]
  if (verbose)
    close(pb)
  stopCluster(cl)
  if (verbose) {
    message(
      sprintf(
        "Imported %d faces of lengths %d. Removed %d for having no parsed content.",
        nrow(dat),
        ncol(dat),
        length(infiles) - nrow(dat)
      )
    )
  }
  return(dat)
}
importMetaMatrix <- function(file, verbose = FALSE) {
  x <- read.csv(file, header = FALSE, sep = "(")
  x <- x[, -6]
  features <- c("n", "sex", "age", "race", "face", "prop")
  names(x) <- features
  for (f in features[-6]) {
    x[[f]] <- str_remove(x[[f]], "_[:alpha:]+[:blank:]")
    x[[f]] <- str_remove(x[[f]], "\\)")
  }
  x$prop <- str_extract(x$prop, "[:alpha:]+|[:alpha:]+\\s[:alpha:]")
  before <- nrow(x)
  complete_sample <- x$sex != "descriptor"
  x <- x[complete_sample, ]
  after <- nrow(x)
  if (verbose) {
    message(
      sprintf(
        "Imported metadata for %d samples from file: \"%s\".\nRemoved %d samples without metadata.",
        nrow(x),
        file,
        before - after
      )
    )
  }
  return(x)
}

plotImage <- function(v, title = NULL,color_scale=gray.colors(
  256,
  start = 0.3,
  end = 0.9,
  gamma = 2.2,
  alpha = NULL
)) {
  d <- matrix(v, nrow = sqrt(length(v)))
  return(image(
    d[, nrow(d):1],
    col = color_scale,
    axes = FALSE,
    main = title
  ))
}

#dat <- importFaceMatrix(verbose = TRUE)
#meta_TR<-importMetaMatrix("faces/faceDR",verbose=FALSE)
#meta_T<-importMetaMatrix("faces/faceDS",verbose=FALSE)