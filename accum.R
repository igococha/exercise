

mkRow <- function(nCol) {
  x <- as.list(rnorm(nCol))
  # make row mixed types by changing first column to string
  x[[1]] <- ifelse(x[[1]]>0,'pos','neg')
  names(x) <- paste('x',seq_len(nCol),sep='.')
  x
}


# the R way
mkFrameList <- function(nRow,nCol) {
  d <- lapply(seq_len(nRow),function(i) {
    ri <- mkRow(nCol)
    data.frame(ri,
               stringsAsFactors=FALSE)
  })
  do.call(rbind,d)
}

# for loop with 'update' / need to know length?
mkFrameForList <- function(nRow,nCol) {
  d <- as.list(seq_len(nRow))
  for(i in seq_len(nRow)) {
    ri <- mkRow(nCol)
    di <- data.frame(ri,
                     stringsAsFactors=FALSE)
    d[[i]] <- di
  }
  do.call(rbind,d)
}

# faster:
# replace: do.call(rbind,d) with data.table::rbindlist(d)

mkRowList4 <- function() {
  x <- as.list(rnorm(4))
  names(x) <- c('a','b','c','d')
  return(x)
}

# this works - mkRowList
mkFrameListfromList4 <- function(nRow) {
  d <- lapply(seq_len(nRow),function(i) {
    ri <- mkRowList4()
    data.frame(ri,
               stringsAsFactors=FALSE)
  })
  do.call(rbind,d)
}

