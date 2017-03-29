# From https://gist.github.com/mrdwab/6424112

library(data.table)

stratified <- function(df, group, size, select = NULL, 
                       replace = FALSE, bothSets = FALSE) {
  if (is.null(select)) {
    df <- df
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(df)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) df[[x]] %in% select[[x]])
    df <- df[rowSums(temp) == length(select), ]
  }
  df.interaction <- interaction(df[group], drop = TRUE)
  df.table <- table(df.interaction)
  df.split <- split(df, df.interaction)
  if (length(size) > 1) {
    if (length(size) != length(df.split))
      stop("Number of groups is ", length(df.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- setNames(size, names(df.split))
      # message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
      #         paste(n, collapse = ", "), "),\n.Names = c(",
      #         paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(df.split)),
             n <- size[names(df.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(df.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(df.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(df.table >= size) || isTRUE(replace)) {
      n <- setNames(rep(size, length.out = length(df.split)),
                    names(df.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(df.table[df.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(df.table[df.table >= size], function(x) x = size),
             df.table[df.table < size])
    }
  }
  temp <- lapply(
    names(df.split),
    function(x) df.split[[x]][sample(df.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)
  
  if (isTRUE(bothSets)) {
    set2 <- df[!rownames(df) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}

stratifiedDT <- function(indt, group, size, select = NULL, 
                         replace = FALSE, keep.rownames = FALSE,
                         bothSets = FALSE) {
  if (is.numeric(group)) group <- names(indt)[group]
  if (!is.data.table(indt)) indt <- as.data.table(
    indt, keep.rownames = keep.rownames)
  if (is.null(select)) {
    indt <- indt
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(indt)))
      stop("Please verify your 'select' argument")
    temp <- vapply(names(select), function(x)
      indt[[x]] %in% select[[x]], logical(nrow(indt)))
    indt <- indt[rowSums(temp) == length(select), ]
  }
  df.table <- indt[, .N, by = group]
  df.table
  if (length(size) > 1) {
    if (length(size) != nrow(df.table))
      stop("Number of groups is ", nrow(df.table),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      stop("size should be entered as a named vector")
    } else {
      ifelse(all(names(size) %in% do.call(
        paste, df.table[, group, with = FALSE])),
        n <- merge(
          df.table, 
          setnames(data.table(names(size), ss = size), 
                   c(group, "ss")), by = group),
        stop("Named vector supplied with names ",
             paste(names(size), collapse = ", "),
             "\n but the names for the group levels are ",
             do.call(paste, c(unique(
               df.table[, group, with = FALSE]), collapse = ", "))))
    }
  } else if (size < 1) {
    n <- df.table[, ss := round(N * size, digits = 0)]
  } else if (size >= 1) {
    if (all(df.table$N >= size) || isTRUE(replace)) {
      n <- cbind(df.table, ss = size)
    } else {
      message(
        "Some groups\n---",
        do.call(paste, c(df.table[df.table$N < size][, group, with = FALSE], 
                         sep = ".", collapse = ", ")),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- cbind(df.table, ss = pmin(df.table$N, size))
    }
  }
  setkeyv(indt, group)
  setkeyv(n, group)
  indt[, .RNID := sequence(nrow(indt))]
  out1 <- indt[indt[n, list(
    .RNID = sample(.RNID, ss, replace)), by = .EACHI]$`.RNID`]
  
  if (isTRUE(bothSets)) {
    out2 <- indt[!.RNID %in% out1$`.RNID`]
    indt[, .RNID := NULL]
    out1[, .RNID := NULL]
    out2[, .RNID := NULL]
    list(SAMP1 = out1, SAMP2 = out2)
  } else {
    indt[, .RNID := NULL]
    out1[, .RNID := NULL][]
  }
}