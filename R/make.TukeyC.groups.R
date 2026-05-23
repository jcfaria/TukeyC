##
## Function to group means
##
make.TukeyC.groups <- function(x) {
  # x: symmetric logical matrix; FALSE = not significantly different,
  # TRUE = significantly different (rows/columns sorted by decreasing mean)

  traits <- rownames(x)
  n_traits <- length(traits)

  if (n_traits == 0L) {
    return(matrix(
      character(),
      nrow = 0L,
      ncol = 0L
    ))
  }

  established_groups <- list()

  for (i in seq_len(n_traits)) {
    candidate <- which(!x[i, ])

    if (length(candidate) > 2L) {
      validated <- candidate[1L]
      for (j in 2:length(candidate)) {
        potential_member <- candidate[j]
        if (all(!x[potential_member, validated])) {
          validated <- c(
            validated,
            potential_member
          )
        }
      }
      candidate <- validated
    }

    established_groups[[length(established_groups) + 1L]] <- candidate
  }

  established_groups <- established_groups[
    order(
      vapply(
        established_groups,
        length,
        0L
      ),
      decreasing = TRUE
    )
  ]

  unique_groups <- list()
  for (i in seq_along(established_groups)) {
    current_g <- established_groups[[i]]
    if (length(unique_groups) > 0L) {
      is_subset <- vapply(
        unique_groups,
        function(saved_g) {
          all(current_g %in% saved_g)
        },
        logical(1L)
      )
      if (any(is_subset)) {
        next
      }
    }
    unique_groups[[length(unique_groups) + 1L]] <- current_g
  }

  first_appearance <- vapply(
    unique_groups,
    min,
    0L
  )
  unique_groups <- unique_groups[order(first_appearance)]

  n_groups <- length(unique_groups)
  group_matrix <- matrix(
    "",
    nrow = n_traits,
    ncol = n_groups
  )
  rownames(group_matrix) <- traits
  colnames(group_matrix) <- paste0(
    "G",
    seq_len(n_groups)
  )

  alphabet <- c(
    letters,
    LETTERS,
    do.call(
      paste0,
      expand.grid(
        letters,
        letters
      )
    ),
    do.call(
      paste0,
      expand.grid(
        LETTERS,
        LETTERS
      )
    )
  )

  for (g in seq_len(n_groups)) {
    group_matrix[unique_groups[[g]], g] <- alphabet[g]
  }

  return(group_matrix)
}
