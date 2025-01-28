# Function to calculate common elements for combinations of vectors from a list
find_common_elements <- function(vectors, min_overlap = 1) {
  # Get vector names
  vector_names <- names(vectors)
  if (is.null(vector_names)) {
    vector_names <- paste0("Vector", seq_along(vectors))
  }
  
  n <- length(vectors)
  
  # Check for at least two vectors
  if (n < 2) {
    stop("Please provide at least two vectors.")
  }
  
  # Get all combinations of the vectors (excluding single-vector combinations)
  all_combinations <- lapply(2:n, function(k) combn(seq_along(vectors), k, simplify = FALSE))
  all_combinations <- unlist(all_combinations, recursive = FALSE)
  
  # Find common elements for each combination
  results <- lapply(all_combinations, function(indices) {
    combo_name <- paste0("Common to: ", paste(vector_names[indices], collapse = ", "))
    common <- Reduce(intersect, vectors[indices])
    list(combination = combo_name, elements = common)
  })
  
  # Filter by minimum overlap (optional)
  if (min_overlap > 1) {
    results <- Filter(function(x) length(x$elements) >= min_overlap, results)
  }
  
  # Convert to a data frame for easy readability
  output <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      Combination = res$combination,
      Elements = paste(res$elements, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
  
  return(output)
}