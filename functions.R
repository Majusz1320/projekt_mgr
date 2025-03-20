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


find_uncommon_elements <- function(vectors, min_overlap = 1) {
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
  
  # Check for maximum of 4 vectors
  if (n > 4) {
    warning("This function is designed for up to 4 vectors. Using only the first 4 vectors.")
    vectors <- vectors[1:4]
    vector_names <- vector_names[1:4]
    n <- 4
  }
  
  # Create a result list
  results <- list()
  
  # Process each vector to find elements unique to it
  for (i in 1:n) {
    other_indices <- setdiff(1:n, i)
    other_vectors_union <- Reduce(union, vectors[other_indices])
    unique_elements <- setdiff(vectors[[i]], other_vectors_union)
    
    results[[i]] <- list(
      combination = paste0("Unique to: ", vector_names[i]),
      elements = unique_elements
    )
  }
  
  # Convert to a data frame for easy readability
  output <- do.call(rbind, lapply(results, function(res) {
    data.frame(
      Combination = res$combination,
      Elements = paste(res$elements, collapse = ", "),
      Count = length(res$elements),
      stringsAsFactors = FALSE
    )
  }))
  
  return(output)
}