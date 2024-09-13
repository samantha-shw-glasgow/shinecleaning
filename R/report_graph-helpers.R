#' Print multiple graphs across chunks
#'
#' `print_first_graph` prints the first graph and returns the remaining in a list
#' `print_rest_of_graphs` prints the rest
#'
#' @param graph_list List of graphs to print
#'
#' @return `print_first_graph` invisibly returns list of graphs with first dropped
print_first_graph <- function(graph_list) {

  print(graph_list[[1]])

  graph_list[[1]] <- NULL

  invisible(graph_list)
}

#' @rdname print_first_graph

print_rest_of_graphs <- function(graph_list) {

  walk(graph_list, print)

}
