# Rules 

# A live cell dies if it has fewer than two live neighbors.
# A live cell with two or three live neighbors lives on to the next generation.
# A live cell with more than three live neighbors dies.
# A dead cell will be brought back to live if it has exactly three live neighbors.

rm(list=ls())
gc()
initialize_grid <- function(rows, cols) {
  matrix(sample(c(0, 1), rows * cols, replace = TRUE, prob = c(0.9, 0.1)), nrow = rows, ncol = cols)
}



plot_grid <- function(grid) {
  cat("\014") #  ctrl + l
  for (i in 1:nrow(grid)) {
    for (j in 1:ncol(grid)) {
      if (grid[i, j] == 1) {
        cat("O ")
      } else {
        cat(". ")
      }
    }
    cat("\n")
  }
}


update_grid <- function(grid) {
  rows <- nrow(grid)
  cols <- ncol(grid)
  new_grid <- matrix(0, nrow = rows, ncol = cols)
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      # Define the neighborhood indices
      i_min <- max(1, i - 1)
      i_max <- min(rows, i + 1)
      j_min <- max(1, j - 1)
      j_max <- min(cols, j + 1)
      
      # Count live neighbors
      live_neighbors <- sum(grid[i_min:i_max, j_min:j_max]) - grid[i, j]
      
      # Apply rules
      if (grid[i, j] == 1) {
        if (live_neighbors %in% c(2, 3)) {
          new_grid[i, j] <- 1
        }
      } else {
        if (live_neighbors == 3) {
          new_grid[i, j] <- 1
        }
      }
    }
  }
  
  return(new_grid)
}



run_game_of_life <- function(rows, cols, steps) {
  grid <- initialize_grid(rows, cols)
  step <- 0
  for (step in 1:steps) {
    cat("\014") # Clear console
    cat("Step:", step, "\n")
    live_count <- sum(grid)
    dead_count <- rows * cols - live_count
    cat("Live cells:", live_count, "\n")
    cat("Dead cells:", dead_count, "\n")
    for (i in 1:nrow(grid)) {
      for (j in 1:ncol(grid)) {
        if (grid[i, j] == 1) {
          cat("O ")
        } else {
          cat(". ")
        }
      }
      cat("\n")
    }
    grid <- update_grid(grid)
    step <- step + 1
    Sys.sleep(0.5)
  }
}

run_game_of_life(20, 20, 50)

