"
Below script was obtained from PCIbex Documentation: https://doc.pcibex.net/how-to-guides/data-transformation.html
"
read.pcibex <- function(filepath, auto.colnames = TRUE, fun.col = function(col, cols) {
                          cols[cols == col] <- paste(col, "Ibex", sep = ".")
                          return(cols)
                        }) {
  n.cols <- max(count.fields(filepath, sep = ",", quote = NULL), na.rm = TRUE)
  if (auto.colnames) {
    cols <- c()
    con <- file(filepath, "r")
    while (TRUE) {
      line <- readLines(con, n = 1, warn = FALSE)
      if (length(line) == 0) {
        break
      }
      m <- regmatches(line, regexec("^# (\\d+)\\. (.+)\\.$", line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)) {
          cols <- fun.col(value, cols)
        }
        cols[index] <- value
        if (index == n.cols) {
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char = "#", header = FALSE, col.names = cols))
  } else {
    return(read.csv(filepath, comment.char = "#", header = FALSE, col.names = seq(1:n.cols)))
  }
}

# Read the raw CSV obtained from PCIbex
current_dir <- getwd()
parent_dir <- dirname(current_dir)
file_path <- file.path(parent_dir, "False-Friends/csv", "FalseFriendsData.csv")
results <- read.pcibex(file_path)

# Save to csv
output_file_path <- file.path(parent_dir, "False-Friends/csv", "cleaned.csv")
write.csv(results, output_file_path, row.names = FALSE)
