# Load necessary libraries
library(dplyr)
library(gridExtra)
library(grid)

# Create sample data frame
df <- data.frame(
  id = 1:20,
  name = c("Alex", "Ben", "Cathy", "David", "Eliza", "Frank", "George", "Henry", "Irene", "Jack",
           "Ken", "Liza", "Mona", "Nina", "Oscar", "Paul", "Quincy", "Rita", "Taka", "Uma"),
  score = c(75, 85, 65, 95, 55, 80, 78, 62, 90, 81,
            77, 93, 60, 87, 88, 74, 82, 69, 99, 68),
  comment = rep("Good", 20)
)

# Filter for top 5, bottom 5, and the row with 'Taka' in the name
top_5 <- df %>% arrange(desc(score)) %>% head(5)
bottom_5 <- df %>% arrange(score) %>% head(5)
taka_row <- df %>% filter(grepl("taka", name, ignore.case = TRUE))

# Combine these rows
selected_rows <- bind_rows(top_5, taka_row, bottom_5)

# Sort selected rows by their original order
selected_rows <- selected_rows %>% arrange(id)

# Create a row showing the clipping between top and bottom rows
clipping_row <- data.frame(
  id = NA,
  name = "...",
  score = NA,
  comment = "..."
)

# Combine the final table (Top 5 + Taka's row + Clipping + Bottom 5)
final_table <- bind_rows(top_5, clipping_row, taka_row, clipping_row, bottom_5)

# Generate the table as a grob (graphical object) for output
table_grob <- tableGrob(final_table)

# Add dotted lines for clipped rows
for (i in which(is.na(final_table$id))) {
  table_grob$grobs[[i]]$gp <- gpar(lty = "dotted", col = "black")
}

# Save the table as an image
png("filtered_table_output.png", width = 800, height = 400)
grid.draw(table_grob)
dev.off()

# Show the table in RStudio Viewer
grid.draw(table_grob)
