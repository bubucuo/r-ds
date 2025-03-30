# Install VennDiagram package
install.packages("VennDiagram")

# ! maybe you need to install these packages
# install.packages("grid")
# install.packages("futile.logger")


# Load VennDiagram package
library("grid")
library("futile.logger")
library("VennDiagram")


# ! Example 1: Single Venn Diagram in R
grid.newpage() # Move to new plotting page
draw.single.venn(area = 10) # Create single venn diagram


# Example 2: Pairwise Venn Diagram
grid.newpage() # Move to new plotting page
draw.pairwise.venn(
    area1 = 10, # Create pairwise venn diagram
    area2 = 20,
    cross.area = 2
)


# Example 3: Venn Diagram with Three Sets
grid.newpage() # Move to new plotting page
draw.triple.venn(
    area1 = 10, # Create venn diagram with three sets
    area2 = 20,
    area3 = 15,
    n12 = 2,
    n23 = 3,
    n13 = 7,
    n123 = 2
)


# Example 4: Change Color of Venn Diagram
grid.newpage() # Move to new plotting page
draw.triple.venn(
    area1 = 10, # Change color of venn diagram
    area2 = 20,
    area3 = 15,
    n12 = 2,
    n23 = 3,
    n13 = 7,
    n123 = 2,
    col = "red",
    fill = "#1b98e0"
)


#  Example 5: Specify Different Color for Each Set
grid.newpage() # Move to new plotting page
draw.triple.venn(
    area1 = 10, # Different color for each set
    area2 = 20,
    area3 = 15,
    n12 = 2,
    n23 = 3,
    n13 = 7,
    n123 = 2,
    col = "red",
    fill = c("pink", "green", "orange")
)


# Example 6: Disable Transparency of Venn Diagram
grid.newpage() # Move to new plotting page
draw.triple.venn(
    area1 = 10, # Disable transparency of venn diagram
    area2 = 20,
    area3 = 15,
    n12 = 2,
    n23 = 3,
    n13 = 7,
    n123 = 2,
    col = "red",
    fill = c("pink", "green", "orange"),
    alpha = 1
)


# Example 7: Remove Lines from Venn Diagram
grid.newpage() # Move to new plotting page
draw.triple.venn(
    area1 = 10, # Remove lines from venn diagram
    area2 = 20,
    area3 = 15,
    n12 = 2,
    n23 = 3,
    n13 = 7,
    n123 = 2,
    fill = c("pink", "green", "orange"),
    lty = "blank"
)


# Example 8: Add Name to Each Set of Venn Diagram
grid.newpage() # Move to new plotting page
draw.triple.venn(
    area1 = 10, # Add name to each set
    area2 = 20,
    area3 = 15,
    n12 = 2,
    n23 = 3,
    n13 = 7,
    n123 = 2,
    fill = c("pink", "green", "orange"),
    lty = "blank",
    category = c("Group 1", "Group 2", "Group 3")
)
