# devtools::install_github("https://github.com/cj-holmes/vhs")
library(vhs)

cols <- vhs("maxell_gu")

nrows <- 2
ncols <- 4
order <- matrix(c(1:(nrows*ncols)), nrows, ncols, byrow=TRUE)
magn <- 1.0
widths <- rep(magn, ncols)
heights <- rep(magn,nrows)

# pdf( "ces_logo.pdf", width = sum(widths), height = sum(heights) )
panel <- layout(
  order,
  widths = widths,
  heights = heights,
  TRUE
)
# layout.show(panel)

par(mar = c(0, 0, 0, 0), family = "Helvetica", xaxs="i", yaxs="i")

## lowercase
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
text(0.5, 0.5, labels = ">", adj = 0.5, font = 2, cex = 12, col = cols[1])
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
text(0.5, 0.5, labels = "c", adj = 0.5, font = 2, cex = 12, col = cols[1])
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
text(0.5, 0.5, labels = "e", adj = 0.5, font = 2, cex = 12, col = cols[1])
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
text(0.5, 0.5, labels = "s", adj = 0.5, font = 2, cex = 12, col = cols[1])

plot(c(0,1), c(0,1), type = "n", axes = FALSE)
rect(0, 0, 1, 1, col = "white", border = NA)
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
rect(0, 0, 1, 1, col = cols[2], border = NA)
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
rect(0, 0, 1, 1, col = cols[3], border = NA)
plot(c(0,1), c(0,1), type = "n", axes = FALSE)
rect(0, 0, 1, 1, col = cols[1], border = NA)

# dev.off()
