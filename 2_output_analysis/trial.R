

# ternary_plot -----------------------------------------------------------
# https://cran.r-project.org/web/packages/Ternary/vignettes/Ternary.html

# install.packages('Ternary')
# Ternary::TernaryApp()
suppressPackageStartupMessages(library('Ternary'))


# TernaryPlot()

# par(mfrow = c(2, 2), mar = rep(0.5, 4))
# for (dir in c('up', 'right', 'down', 'le')) {
#     TernaryPlot(point = dir, atip = 'A', btip = 'B', ctip = 'C',
#                 alab = 'Aness', blab = 'Bness', clab = 'Cness')
#     TernaryText(list(A = c(10, 1, 1), B = c(1, 10, 1), C = c(1, 1, 10)),
#                 col = cbPalette8[4], font = 2)
# }

TernaryPlot(alab = "Redder \u2192", blab = "\u2190 Greener", clab = "Bluer \u2192",
            lab.col = c('red', 'darkgreen', 'blue'),
            point = 'right', lab.cex = 0.8, grid.minor.lines = 0,
            grid.lty = 'solid', col = rgb(0.9, 0.9, 0.9), grid.col = 'white', 
            axis.col = rgb(0.6, 0.6, 0.6), ticks.col = rgb(0.6, 0.6, 0.6),
            axis.rotate = FALSE,
            padding = 0.08)
# Colour the background:
cols <- TernaryPointValues(rgb, resolution = 240L)
ColourTernary(cols, spectrum = NULL)
# data_points <- list(
#     R = c(255, 0, 0), 
#     O = c(240, 180, 52),
#     Y = c(210, 222, 102),
#     G = c(111, 222, 16),
#     B = c(25, 160, 243),
#     I = c(92, 12, 243),
#     V = c(225, 24, 208)
# )
# AddToTernary(points, data_points, pch = 21, cex = 2.8, 
#              bg = vapply(data_points, 
#                          function (x) rgb(x[1], x[2], x[3], 128,
#                                           maxColorValue = 255),
#                          character(1))
# )
# AddToTernary(text, data_points, names(data_points), cex = 0.8, font = 2)
# legend('bottomright', 
#        legend = c('Red', 'Orange', 'Yellow', 'Green'),
#        cex = 0.8, bty = 'n', pch = 21, pt.cex = 1.8,
#        pt.bg = c(rgb(255,   0,   0, 128, NULL, 255), 
#                  rgb(240, 180,  52, 128, NULL, 255),
#                  rgb(210, 222, 102, 128, NULL, 255),
#                  rgb(111, 222,  16, 128, NULL, 255))
# )








