

for(i in 1:8){
    # i = 1
    # weekly_minutes
    data = weekly_minutes[, .SD,
                          .SDcol = c('weekth', 'hour_minute_th',
                                     paste(activity_categories[i], '_share', sep = '')
                          )]
    colnames(data)[3] = 'share'
    png(
        paste('visualization/01_minutes_share/1.0.', i,
              '_minutes_share_heatmap_of_',
              activity_categories[i], '.png', sep = ''),
        width = 8.8, height = 8.8, units = 'cm', res = 600)
    ggplot() +
        geom_tile(
            data = data,
            aes(x = weekth, y = hour_minute_th, fill = share),
            colour = "white") +
        geom_vline(xintercept = lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = post_lockdown, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = mask_requirement, linetype = "dashed",
                   color = 'black', size = 0.2) +
        geom_vline(xintercept = second_wave, linetype = "dashed",
                   color = 'black', size = 0.2) +
        scale_fill_gradient2(
            low = "red", mid = "yellow", high = "blue",
            limits = c(0, 1),
            breaks=seq(0, 1, 0.2),
            labels=c('0', '0.2', '0.4', '0.6', '0.8', '1'),
            guide = guide_colourbar(
                title = paste('Share of', activity_categories[i], 'minutes'),
                title.position = 'left',
                title.theme = element_text(size = 8, family = 'Times New Roman'),
                label.theme = element_text(size = 8, family = 'Times New Roman'),
                barwidth = unit(4, units = "cm"),
                barheight = unit(0.2, units = "cm"),
                draw.ulim = TRUE,
                title.vjust = 1,
                direction = 'horizontal'
            )) +
        ylab("O'clock") +
        theme_bw() + theme_minimal() +
        scale_x_continuous(
            limits = c(min(data$weekth) -1, max(data$weekth) +1),
            expand = c(0, 0),
            breaks = seq(1, max(data$weekth), 4),
            labels = monday_weekth_labels[
                1:length(seq(1, max(data$weekth), 4))] ) +
        scale_y_reverse(
            limits = c(96 + 1, 1 - 1),
            expand = c(0, 0),
            breaks = seq(93, 1, -4),
            labels = seq(24, 1, -1)
        ) +
        theme(
            axis.title.y = element_text(
                size = 8, family = 'Times New Roman'),
            axis.text.x = element_text(
                size = 8, family = 'Times New Roman', vjust = 0.75, angle = 45),
            axis.text.y = element_text(
                size = 8, family = 'Times New Roman', hjust = 1),
            axis.ticks = element_line(size = 0.2),
            axis.title.x = element_blank(),
            panel.spacing.x = unit(0, "cm"),
            legend.position = c(0.45, -0.25),
            legend.background = element_blank(),
            # (top, right, bottom, left)
            plot.margin = unit(c(0.01, 0.3, 0.6, 0.08), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect( colour = "black", fill=NA, size=0.1),
            panel.background = element_blank()
        )
    dev.off()
    print(i)
}


