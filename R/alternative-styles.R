# autoplot_radf_style2 <- function(x, y) {
#   augment_join(radf_dta) %>%
#   filter(sig == 95, name == "bsadf") %>%
#     mutate(
#       dummy = tstat > crit,
#       dummy_stat = dplyr::if_else(dummy, tstat, NA_real_)
#     ) %>%
#     ggplot() +
#     geom_area(aes(x = index, y = dummy_stat, fill = id),  position="identity") +
#     facet_grid(id~ ., scales = "free_y", switch = "y") +
#     theme_bw() +
#     scale_y_continuous(position = "right") +
#     scale_fill_manual(values = rep("gray30", 5)) +
#     # scale_fill_grey() +
#     theme(
#       # axis.title.y = element_text(vjust = 0, hjust = 0),
#       strip.text.y.left = element_text(face = "bold", size = 8, angle = 0),
#       # strip.text.y = element_text(face = "bold", size = 8, vjust = 1),
#       strip.background = element_blank(),
#       legend.position = "none",
#       axis.title = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_line(linetype = "dashed")
#     )
# }
#
# autoplot_ds_radf_style2 <- function(x) {
#   tidy(datestamp(radf_dta)) %>%
#     ggplot() +
#     geom_point(aes(Start, id)) +
#     geom_point(aes(End, id)) +
#     geom_segment(
#       aes_string(x = "Start", xend = "End", y = "id", yend = "id"), ) +
#     theme_bw()
#
# }
#
