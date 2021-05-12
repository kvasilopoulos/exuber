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

# autoplot_ds_radf_style3 <- function(x) {
#   filter(x, name == "bsadf", sig == "95") %>%
#     mutate(area = ifelse(tstat > crit, tstat, 0)) %>%
#     ggplot(aes(index, area)) +
#     geom_area() +
#     facet_wrap(~id, ncol = 1, strip.position = "left", scales = "free_y") +
#     # scale_y_continuous(position = "right") +
#     theme_bw() +
#     theme(
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank(),
#       panel.grid = element_blank(),
#       axis.title = element_blank(),
#       strip.background = element_blank(),
#       strip.text.y.left = element_text(angle = 0),
#
#     )
# }



# TODO https://plotnine.readthedocs.io/en/stable/generated/plotnine.geoms.geom_segment.html


# additional style for autoplot.ds ----------------------------------------


# tidy(datestamp(radf(sim_data))) %>%
#   ggplot() +
#   geom_segment(aes_string(x = "Start", xend = "End", y = "id", yend = "id"), color = "grey", size = 1) +
#   geom_point(aes(x = Start, y = id), size = 2, color = "red") +
#   geom_point(aes(x = Start + 5, y = id), size = 2, color = "green") +
#   geom_point(aes(x = End, y = id), size = 2, color = "blue") +
#   geom_text(aes(x = Start + 5, y = id, label = Duration), size = 4, color = "white") +
#   labs(title = "", x = "", y = "") + #intentionally not in theme (for extra margin)
#   theme_bw() +
#   scale_y_discrete(drop = FALSE) +
#   theme(
#     axis.text.y = element_text(face = "bold", size = 8, hjust = 0),
#     legend.position = "none",
#     panel.grid.major.y = element_blank(),
#     plot.margin = margin(0.5, 1, 0, 0.5, "cm")
#   )
