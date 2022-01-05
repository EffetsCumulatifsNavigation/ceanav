#' Logo du projet
#'
#' Fonction pour générer le logo du projet
#'
#' @keywords figure
#'
#' @export
#'

fig_logo <- function() {
  # Data to use as figure
  load_output("cumulative_effects")

  # Thanks to hexSticker package: https://github.com/GuangchuangYu/hexSticker
  hexSticker::sticker(~plot_simple(cumulative_effects[, "cumulative_effects"]),
                      package="ceanav",
                      p_size=20,
                      s_x=1.1,
                      s_y=.9,
                      p_y = 1.5,
                      s_width=1.5,
                      s_height=1,
                      p_family = "serif",
                      h_fill = "#a6b6c8",
                      h_color = "#0f3c4f",
                      p_color = "#0f3c4f",
                      filename="figures/logo.png")

  if (!file.exists("man/figures/")) dir.create("man/figures/")
  file.copy("figures/logo.png", "man/figures/logo.png", overwrite = TRUE)
}
