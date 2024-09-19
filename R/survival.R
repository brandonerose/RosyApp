ggsave_workaround <- function(g){
  survminer:::.build_ggsurvplot(
    x = g,
    surv.plot.height = NULL,
    risk.table.height = NULL,
    ncensor.plot.height = NULL
  )
}
font_maker<-function(size=12,style="bold",color="black"){
  styles <- c("plain", "bold", "italic", "bold.italic")
  if(!style %in% styles)stop("style must be of type: ", styles %>% paste0(collapse = ", "))
  c(as.numeric(size),as.character(style),as.character(color))
}
make_survival <- function(fit,data,group,title,colors,xlim=NULL){
  data$group <- data[[group]]
  print(fit)
  # survminer::surv_pvalue(fit=fit,data = data) %>% print.data.frame()
  ggsurvplot(
    fit = fit,
    data = data,
    surv.median.line = "hv", # Add medians survival
    # Change legends: title & labels,
    title = title,
    xlim=xlim,
    break.x.by=12,
    # legend = "bottom",
    legend.title = data[[group]] %>% attr("label"),
    # legend.labs = data[[group]] %>% attr("levels"),
    legend.labs = data[[group]] %>% unique(),
    # Add p-value and tervals
    combine = T,
    pval = T,
    pval.coord=c(27, 0.25),
    conf.int = T,
    xlab =" Months",
    # Add risk table
    risk.table = T,
    tables.height = 0.2,
    tables.theme = theme_cleantable(),
    surv.scale = "percent",
    # Color palettes. Use custom color: c("#E7B800", "#2E9FDF"),
    # or brewer color (e.g.: "Dark2"), or ggsci color (e.g.: "jco")
    palette = colors,
    ggtheme = theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold")),
    font.x = font_maker(14),
    font.y = font_maker(14),
    font.main = font_maker(14),
    font.submain = font_maker(14),
    font.tickslab = font_maker(14),
    font.caption = font_maker(14),
    font.title = font_maker(14),
    font.subtitle = font_maker(14),
    font.legend = font_maker(14),
    table.theme = theme_classic()
  )
}
