theme_blank <- function(...) {
  theme_bw()+
  theme(legend.position = "none",
        panel.grid=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_blank()
  )
}

theme_minimal <- function(...) {
  theme_bw()+
    theme(legend.position = "none",
          panel.grid=element_blank()
    )
}

theme_map <- function(...) {
  coord_map()+theme_bw()+
    theme(panel.grid=element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank()
          )
}

theme_tufted <- function(...){
  theme_tufte(base_family = "Helvetica")+
    theme(panel.grid.major=element_line(size=0.25,color="black"),
          axis.ticks = element_line(size=0.25))
}