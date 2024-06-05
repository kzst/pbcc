#-----------------------------------------------------------------------------#
#                                                                             #
#  PERCENTILE-BASED CONTROL CHARTS                                            #
#                                                                             #
#  Written by: Aamir Saghir, Zsolt T. Kosztyan                                #
#              Department of Quantitative Methods                             #
#              University of Pannonia, Hungary                                #
#              kosztyan.zsolt@gtk.uni-pannon.hu                               #
#                                                                             #
# Last modified: June 2024                                                    #
#-----------------------------------------------------------------------------#

#' @export
plot.pbcc <- function(x, title,...){
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package \"ggplot2\" must be installed to use this function.",
      call. = FALSE ) }
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop(
      "Package \"ggpubr\" must be installed to use this function.",
      call. = FALSE
    )
  }
  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(
      "Package \"grDevices\" must be installed to use this function.",
      call. = FALSE ) }

  if (methods::is(x,"pbcc")){
    Q <- x
    data.name <-  Q$data.name
    type <- Q$type
  if(missing(title))
    {
      title <- paste(type, "chart for", data.name)
    }

  if (any(type==c("Xbar", "R", "S", "S2"))){
    stats <- Q$statistics
    lcl <- Q$LCL
    cl <- center <- Q$CL
    ucl <- Q$UCL
    limits <- matrix(c(lcl, ucl), ncol = 2)
    Groups<-value<-variable<-NULL
    stat<-value<-variable<-NULL
    Groups <- c(1:length(stats))
    df <- data.frame(Groups=Groups,stat = stats, lcl = lcl, ucl = ucl)
    ylim <- grDevices::extendrange(c(df$stats, df$lcl, df$ucl))
    xlim <- grDevices::extendrange(df$Groups)
    plot<- ggplot2::ggplot(df, ggplot2::aes(x = Groups,  y = stat)) +
     ggplot2::geom_point()+ ggplot2::geom_line()+ ggplot2::geom_hline(yintercept=c(lcl,cl, ucl), linetype="dashed") +
     ggplot2::geom_text(data = data.frame(y = c(rev(lcl)[1],rev(cl)[1],rev(ucl)[1]), x = rep(xlim[2], 3)),
     ggplot2::aes_string(x = "x", y = "y"), label = c("LCL","CL", "UCL"), hjust = -0.2, size = 10 * 5/14, col = grDevices::gray(0.3))+
     ggplot2:: labs(title=title, subtitles= "",x =  "Groups",  y = "Group summary statistics")+
     ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off")+
       ggplot2::theme_light() +
       ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
             legend.position = "none",
             plot.margin = ggplot2::margin(5, 30, 5, 5))
    } else    {
      stats <- Q$statistics
      lcl <- Q$LCL
      cl <- center <- Q$CL
      ucl <- Q$UCL
      limits <- matrix(c(lcl, ucl), ncol = 2)
      Groups<-value<-variable<-NULL
      stat<-value<-variable<-NULL
      Groups <- c(1:length(stats))
      df <- data.frame(Groups=Groups,stat = stats, lcl = lcl, ucl = ucl)
      ylim <- grDevices::extendrange(c(df$stats, df$lcl, df$ucl))
      xlim <- grDevices::extendrange(df$Groups)
      plot1 <- ggplot2::ggplot(df, ggplot2::aes(x = Groups,  y = stat)) +
        ggplot2::geom_point()+ggplot2::geom_line()+ggplot2::geom_hline(yintercept=c(lcl,cl, ucl), linetype="dashed") +
        ggplot2::geom_text(data = data.frame(y = c(rev(lcl)[1],rev(cl)[1],rev(ucl)[1]), x = rep(xlim[2], 3)),
        ggplot2::aes_string(x = "x", y = "y"), label = c("LCL","CL", "UCL"), hjust = -0.2, size = 10 * 5/14, col = grDevices::gray(0.3))+
        ggplot2::labs(subtitles= "",x =  "Groups",  y = "Group summary statistics")+
        ggplot2::coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off")+
        ggplot2::theme_light() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
              legend.position = "none",
              plot.margin = ggplot2::margin(5, 30, 5, 5))
      stats1 <- Q$statistics1
      lcl1 <- Q$LCL1
      cl1 <- center1 <- Q$CL1
      ucl1 <- Q$UCL1
      limits1 <- matrix(c(lcl1, ucl1), ncol = 2)
      Groups1<-value<-variable<-NULL
      stat1<-value<-variable<-NULL
      Groups1 <- c(1:length(stats1))
      df1 <- data.frame(Groups1=Groups1,stat1 = stats1, lcl1 = lcl1, ucl1 = ucl1)
      ylim1 <- grDevices::extendrange(c(df1$stats1, df1$lcl1, df1$ucl1))
      xlim1 <- grDevices::extendrange(df1$Groups1)
      plot2 <- ggplot2::ggplot(df1, ggplot2::aes(x = Groups1,  y = stat1)) +
        ggplot2::geom_point()+ ggplot2::geom_line()+ ggplot2::geom_hline(yintercept=c(lcl1,cl1, ucl1), linetype="dashed") +
        ggplot2::geom_text(data = data.frame(y = c(rev(lcl1)[1],rev(cl1)[1],rev(ucl1)[1]), x = rep(xlim1[2], 3)),
        ggplot2::aes_string(x = "x", y = "y"), label = c("LCL","CL", "UCL"), hjust = -0.2, size = 10 * 5/14, col = grDevices::gray(0.3))+
        ggplot2:: labs(subtitles= "",x =  "Groups",  y = "Group summary statistics")+
        ggplot2::coord_cartesian(xlim = xlim1, ylim = ylim1, expand = FALSE, clip = "off")+
        ggplot2::theme_light() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11),
              legend.position = "none",
              plot.margin = ggplot2::margin(5, 30, 5, 5))

    plot <- ggpubr::ggarrange(plot1, plot2, labels = title, ncol = 2,  widths = 0.5, heights = 0.5)
    }
  return(plot)

   } else {
      plot(x,...)
    }
}


