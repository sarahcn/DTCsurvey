#' Back to Back Barplot
#'
#' Function to make back to back bar plots as typically desired for survey data.
#'
#' Input data frame should be limited to the variables you want plotted,
#' typically likert-like scales (level of agreement) or yes/no questions.
#' The mapping between response values (e.g., 1,2,3,4) and response labels ("strongly agree","agree", etc.)
#' is given in function arguments.
#' Note only a series of statements with the same coding can be plotted at once.
#' If all response value are provided to \code{rightside_responses}, typical (not back to back) barplot can be created.
#'
#' @param{raw} {Input raw data frame.}
#' @param{group_var} {Optional variable by which to group, will create facet wrapped plot}
#' @param{response_levels} {Values in data frame in the order you want them plotted left to right, e.g. 1:4}
#' @param{response_labels} {Labels for responses in same order as \code{response_levels}}
#' @param{response_colors} {Vector assinging each \code{response_labels} value to a color.
#' Default assigns shades of red to left hand side variables, shades of blue to right hand side variables}
#' @param{rightside_resposes} {Vector of \code{response_levels} to be plotted on right hand side}
#' @param{leftside_resposes} {Vector of \code{response_levels} to be plotted on left hand side}
#' @param{count_caption} {Logical, whether to plot lower right caption with total N}
#' @param{zerolin} {Logical, whether to plot a vertical line at x=0}
#'
#' @return ggplot object to which other plotting attributes can easily be added (e.g., title)
#' @author Sarah C. Nelson (sarahcn@uw.edu)
#' @seealso https://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
#'
#' @examples
#' library(DTCsurvey)
#' library(ggplot2)
#' library(reshape2)
#' library(dplyr)
#'
#' # with no grouping
#' dat <- data.frame(matrix(sample(1:4,60, replace=TRUE), nrow=20,ncol=3))
#' names(dat) <- paste0("statement",1:ncol(dat))
#' b2b_barplot(raw=dat)
#'
#' # with grouping
#' dat$sex <- c(rep("M",times=10), rep("F",times=10))
#' b2b_barplot(raw=dat, group_var = "sex")
#'
#' @rdname b2b_barplot
#' @export

b2b_barplot <- function(raw = dat,
                        group_var = NULL,
                        response_levels = c(4:1),
                        response_labels = c("strongly disagree", "disagree", "agree", "strongly agree"),
                        response_colors = c(
                          "strongly disagree" = "#ca0020",
                          "disagree" = "#f4a582",
                          "agree" =  "#92c5de",
                          "strongly agree" = "#0571b0"
                        ),
                        rightside_responses = 1:2,
                        leftside_responses = 3:4,
                        count_caption = TRUE,
                        zeroline = TRUE) {

  # prepare melted data frame of counts, with no group by variables
  if(is.null(group_var)){
    # count first
    freq <- table(col(raw), as.matrix(raw))
    data <- data.frame(cbind(freq), Names = names(raw))
    # melt second
    data.m <- reshape2::melt(data, id.vars = 'Names')
    names_levels <- names(raw)
    # remove 'X' from variable
    data.m$variable <- gsub("X","", data.m$variable)
  }

  # prepare melted data frame of counts, when there is a group by variable
  if(!is.null(group_var)){
    # melt first
    names(raw)[which(names(raw) == group_var)] <- "grouping"
    group.frqs <- as.data.frame(table(raw$grouping), stringsAsFactors=FALSE)
    raw.m <- reshape2::melt(raw, id.vars="grouping")
    names(raw.m)[2:3] <- c("Names","variable")
    # count second
    data.m <- raw.m %>% dplyr::group_by(grouping, Names, variable) %>% dplyr::summarize(value=n())
    # convert grouping to ordered factor
    data.m$grouping.f <- factor(data.m$grouping,
                                levels = group.frqs$Var1[order(group.frqs$Freq, decreasing = TRUE)],
                                ordered=TRUE)
    names_levels <- setdiff(names(raw), "grouping")
  }

  # convert to ordered factors
  data.m$variable.f <-  factor(data.m$variable, levels = response_levels,
                               labels = response_labels, ordered = TRUE)
  data.m$Names.f <- factor(data.m$Names, levels = names_levels, ordered = TRUE)

  # build plot
  p <- ggplot2::ggplot(data.m, aes(Names.f, value)) +
    geom_bar(
      data = subset(data.m, variable %in% rightside_responses),
      aes(y = value, fill = variable.f),
      position = position_stack(reverse = TRUE),
      stat = "identity"
    ) +
    geom_bar(
      data = subset(data.m, variable %in% leftside_responses),
      aes(y = -value, fill = variable.f),
      stat = "identity"
    ) +
    ylab("count*") + xlab("statement") + coord_flip() +
    scale_fill_manual(
      values = response_colors,
      name = "Response",
      breaks = levels(data.m$variable.f)
    ) +
    scale_y_continuous(labels = abs) + # remove negative counts on y axis
    scale_x_discrete(limits = rev(levels(data.m$Names.f)))  # puts reasons top to bottom, in survey order

  # add caption of number of respondents
  if(count_caption) {
    p <- p + labs(caption = paste("*Over", nrow(raw), "respondents"))
  }

  # add ref line at count=0 (to distinguish rightside from leftside)
  if (zeroline) {
    # plot on yaxis due too coord flip
    p <- p + geom_hline(yintercept = 0)
  }

  # facet wrap by groups
  if(!is.null(group_var)){
    p <- p + facet_wrap(~grouping.f)
  }

  # prints graph to console - then user can save with ggsave, device call, etc. (I think)
  p

}
