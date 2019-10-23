# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

f <- function(data,para){
  ggplot(data,aes(substr(DRG.Definition,1,3),y=get(para))) +  # ploting AMP vs DRG
    scale_y_log10(breaks = c(1000,10000,100000), # substituting the default values with dollar values
                  labels = c("$1000", "$10,000", "$100,000")) + # reducing white space by using log function for y- axis
    geom_boxplot(outlier.size = 0.2) + #reducing dot size to make the graph clean and readable
    theme(axis.text.x = element_text(angle = 90), text = element_text(size = 8)) +
    labs(title = "DRG codes") +
    xlab("DRG Code") +
    ylab(para) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) #adjusting text size
}

drg_apply <- function(data, func){
  data %>%
    mutate(DRG_Code = paste("DRG",substr(DRG.Definition,1,3), sep = "_")) %>%
    select(DRG_Code, Provider.Id, Provider.State, Average.Medicare.Payments) %>%
    spread(key = DRG_Code, value = Average.Medicare.Payments) %>%
    select(3:102) %>%
    apply(MARGIN = 2, FUN = func, na.rm = TRUE) %>%
    round(2)
}
