#' Attach attribute columns to node df to define plot
#'
#' This function primarily sets attributes related to size and position.
#' (fontsize, width, height, x, y) as well as shape and tooltips. It effectively
#' sets a default look for the structure of the graph. It also needs work- the
#' numbers are hardcoded, and need to be made relative. It's possible to
#' add other attributes (see `DiagrammeR` documentation), and we'll likely also
#' want to set some of these (especially height and width) functionally (like we
#' do with colour)
#'
#' @param nodedf a tibble, the dataframe of nodes
#'
#' @return a tibble of nodes with new attribute columns needed to control the look in `DiagrammeR`
#' @export
#'
#' @examples
node_plot_atts <- function(nodedf) {
  
  # The way I'm getting x is crude, and likely will fail once we have more complex networks
  nodedf <- nodedf %>% 
    mutate(shape = 'rectangle',
           tooltip = Name,
           # Name = str_wrap(Name, 40),
           fontsize = 25,
           # width = 5, #str_length(Name)/2,
           width = ifelse(str_length(Name) < 40,
                          str_length(Name)/5,
                          40/5),
           # x = nodeorder * 15,
           height = (str_count(Name, '\\n') + 1)/2)
  
  # If we want things centered, need to get y with grouping
  nodedf <- nodedf %>% 
    group_by(NodeType) %>% 
    # This sets everything to have a midpoint at 0
    # Trying something less crude. This works to separate the boxes, but gets way too tall
    # mutate(num = n(),
    #        toth = sum(height, na.rm = TRUE)*2,
    #        mid = toth/2,
    #        rown = cumsum(height*2)-height*2,
    #        y = rown-mid) %>%
    # Double columns are a bit better?
  mutate(num = n(),
         mid = num/2,
         rown = row_number(),
         y = rown-mid) %>% # Kinda silly this way instead of mid-rown, but it keeps things from flipping
    mutate(basex = nodeorder*15,
           devx = width/1.8,
           plusminus = rep_along(devx, c(1, -1)) * devx,
           x = basex + plusminus) %>% 
    ungroup() %>% 
    select(-num, -mid, -rown, basex, devx, plusminus)

  
  return(nodedf)
}
