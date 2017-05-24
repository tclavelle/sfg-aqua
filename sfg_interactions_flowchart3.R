###
# Create a graph with both nodes and edges
# defined, and, add some default attributes
# for nodes and edges
###

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Initialize graph
g1 <- create_graph()

# Create a node data frame
node_df <- create_node_df(n = 7,
                          label = c('habitat', 
                                    'wild\npopulation',
                                    'capture\nfisheries',
                                    'cage', 
                                    'pond', 
                                    'shellfish', 
                                    'seaweed'),
                          style = 'filled',
                          color = 'aqua',
                          shape = 'rectangle',
                          fixedsize = F)

# Create edge data frame
edge_df <- create_edge_df(from  = c(1,2,2,2,3,3,3,3,4,5,6,7),
                          to    = c(2,3,4,5,6,4,5,1,1,1,1,1),
                          color = c('gray',rep('Salmon', times = 11)),
                          label = c('','harvest','feed','feed',rep('seed/fry', times = 3),
                                    'destructive\nfishing',rep('habitat\nmodification',times = 4)))

g1 <- g1 %>%
  add_node_df(node_df = node_df) %>%
  add_edge_df(edge_df = edge_df)

# render graph
render_graph(g1)

# export graph
# export_graph(g1, file_name = 'sfg-aqua-figures/flowchart.png')
