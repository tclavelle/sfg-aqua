##################################################
## Project: Aquaculture Interactions
## Script purpose: Build flowchart / network diagrams
## Date: 05/9/2017
## Author: Tyler Clavelle
##################################################

library(DiagrammeR)

### Interaction network with interaction types as sub-nodes
g1 <- grViz("

digraph dot {
compound = true;            
      # graph [fontsize = 10, layout = dot, rankdir = LR]
      graph [fontsize = 10, layout = dot]
      
      node [shape = oval,
      style = filled,
      color = grey,
      label = '']
      
      ## SHAPES FOR STAKEHOLDERS      
      node [fillcolor = DarkSeaGreen, label = 'Capture\nfisheries']
      a
      node [fillcolor = orange, label = 'Wild\npopulation', shape = square]
      f
      node [fillcolor = orange, label = 'Habitat /\necosystem', shape = square]
      g
      edge [color = Salmon, label = 'harvest', fontcolor = Salmon, dir = back]
      a -> f
      
      edge [color = Salmon, label = 'habitat modification', fontcolor = Salmon, dir = forward]
      a -> g

subgraph cluster0 {
rank = same 
label = 'Fed mariculture'
      node [fillcolor = LightSeaGreen, label = 'Ponds']
      b
      node [fillcolor = LightSeaGreen, label = 'Cages']
      c
}
      edge [color = Salmon, label = 'fishmeal', dir = forward]
      a -> c [lhead = cluster0]
      edge [color = DarkMagenta, label = 'price competition', fontcolor = DarkMagenta, dir = both]
      c -> a [ltail = cluster0]
      edge [color = DarkMagenta, label = 'increased market', fontcolor = DarkMagenta]
      c -> a [ltail = cluster0]
      edge [color = DarkMagenta, label = 'increased demand', fontcolor = DarkMagenta]
      c -> a [ltail = cluster0]
      edge [color = SteelBlue, label = 'exclusion', fontcolor = SteelBlue]
      b -> a [ltail = cluster0]
      edge [color = Salmon, label = 'seed/fry', fontcolor = Salmon, dir = forward]
      f -> b [lhead = cluster0];
      edge [color = Salmon, label = 'disease', dir = both]
      c -> f [ltail = cluster0]
      edge [color = Salmon, label = 'water quality', fontcolor = Salmon]
      b -> g [ltail = cluster0]
      edge [color = Salmon, label = 'genetic fitness', fontcolor = Salmon]
      c -> f
      edge [color = Salmon, label = 'invasive species', fontcolor = Salmon]
      c -> g
      edge [color = Salmon, label = 'stock enhancement', fontcolor = Salmon]
      {b c} -> f [ltail = cluster0]
      edge [color = SteelBlue, label = 'de facto MPA', fontcolor = SteelBlue]
      b -> f [ltail = cluster0]
      edge [color = SteelBlue, label = 'FAD', fontcolor = SteelBlue]
      b -> a [ltail = cluster0]

subgraph cluster1 {
rank = same
label = 'Unfed mariculture'
      node [fillcolor = LightSeaGreen, label = 'Shellfish']
      d
      node [fillcolor = LightSeaGreen, label = 'Seaweed']
      h
}

edge [color = DarkMagenta, label = 'price competition', fontcolor = DarkMagenta, dir = both]
      d -> a [ltail = cluster1]
      edge [color = DarkMagenta, label = 'increased market', fontcolor = DarkMagenta]
      d -> a [ltail = cluster1]
      edge [color = DarkMagenta, label = 'increased demand', fontcolor = DarkMagenta]
      d -> a [ltail = cluster1]
      edge [color = SteelBlue, label = 'exclusion', fontcolor = SteelBlue, dir = both]
      d -> a [ltail = cluster1]
      c -> h [ltail = cluster0, lhead = cluster1]
      edge [color = Salmon, label = 'seed/fry', fontcolor = Salmon, dir = forward]
      f -> d [lhead = cluster1];
      edge [color = Salmon, label = 'disease', dir = both]
      d -> f [ltail = cluster1]
      edge [color = Salmon, label = 'water quality', fontcolor = Salmon]
      d -> g [ltail = cluster1]
      edge [color = SteelBlue, label = 'de facto MPA', fontcolor = SteelBlue]
      d -> f [ltail = cluster1]
      edge [color = SteelBlue, label = 'FAD', fontcolor = SteelBlue]
      d -> a [ltail = cluster1]

}")

export_graph(g1, file_name = 'sfg-aqua-figures/flowchart1.png', file_type = 'PNG')
