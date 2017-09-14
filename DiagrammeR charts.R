##################
#
# Create nodes & edges; rescale edge data for line width; 
# export DOT code for editing with GraphViz
#
##################

# Require v0.6 of DiagrammeR
require(devtools)
install_version("DiagrammeR", version = "0.6", repos = "http://cran.us.r-project.org")

library(DiagrammeR)
library(pipeR)

# Create a node data frame
nodes_1 <-
  create_nodes(nodes = c("a", "b", "c", "d", "e"),
               label = c("Zone","Affiliation","Pests","Aware","Farmer"),
               type = "upper",
               shape = "rectangle")

nodes_1

nodes_2 <-
  create_nodes(nodes = c("f", "g", "h", "i", "j", "k", "l", "p", "q"),
               label = c("Seen_hares","Threat","Support_mgmt","Petition","Hunt",
                         "Support_gvt","Landowner","Conservation","Seen_Euro"),
               type = "lower",
               shape = "rectangle")

nodes_2

all_nodes <- combine_nodes(nodes_1, nodes_2)

all_nodes

# Create an edge data frame
edges_1 <-
  create_edges(from = c("c", "d", "d", "d", 
                        "d", "e", "j", "j",	
                        "j",	"j", "p",	"p",	
                        "p",	"f", "g",	"g",	
                        "g", "i", "i", "h"),
               to = c("l", "f", "p", "q", 
                      "j", "l", "f", "q",	
                      "i", "k", "q", "g",	
                      "h", "q", "h", "i",	
                      "k", "h", "k", "k"),
               dir = "both",
               color = "black",
               data = c(0.163,  0.122,	0.136,	0.473,	
                        0.398,	0.501,	0.164,	0.348,	
                        0.288,	0.182,	0.209,	0.245,	
                        0.119,	0.177,  0.431,	0.379,	
                        0.243,  0.450,  0.747,  0.482))

edges_1

edges_2 <-
  create_edges(from = c("e", "e", "e",	"e"),
               to = c("g",  "i",	"h", "j"), #significant negative correlations
               dir="both",
               color="black",
               data = c(0.142,  0.141,	0.114,	0.201))


all_edges <- combine_edges(edges_1, edges_2)

all_edges$data <- as.numeric(all_edges$data)

#Scale edge data and create line width column
edges <- scale_edges(edges_df = all_edges,
                     to_scale = all_edges$data,
                     edge_attr = "penwidth",
                     range = c(5, 40))

edges

#Set attributes
node_attrs <- c("fontsize = 50", "fontname = Courier",
                "width = 2", "height = 1.5", "color = black",
                "penwidth = 4", "fixedsize = TRUE", "height = 1", "width = 5")

edge_attrs <- c("arrowhead = none",
                "arrowtail = none")

graph_attrs <- c("overlap = false",
                 "fixedsize = true",
                 "ranksep = .5",
                 "compound = true", 
                 "nodesep = .5",
                 "color = crimson",
                 "outputorder = nodesfirst")

#Create graph
graph <- graphviz_graph(
  nodes_df = all_nodes, 
  edges_df = all_edges,
  edge_attrs = edge_attrs,
  graph_attrs = graph_attrs,
  node_attrs = node_attrs)

#graphviz_render(graph, width = 1200, height = 800)

#export dot script for GraphViz
graphviz_render(graph, output = "DOT") %>>% cat(file = "dot.gv")


#edit DOT script from raw code graph
network <- grViz("

      digraph {graph 
      [overlap = true, 
      #rankdir = LR,
      #fixedsize = true,       
      ranksep = 2.5,       
      compound = true,       
      nodesep = 1,       
      color = crimson,
      dpi = 250]
      
      node 
      [fontsize = 28,     
      fontname = Courier,     
      #width = 2,     
      #height = 1.5,     
      color = black,     
      penwidth = 4,     
      #fixedsize = TRUE,     
      height = 1,     
      width = 5]
      
      edge 
      [arrowhead = normal,     
      arrowtail = normal,
      arrowsize = 5]
      
      'a' [label = '0.75', shape = 'rectangle',  margin = '.5,.5,.5,.5'] 
      'b' [label = '0.11', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'c' [label = 'Key*', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'd' [label = 'Aware of the invasive issue*', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'e' [label = 'Farmer', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'f' [label = 'Seen hares', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'g' [label = 'Consider the invasive threat important*', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'h' [label = 'Support management of European hare', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'i' [label = 'Petition the government directly', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'j' [label = 'Hunt', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'k' [label = 'Support government decision to cull', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'l' [label = 'Landowner*', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'p' [label = 'Concerned with conservation of biodiversity', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'q' [label = 'Seen European hare', shape = 'rectangle', margin = '.5,.5,.5,.5'] 
      'r' [label = 'Support for lethal culling*', shape = 'rectangle', margin = '.5,.5,.5,.5']

      #  'c'->'l' [dir = 'both', color = 'black', penwidth = '2.47077409162717'] 
      #  'd'->'f' [dir = 'both', color = 'black', penwidth = '1.24012638230648'] 
      #  'd'->'p' [dir = 'both', color = 'black', penwidth = '1.66034755134281'] 
      #  'd'->'q' [dir = 'both', color = 'black', penwidth = '11.7756714060032'] 
      #  'd'->'j' [dir = 'both', color = 'black', penwidth = '9.52448657187994'] 
      #  'e'->'l' [dir = 'both', color = 'black', penwidth = '12.6161137440758'] 
      #  'j'->'f' [dir = 'both', color = 'black', penwidth = '2.50078988941548'] 
      #  'j'->'q' [dir = 'both', color = 'black', penwidth = '8.02369668246445'] 
      #  'j'->'i' [dir = 'both', color = 'black', penwidth = '6.22274881516588'] 
      #  'j'->'k' [dir = 'both', color = 'black', penwidth = '3.04107424960505'] 
      #  'p'->'q' [dir = 'both', color = 'black', penwidth = '3.85150078988941'] 
      #  'p'->'g' [dir = 'both', color = 'black', penwidth = '4.93206951026856'] 
      #  'p'->'h' [dir = 'both', color = 'black', penwidth = '1.15007898894155'] 
      #  'f'->'q' [dir = 'both', color = 'black', penwidth = '2.89099526066351'] 
      #  'g'->'h' [dir = 'both', color = 'black', penwidth = '10.5150078988942'] 
      #  'g'->'i' [dir = 'both', color = 'black', penwidth = '8.95418641390205'] 
      #  'g'->'k' [dir = 'both', color = 'black', penwidth = '4.87203791469194'] 
      #  'i'->'h' [dir = 'both', color = 'black', penwidth = '11.085308056872'] 
      #  'i'->'k' [dir = 'both', color = 'black', penwidth = '20'] 
      #  'h'->'k' [dir = 'both', color = 'black', penwidth = '12.0458135860979'] 
      #  'e'->'g' [dir = 'both', color = 'black', penwidth = '1.84044233807267'] 
      #  'e'->'i' [dir = 'both', color = 'black', penwidth = '1.81042654028436'] 
      #  'e'->'h' [dir = 'both', color = 'black', penwidth = '1'] 
      #  'e'->'j' [dir = 'both', color = 'black', penwidth = '3.61137440758294'] 
      
      
      'r'->'c' [dir = 'forward', color = 'black', penwidth = '5']
      'r'->'d' [dir = 'forward', color = 'black', penwidth = '5']
      'r'->'e' [dir = 'forward', color = 'black', penwidth = '5']
      'c'->'l' [dir = 'none', color = 'black', penwidth = '7.70932069510269'] 
      'd'->'f' [dir = 'none', color = 'black', penwidth = '5.44233807266983'] 
      'd'->'p' [dir = 'none', color = 'black', penwidth = '6.21642969984202'] 
      'd'->'q' [dir = 'none', color = 'black', penwidth = '24.8499210110585'] 
      'd'->'j' [dir = 'none', color = 'black', penwidth = '20.7030015797788'] 
      'e'->'l' [dir = 'none', color = 'black', penwidth = '26.3981042654028'] 
      'j'->'f' [dir = 'none', color = 'black', penwidth = '7.76461295418641'] 
      'j'->'q' [dir = 'none', color = 'black', penwidth = '17.9383886255924'] 
      'j'->'i' [dir = 'none', color = 'black', penwidth = '14.6208530805687'] 
      'j'->'k' [dir = 'none', color = 'black', penwidth = '8.75987361769352'] 
      'p'->'q' [dir = 'none', color = 'black', penwidth = '10.2527646129542'] 
      'p'->'g' [dir = 'none', color = 'black', penwidth = '12.2432859399684'] 
      'p'->'h' [dir = 'none', color = 'black', penwidth = '5.27646129541864'] 
      'f'->'q' [dir = 'none', color = 'black', penwidth = '8.48341232227488'] 
      'g'->'h' [dir = 'none', color = 'black', penwidth = '22.5276461295419'] 
      'g'->'i' [dir = 'none', color = 'black', penwidth = '19.652448657188'] 
      'g'->'k' [dir = 'none', color = 'black', penwidth = '12.1327014218009'] 
      'i'->'h' [dir = 'none', color = 'black', penwidth = '23.5781990521327'] 
      'i'->'k' [dir = 'none', color = 'black', penwidth = '40'] 
      'h'->'k' [dir = 'none', color = 'black', penwidth = '25.347551342812'] 
      'e'->'g' [dir = 'none', color = 'black', penwidth = '6.54818325434439'] 
      'e'->'i' [dir = 'none', color = 'black', penwidth = '6.49289099526066'] 
      'e'->'h' [dir = 'none', color = 'black', penwidth = '5'] 
      'e'->'j' [dir = 'none', color = 'black', penwidth = '9.81042654028436'] 
      
      
      #subgraph {
      #node [fixedsize = true, width = 3]
      #rank = same; c;  l; d;
      #}    
      
      
   }
")

network
