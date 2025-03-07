source("/home/cantabile/Documents/repos/version0data/pressabuttonnow/game_text.R")
source("/home/cantabile/Documents/repos/version0data/pressabuttonnow/game_fns.R")
source("/home/cantabile/Documents/repos/version0data/pressabuttonnow/node_classes.R")

# Debugging: Print statements to check if functions are loaded
print("Loaded game_text.R")
print("Loaded game_fns.R")
print("Loaded node_classes.R")

# Example of creating an instance of buttonNode
node_instance <- buttonNode$new(node_label = "start", button_nodes = button_nodes)

# Debugging: Print the instance to check if it is created correctly
print(node_instance)

# Activate the node to see if it works
result <- node_instance$activateNode()

# Debugging: Print the result to check if the function works correctly
print(result)