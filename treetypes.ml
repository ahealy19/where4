type prediction = (string * float)

type decision = (string * float)

type tree_node = 
	| Node of (decision * int * int)  
	| Leaf of prediction list

type decision_tree = tree_node array

type forest = decision_tree list

type tree_or_forest = 
	| Tree of decision_tree
	| Forest of forest
