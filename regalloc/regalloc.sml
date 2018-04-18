
structure StringSet = SplaySetFn(type ord_key = string val compare = String.compare)
val regSet = StringSet.addList(StringSet.empty, map MipsFrame.getRegName (map MipsFrame.getTemp (MipsFrame.argRegs @
							                                         MipsFrame.calleeSaves @
							                                         MipsFrame.callerSaves)))

(*Generate liveness information*)
(*Grab MipsFrame.tempMap*)
(*In interference graph, add all precolored temps that aren't already in the graph*)
(*Make all precolored temps interfere with each other (Appel possibly proposes an efficient/easy way to do this?)*)
(*Returns a Temp.Map and an interference graph*)
fun init filename =
    let
	val (ig,mg) = Liveness.main filename
    in
        ()
    end
	
    

fun push(element, list) = element::list

fun pop(list) = (List.hd(list), List.tl(list))

(*Remove all nodes of trivial degree until we reach a base case (single node) or can't simplify any further*)
(*returns the fully simplified graph, and a stack containing all the nodes that we have removed from the graph*)
(*Remember to take into account move edges when counting degree!*)
fun simplify(igraph, mgraph) =
    let
	val nodeSet = Liveness.TempGraph.nodes igraph
	val change = ref 0
	fun removeTrivials stack =
	    ((foldl (fn (node, (ig,stk)) => if (Liveness.TempGraph.degree(node) < MipsFrame.numRegs)
	                                    then (change:=1; (Liveness.TempGraph.remove(ig, node), push((node,Liveness.TempGraph.adj(node)),stk)))
				            else (ig,stk))
	    (igraph,stack) nodeSet);
	    if !change = 1
	    then (change:=0; removeTrivials stack)
	    else (igraph,stack))
    in
	removeTrivials []
    end

fun colorSimpGraph(sgraph, tempMap) =
    let val nodes = Liveness.TempGraph.nodes(sgraph)
		    fun precolor(temp, map) =
				    (let val neighbors = Liveness.TempGraph.adj(temp)
						     fun getValidColors(neighbor, set) =
								    case Temp.Map.find(map, neighbor) of
										     SOME(color) => StringSet.delete(set, color)
										     | NONE => set
								val validColors = foldl getValidColors regSet neighbors
						in Temp.Map.insert(map, Liveness.TempGraph.nodeInfo(temp), List.hd(StringSet.listItems(validColors)))
						end)
		in foldl precolor tempMap nodes
		end

(*Takes as arguments the output of simplify*)
(*Rebuild graph: First, color the base igraph which should be trivial. Then, add nodes from the stack and color each one*)
(*For each node that we add to the graph and color, add to the tempMap*)
(*Return the tempMap*)
(*If we find that the graph is impossible to color, raise an exception or throw an error message or something*)
fun select(rgraph, tempMap, []) = (rgraph, tempMap)
    | select(rgraph, tempMap, nodeStack) =
        (*pop from stack, add element to graph*)
        let val ((temp, nList), newStack) = pop(nodeStack)
            val augGraph = Liveness.TempGraph.addNode(rgraph, temp, temp)
            (*iterate over list of neighbors, create edges, remove color of neighbors from list*)
            fun handleNeighbor (neighbor, (graph, set)) =
                let val newGraph = Liveness.TempGraph.addEdge(graph, {from=temp, to=neighbor})
                    val newSet = case Temp.Map.find(tempMap, neighbor) of
                                SOME(color) => StringSet.delete(set, color)
                                | NONE => (print("ERROR: Neighboring node is not yet colored"); set)
                in (newGraph, newSet)
                end
                handle NotFound => (print "Multiple neighbors colored the same, we good";
				   (Liveness.TempGraph.addEdge(graph, {from=temp, to=neighbor}), set))
            val (updatedGraph, validColors) = foldl handleNeighbor (augGraph, regSet) nList
            (*choose first from list to color this node, recurse*)
        in if StringSet.isEmpty(validColors)
           then (*TODO:Raise execption, cannot color!!!*) (*Do we want to spill idk*) (rgraph, tempMap)
           else select(updatedGraph, Temp.Map.insert(tempMap, temp, List.hd(StringSet.listItems(validColors))), newStack)
        end
