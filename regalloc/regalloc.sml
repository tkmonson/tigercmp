
structure StringSet = SplaySetFn(type ord_key = string val compare = String.compare)
val regSet = StringSet.addList(["$v0", "$v1",
                                "$a0", "$a1", "$a2", "$a3",
                                "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "t6$", "$t7", "$t8", "$t9",
                                "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7"])

(*Generate liveness information*)
(*Grab MipsFrame.tempMap*)
(*In interference graph, add all precolored temps that aren't already in the graph*)
(*Make all precolored temps interfere with each other (Appel possibly proposes an efficient/easy way to do this?)*)
(*Returns a Temp.Map and an interference graph*)
fun init filename = ()

fun push(element, list) = element::list

fun pop(list) = (List.hd(list), List.tl(list))

(*Remove all nodes of trivial degree until we reach a base case (single node) or can't simplify any further*)
(*returns the fully simplified graph, and a stack containing all the nodes that we have removed from the graph*)
(*Remember to take into account move edges when counting degree!*)
fun simplify(igraph, mgraph) = ()

(*Takes as arguments the output of simplify*)
(*Rebuild graph: First, color the base igraph which should be trivial. Then, add nodes from the stack and color each one*)
(*For each node that we add to the graph and color, add to the tempMap*)
(*Return the tempMap*)
(*If we find that the graph is impossible to color, raise an exception or throw an error message or something*)
fun select(rGraph, tempMap, []) = (rGraph, tempMap)
    | select(rGraph, tempMap, nodeStack) =
        (*pop from stack, add element to graph*)
        let val ((temp, eList), newStack) = pop(nodeStack)
            val augGraph = TempGraph.addNode(rGraph, temp, temp)
            (*iterate over list of neighbors, create edges, remove color of neighbors from list*)
            fun handleNeighbor (neighbor, (graph, set)) =
                let val newGraph = Liveness.TempGraph.addEdge(graph, {node, neighbor})
                    val newSet = case tempMap.look(neighbor) of
                                SOME(color) => StringSet.delete(set, color)
                                | NONE => (*TODO; Print error, should have all neighbors in table*) set
                in (newGraph, newSet)
                end
                handle NotFound => print "Multiple neighbors colored the same, we good"
            val (updatedGraph, validColors) = foldl handleNeighbor eList (augGraph, regSet)
            (*choose first from list to color this node, recurse*)
        in if Temp.set.isEmpty(validColors)
           then (*TODO:Raise execption, cannot color!!!*)
           else select(updatedGraph, TempMap.enter(tempMap, temp, List.hd(validColors), newStack)
        end
