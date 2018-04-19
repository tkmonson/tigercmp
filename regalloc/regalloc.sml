(*BUG: callee-saves are liveout of tig_main, but they never get stored/restored, so they conflict with every temp in tig_main. Options:
-Call procEntryExit1 on tig_main
-Don't make calleSaves liveOut in procEntryExit2 (shouldn't matter if they are or aren't...)
*)

structure StringSet = SplaySetFn(type ord_key = string val compare = String.compare)
val regNodes = MipsFrame.argRegs @ MipsFrame.calleeSaves @ MipsFrame.callerSaves @MipsFrame.specials

(*Precolored temps*)
val pcTemps = map MipsFrame.getTemp regNodes

(*Colors of precolored nodes*)
val regSet = StringSet.addList(StringSet.empty, map MipsFrame.getRegName pcTemps)

(*Generate liveness information*)
(*In interference graph, add all precolored temps that aren't already in the graph*)
(*Make all precolored temps interfere with each other (Appel possibly proposes an efficient/easy way to do this?)*)
(*Returns an interference graph and a move graph*)
fun init filename =
  let
	 val (igraph,mgraph) = Liveness.main filename
	 val tempList = map Liveness.TempGraph.getNodeID (Liveness.TempGraph.nodes igraph)

   (*Adds a temp to the graph, unless it's already in the graph*)
   fun addTempToInterferenceGraph(temp, graph) = if (List.exists (fn node => node = temp) tempList)
                                                then graph
                                                else Liveness.TempGraph.addNode(graph,temp,temp)
      handle Liveness.TempGraph.NoSuchNode(id) => (print("No node of id " ^ Int.toString id ^ " in addTempToInterferenceGraph"); graph)

   (*Adds edges between the given temp and all the pc temps*)
   fun addPCEdges(temp, graph) = foldl (fn(pc, gr) => Liveness.TempGraph.doubleEdge(gr,temp,pc)) graph pcTemps

   (*Add all precolored nodes to the interference graph*)
   val igraphWithPCNodes = foldl addTempToInterferenceGraph igraph pcTemps
   (*Add edges from each precolored node to every precolored node, including self*)
   val igraphWithPCEdges = foldl addPCEdges igraphWithPCNodes pcTemps
  in
   (igraphWithPCEdges, mgraph)
  end



fun push(element, list) = element::list

fun pop(list) = (List.hd(list), List.tl(list))

(*Remove all nodes of trivial degree until we reach a base case (single node) or can't simplify any further*)
(*returns the fully simplified graph, and a stack containing all the nodes that we have removed from the graph*)
fun simplify(igraph, mgraph) =
  let
	 val change = ref 0
	 fun removeTrivials (graph, stack) =
	    let val (newgraph, newstack) = (foldl (fn (node, (ig,stk)) => if (Liveness.TempGraph.degree(node) < MipsFrame.numRegs)
	                                                                   then (change:=1; (Liveness.TempGraph.remove(ig, node),
                                                                                       push((Liveness.TempGraph.getNodeID(node),Liveness.TempGraph.adj(node)),stk)))
				                                                             else (ig,stk))
	                                           (graph,stack) (Liveness.TempGraph.nodes graph))
	     in if !change = 1
	        then (change:=0; removeTrivials(newgraph, newstack))
	        else (graph,stack)
       end
  in
	 removeTrivials(igraph, [])
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
    handle Empty => (print "Couldn't color all of the nodes in the simplified interference graph! Boo hoo!"; tempMap)

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
           then (print "AHHH FAILED TO COLOR THE GRAPH AHHHH"; (rgraph, tempMap))
           else select(updatedGraph, Temp.Map.insert(tempMap, temp, List.hd(StringSet.listItems(validColors))), newStack)
        end

  fun makeRegAllocMips (colorMap, instrs) =
      let fun printTemp t = let val color = Temp.Map.find(colorMap, t)
                            in case color of
                               SOME(reg) => reg
                               | NONE     => (print ("we done fucked up"); "")
                            end
          val format = Assem.format(printTemp)
        (* val () = Printtree.printtree(out,body) *)
      in
        map format instrs
      end

(*Map over fragments in main*)
  fun regAllocation (igraph, mgraph, instr)
      let val (augigraph, augmgraph) = init (igraph, mgraph)
          val (simpigraph, stack) = simplify (augigraph, augmgraph)
          val augTempMap = colorSimpGraph(simpigraph, MipsFrame.tempMap)
          val (finaligraph, colorMap) = select (simpigraph, augTempMap)
      in makeRegAllocMips (colorMap, instr) (*Returns a list of strings that is our mips code*)
      end
