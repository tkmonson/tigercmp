(*BUG: callee-saves are liveout of tig_main, but they never get stored/restored, so they conflict with every temp in tig_main. Options:
-Call procEntryExit1 on tig_main
-Don't make calleSaves liveOut in procEntryExit2 (shouldn't matter if they are or aren't...)
*)

structure RegAlloc =
struct

structure StringSet = SplaySetFn(type ord_key = string val compare = String.compare)
val regNodes =  MipsFrame.callerSaves @ MipsFrame.argRegs @ MipsFrame.calleeSaves @MipsFrame.specials

(*Precolored temps*)
val pcTemps = map MipsFrame.getTemp regNodes

(*Colors of precolored nodes*)
val regSet = StringSet.addList(StringSet.empty, map MipsFrame.getRegName pcTemps)
val selectSet = foldl (fn(item, set) => StringSet.delete(set, item)) regSet ["$ra", "$sp", "$zero", "$fp"]

(* Input: a list of (igraph,mgraph) tuples *)
fun init (igraph,mgraph) =
  let
      val tempList = map Liveness.TempGraph.getNodeID (Liveness.TempGraph.nodes igraph)

   (*Adds a temp to the graph, unless it's already in the graph*)
   fun addTempToInterferenceGraph(temp, graph) = if (List.exists (fn node => node = temp) tempList)
                                                then graph
                                                else Liveness.TempGraph.addNode(graph,temp,temp)
      handle Liveness.TempGraph.NoSuchNode(id) => (print("No node of id " ^ Int.toString id ^ " in addTempToInterferenceGraph\n"); graph)

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

fun printList [] = "\n"
|   printList (a::l) = Int.toString(a) ^ ", " ^ (printList l)

(*Remove all nodes of trivial degree until we reach a base case (single node) or can't simplify any further*)
(*returns the fully simplified graph, and a stack containing all the nodes that we have removed from the graph*)
fun simplify(igraph, mgraph) =
  let
	 val change = ref 0
	 fun removeTrivials (graph, stack) =
	    let fun removeNode (id, (ig,stk)) = let   val node = Liveness.TempGraph.getNode(ig, id)
                                                val adjs = Liveness.TempGraph.adj'(ig) (node)
                                                val neighborIDs = map Liveness.TempGraph.getNodeID (Liveness.TempGraph.adj'(ig) (node))
                                                val rmgraph = Liveness.TempGraph.removeNode(ig, id)
                                          in   if (Liveness.TempGraph.degree(node) < MipsFrame.numRegs)
	                                             then (change:=1; (rmgraph, push((id, neighborIDs), stk)))
				                                       else (ig,stk)
                                            end
          val (newgraph, newstack) = foldl removeNode (graph, stack) (map Liveness.TempGraph.getNodeID (Liveness.TempGraph.nodes(graph)))
	     in if !change = 1
	        then (change:=0; removeTrivials(newgraph, newstack))
	        else (graph,stack)
       end
  in
	 removeTrivials(igraph, [])
  end

fun pickReg [] = (print "Error, reg list empty!\n"; "SPILL")
    | pickReg (regList) =
        let val tList = List.filter (fn (s) => String.substring(s, 1, 1) = "t") regList
        in if List.length(tList) > 0 then List.hd(tList) else List.hd(regList)
        end

fun colorSimpGraph(sgraph, tempMap) =
    let val nodes = Liveness.TempGraph.nodes(sgraph)
		    fun color(temp, map) =
				    (case Temp.Map.find(tempMap, Liveness.TempGraph.nodeInfo(temp)) of
                  SOME(color) => map (*temp is precolored, don't overwrite just in case!*)
                  | NONE => (*temp not yet in color map, let's try to color*)
                      (let val neighbors = Liveness.TempGraph.adj(temp)
                           fun getValidColors(neighbor, set) =
                               case Temp.Map.find(map, neighbor) of
                                    SOME(color) => if StringSet.member(set, color) then StringSet.delete(set, color) else set
                                    | NONE => set
                           val validColors = foldl getValidColors selectSet neighbors
                       in if StringSet.isEmpty(validColors)
                          then (print ("Couldn't color node " ^ Int.toString (Liveness.TempGraph.nodeInfo temp) ^ " in the simplified interference graph! Boo hoo!\n");
                               Temp.Map.insert(map, Liveness.TempGraph.nodeInfo(temp), "SPILL"))
                          else Temp.Map.insert(map, Liveness.TempGraph.nodeInfo(temp), pickReg(StringSet.listItems(validColors)))
                       end))
            (* handle NotFound => (print "handle function for colorSimpGraph"; map)) *)
		in foldl color tempMap nodes
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
                let
                    val newGraph = Liveness.TempGraph.addEdge(graph, {from=temp, to=neighbor})
                    val newSet = case Temp.Map.find(tempMap, neighbor) of
                                SOME(color) => if StringSet.member(set, color) then StringSet.delete(set, color) else set
                                | NONE => (print("ERROR: Neighboring node " ^ Int.toString neighbor ^ " is not yet colored\n"); set)
                in (newGraph, newSet)
                end
                (* handle Liveness.TempGraph.NoSuchNode(id) => (print("Couldn't find node " ^ Int.toString id ^ " When adding node " ^ Int.toString temp ^ "\n"); (graph, set)) *)
            val (updatedGraph, validColors) = foldl handleNeighbor (augGraph, selectSet) nList
            (*choose first from list to color this node, recurse*)
        in if StringSet.isEmpty(validColors)
           then (print "AHHH FAILED TO COLOR THE GRAPH AHHHH\n"; (rgraph, tempMap))
           else select(updatedGraph, Temp.Map.insert(tempMap, temp, List.hd(StringSet.listItems(validColors))), newStack)
        end

  fun makeRegAllocMips (colorMap, instrs) =
      let fun printTemp t = let val color = Temp.Map.find(colorMap, t)
                            in case color of
                               SOME(reg) => reg
                               | NONE     => (print ("There's no color for " ^ Int.toString t ^ " we done fucked up\n"); "SPILL")
                            end
          val format = Assem.format(printTemp)
        (* val () = Printtree.printtree(out,body) *)
      in
        map format instrs
      end

(*Map over fragments in main*)
  fun regAllocation (igraph, mgraph, instr) =
      let val (augigraph, augmgraph) = init (igraph, mgraph)
          (* val () = print "called init" *)
          val (simpigraph, stack) = simplify (augigraph, augmgraph)
          (* val () = print "called simplify" *)
          val augTempMap = colorSimpGraph(simpigraph, MipsFrame.tempMap)
          (* val () = print "called colorSimpGraph" *)
          val (finaligraph, colorMap) = select (simpigraph, augTempMap, stack)
          (* val () = print "called select" *)
      in makeRegAllocMips (colorMap, instr) (*Returns a list of strings that is our mips code*)
      end

end
