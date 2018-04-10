structure Liveness =
struct

structure IntSet = SplaySetFn(type ord_key = int val compare = Int.compare)
structure TempSet = Temp.Set
structure FlowGraph = Flow.FlowGraph
structure Table = Flow.Table

val updated = ref false


  (*Performs DFS on the graph from a particular node, keeping track of nodes visited along the way*)
  (*Returns all sinks reachable from current node*)
  fun findSinks(graph, curID, visited, sinks) =
    let val curNode = FlowGraph.getNode(graph, curID)
        val succIDs = FlowGraph.succs(curNode)
        val succIDSet = IntSet.addList(IntSet.empty, succIDs)
        val newVisited = IntSet.add(visited, curID)
        val notYetVisited = IntSet.difference(succIDSet, newVisited)
        val newSinks = if IntSet.numItems(notYetVisited) = 0 then IntSet.add(sinks, curID) else sinks
        fun findSinksWrapper(id, sin) = findSinks(graph, id, newVisited, sin)
    in
        foldl findSinksWrapper newSinks (IntSet.listItems(notYetVisited))
    end


  (*Calls findSinks for each unvisited node in the graph*)
  (*Returns a list of all sinks and a set of all sources*)
  (* fun genSinkList(sinks, sources, graph) =
        let val allIDs = map FlowGraph.getNodeID (FlowGraph.nodes(graph))
            val allIdSet = IntSet.addList(IntSet.empty, allIDs)
            val nonVisitedList = IntSet.listItems(IntSet.difference(allIdSet, !visited))
        in case nonVisitedList of
                []     => (sinks, sources)
              | (a::l) => genSinkList(findSinks(graph, a)@sinks, IntSet.add(sources, a), graph)
        end *)

fun getLiveOuts (liveInTable, liveOutTable, graph, node) =
    let val AssemNode.ASNODE{ins=_,id=id} = FlowGraph.nodeInfo(node)
        val successors = map (fn nodeID => FlowGraph.getNode(graph, nodeID)) (FlowGraph.succs(node))
        fun calcLiveOuts(n) =
            let val AssemNode.ASNODE{ins=ins, id=nid} = FlowGraph.nodeInfo(n)
                val succLiveIns = Table.look(liveInTable, nid)
            in case succLiveIns of
                   SOME(ins) => (*)(print("Unioning successor " ^ Int.toString nid ^ " liveins to calc live out for " ^ Int.toString id ^ "\n"); ins) *) ins
                   | NONE => (print ("This shouldn't happen, couldn't find successor for node " ^ Int.toString id ^ " in liveIns table!\n"); TempSet.empty)
            end
        val succLiveInList = map calcLiveOuts successors
        val liveOut = foldl TempSet.union TempSet.empty succLiveInList
    in Table.enter(liveOutTable, id, liveOut)
    end

fun calcLiveIns (uses, defs, liveOuts) =
    let val useSet = TempSet.addList(TempSet.empty, uses)
        val defSet = TempSet.addList(TempSet.empty, defs)
    in TempSet.union(useSet, TempSet.difference(liveOuts, defSet))
    end

fun getLiveIns (liveInTable, liveOutTable, node as AssemNode.ASNODE{ins=ins, id=id}) =
    let val liveOuts = case Table.look(liveOutTable, id) of
                       SOME(s) => s
                     | NONE    => (print ("Couldn't find liveouts for node " ^ Int.toString id ^ "\n"); TempSet.empty)
        val (uses, defs) = case ins of
                                Assem.OPER{assem=_, dst=dst, src=src, jump=_} => (src, dst)
                                | Assem.LABEL(x) => ([],[])
                                | Assem.MOVE{assem=_, dst=dst, src=src} => ([src], [dst])
    in Table.enter(liveInTable, id, calcLiveIns(uses, defs, liveOuts))
    end

fun calcEq(table1, table2, id) =
  let val set1 = case Table.look(table1, id) of
                   SOME(s) => s
                 | NONE    => TempSet.empty
      val set2 = case Table.look(table2, id) of
                   SOME(s) => s
                 | NONE    => TempSet.empty
  in TempSet.equal(set1, set2)
  end

(*node: AssemNode.node FlowGraph.node*)
(*predsList: AssemNode.node FlowGraph.node list *)
(*Performs graph traversal for liveness analysis from current node until it reaches a source*)
(*Returns updated livein and liveout tables*)
fun update(node, liveIns, liveOuts, source, graph) =
    let val newLiveOuts = getLiveOuts(liveIns, liveOuts, graph, node)
        val id = FlowGraph.getNodeID(node)
        val newLiveIns = getLiveIns(liveIns, newLiveOuts, FlowGraph.nodeInfo(node))
        val () = if not (calcEq(liveIns, newLiveIns, id)) orelse not (calcEq(liveOuts, newLiveOuts, id)) then updated := true else ()
        val predsList = map (fn nodeID => FlowGraph.getNode(graph, nodeID)) (FlowGraph.preds(node))
        fun updatePreds(a, (predIns, predOuts)) = update(a, predIns, predOuts, source, graph)
    in
        if (FlowGraph.getNodeID node = FlowGraph.getNodeID source)
        then (newLiveIns, newLiveOuts)
        else foldl updatePreds (newLiveIns, newLiveOuts) predsList
    end

  (*Calls update from each sink in the graph, accumulates updated liveness tables*)
  (*If !updated is true, set to false and repeat*)
  (*Else, return *)
  fun genLivenessInfo(LI, LO, source, graph, sinkList) =
    let fun updateWrapper(node, (liveIns, liveOuts)) = update(node, liveIns, liveOuts, source, graph)
        val (newLI, newLO) = foldl updateWrapper (LI, LO) sinkList
    in
      if !updated then (print ("updating liveness info\n"); updated := false; genLivenessInfo(newLI, newLO, source, graph, sinkList))
                  else (newLI, newLO)
    end

fun initLivenessTable(graph) =
    let val idList = map FlowGraph.getNodeID (FlowGraph.nodes(graph))
    in foldl (fn(id, table) => Table.enter(table, id, TempSet.empty)) Table.empty idList
    end

fun testSinks(filename) =
    let val graphLists = Flow.generateFlowInfo(filename)
        val graph = List.hd(graphLists)
        val sinks = map (fn(nid) => FlowGraph.nodeInfo(FlowGraph.getNode(graph, nid))) (IntSet.listItems(findSinks(graph, 0, IntSet.empty, IntSet.empty)))
        fun printSinks (AssemNode.ASNODE{ins=ins, id=id}) = print ("Sink node of id " ^ Int.toString id ^
                                                                   " with assem " ^ Assem.format(MipsGen.printTemp) ins ^ "\n")
    in map printSinks sinks
    end

fun testLiveness(filename) =
    let val graphLists = Flow.generateFlowInfo(filename)
        val graph = List.hd(graphLists)
        val source = FlowGraph.getNode(graph, 0)
        val sinks = map (fn (id) => FlowGraph.getNode(graph, id)) (IntSet.listItems(findSinks(graph, 0, IntSet.empty, IntSet.empty)))
    in genLivenessInfo(initLivenessTable graph, initLivenessTable graph, source, graph, sinks)
    end

fun printLivenessInfo(liveIn, liveOut, id) =
    let val liveInSet = Table.look(liveIn, id)
        val liveOutSet = Table.look(liveOut, id)
        fun printSet(set) = app (fn(setInt) => print("temp " ^ Int.toString setInt ^ ", ")) (TempSet.listItems set)
    in case (liveInSet, liveOutSet) of
       (NONE, _) => print("ERROR, node of id " ^ Int.toString id ^ " does not exist in liveIns\n")
       | (_, NONE) => print("ERROR, node of id " ^ Int.toString id ^ " does not exist in liveOuts\n")
       |(SOME(ins), SOME(outs)) => (print("LiveIns of node " ^ Int.toString id ^ ":\n"); printSet(ins);
                                   print("\nLiveOuts of node " ^ Int.toString id ^ ":\n"); printSet(outs))
    end
    (*TO TEST: Call Flow.generateFlowInfo to get a list of graphs
               To get source, get graph node with ID = 0 in each graph
               call find sinks on that node
               call genLivenessInfo on sink + source node *)

end
