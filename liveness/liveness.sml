structure Liveness =
struct

structure IntSet = SplaySetFn(type ord_key = int val compare = Int.compare)
structure TempSet = Flow.TempSet
structure FlowGraph = Flow.FlowGraph
structure TempGraph = FuncGraph(type ord_key = Temp.temp val compare = Int.compare)
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
fun update(node, liveIns, liveOuts, visited, graph) =
    let val newLiveOuts = getLiveOuts(liveIns, liveOuts, graph, node)
        val id = FlowGraph.getNodeID(node)
        val newLiveIns = getLiveIns(liveIns, newLiveOuts, FlowGraph.nodeInfo(node))
        val () = if not (calcEq(liveIns, newLiveIns, id)) orelse not (calcEq(liveOuts, newLiveOuts, id)) then updated := true else ()
        val predsList = map (fn nodeID => FlowGraph.getNode(graph, nodeID)) (FlowGraph.preds(node))
        val newVisited = IntSet.add(visited, id)
        fun updatePreds(a, (predIns, predOuts)) = update(a, predIns, predOuts, newVisited, graph)
    in
        if IntSet.member(visited, id)
        then (newLiveIns, newLiveOuts)
        else foldl updatePreds (newLiveIns, newLiveOuts) predsList
    end

  (*Calls update from each sink in the graph, accumulates updated liveness tables*)
  (*If !updated is true, set to false and repeat*)
  (*Else, return *)
  fun genLivenessInfo(LI, LO, graph, sinkList) =
    let fun updateWrapper(node, (liveIns, liveOuts)) = update(node, liveIns, liveOuts, IntSet.empty, graph)
        val (newLI, newLO) = foldl updateWrapper (LI, LO) sinkList
    in
      if !updated then (print ("updating liveness info\n"); updated := false; genLivenessInfo(newLI, newLO, graph, sinkList))
                  else (newLI, newLO)
    end

  fun addEdges(lgraph, defs, liveOuts, isMove, uses) =
    let val useSet = IntSet.addList(IntSet.empty, uses)
        (*Creates edges from a particular temp (def) to the list of liveouts*)
        (*If insn is move a <- b then it does not create an edge between a and b as per textbook pg 222*)
        fun createEdgesForDef(def, graph) = foldl (fn(lout, gr) => if (isMove) andalso (IntSet.member(useSet, lout)) then gr
                                                                else TempGraph.doubleEdge(gr, def, lout))
                                                  graph
                                                  (TempSet.listItems(liveOuts))
    (*call createEdgesForDef on each def and accumulate the result*)
    in foldl createEdgesForDef lgraph defs
    end

    (*In move graph, add an edge between each def and each use*)
    (*We're guaranteed that for MIPS, a move will have 1 def and 1 use, so List.hd should be fine*)
    fun addMoveEdges(mgraph, defs, uses) =
      let val use = List.hd(uses)
          val def = List.hd(defs)
      in TempGraph.doubleEdge(mgraph, use, def)
      end
    handle Empty => (print "Encountered a move with empty use or def list!"; mgraph)

  fun makeInterferenceGraph(liveOutTable, flowGraph, defTable, moveTable, useTable, starterLGraph, starterMGraph) =
    let fun addInterferenceEdges (flowNode, (lgraph, mgraph)) =
            let val AssemNode.ASNODE{ins=_, id=id} = FlowGraph.nodeInfo(flowNode)
                val liveOuts = case Flow.look(liveOutTable, id) of
                                SOME(x) => x
                              | NONE    => (print("Couldn't find any liveOuts for node " ^ Int.toString id ^ "\n"); TempSet.empty)
                val defs = case Flow.look(defTable, id) of
                          SOME(d) => d
                        | NONE    => []
                val uses = case Flow.look(useTable, id) of
                                SOME(d) => d
                              | NONE    => []
                val isMove = case Flow.look(moveTable, id) of
                             SOME(x) => x
                            |NONE    => (print("No isMove value for node " ^ Int.toString id ^ "\n"); false)
                val newLGraph = addEdges(lgraph, defs, liveOuts, isMove, uses)
                val newMGraph = if isMove then addMoveEdges(mgraph, defs, uses) else mgraph
            in (newLGraph, newMGraph)
            end
    in
      foldl addInterferenceEdges (starterLGraph, starterMGraph) (FlowGraph.nodes(flowGraph))
    end

fun initLivenessTable(graph) =
    let val idList = map FlowGraph.getNodeID (FlowGraph.nodes(graph))
    in foldl (fn(id, table) => Flow.enter(table, id, TempSet.empty)) Flow.empty idList
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
        val sinks = map (fn (id) => FlowGraph.getNode(graph, id)) (IntSet.listItems(findSinks(graph, 0, IntSet.empty, IntSet.empty)))
    in genLivenessInfo(initLivenessTable graph, initLivenessTable graph, graph, sinks)
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

  fun createBaseIntGraph(tempSet) =
    let val tempList = TempSet.listItems(tempSet)
    in
      foldl (fn(temp, graph) => TempGraph.addNode(graph, temp, temp)) TempGraph.empty tempList
    end

  (*Calls Flow.main, which returns a list of tuples of form (conflow, instr list)*)
  (*Returns a list of tuples of form (interference graph, move graph, instr list)*)
  fun main filename =
    let val fragList = Flow.main filename
        fun handleFrag(cflow, instrs) =
        let
          val Flow.CONFLOW{control=gr, def=d, use=u, ismove=im, temps=t} = cflow
          val sinks = map (fn (id) => FlowGraph.getNode(gr, id))
                          (IntSet.listItems(findSinks(gr, 0, IntSet.empty, IntSet.empty)))
          val (liveIns, liveOuts) = genLivenessInfo(initLivenessTable gr, initLivenessTable gr, gr, sinks)
          val baseInterferenceGraph = createBaseIntGraph(t)
          val (intGraph, moveGraph) = makeInterferenceGraph(liveOuts, gr, d, im, u, baseInterferenceGraph, baseInterferenceGraph)
        in
          (intGraph, moveGraph, instrs)
        end
    in
      map handleFrag fragList
    end

    fun printTempGraph gr =
      let fun pr(id,_) = MipsGen.printTemp id
      in
        TempGraph.printGraph pr gr
      end
end
