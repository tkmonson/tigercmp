
structure IntSet = SplaySetFn(type ord_key = int val compare = Int.compare)
structure TempSet = SplaySetFn(type ord_key = Temp.temp val compare = Temp.TempOrd.compare)
structure FlowGraph = Flow.FlowGraph
structure Table = Flow.Table

  val visited = ref IntSet.empty

  (*Performs DFS on the graph from a particular node*)
  (*Side effect: Updates visited list*)
  (*Returns all sinks reachable from current node*)
  fun findSinks(graph, curID) =
    let val curNode = FlowGraph.getNode(graph, curID)
        val succIDs = FlowGraph.succs(curNode)
        val succIDSet = IntSet.addList(IntSet.empty, succIDs)
        val notYetVisited = IntSet.difference(succIDSet, !visited)
        val newVisited = visited := IntSet.add(!visited, curID)
        fun findSinksWrapper(id) = findSinks(graph, id)
    in if IntSet.numItems(notYetVisited) = 0 then [curID]
       else List.concat(map (findSinksWrapper) (IntSet.listItems(notYetVisited)))
    end


  (*Calls findSinks for each unvisited node in the graph*)
  (*Returns a list of all sinks and a set of all sources*)
  fun genSinkList(sinks, sources, graph) =
        let val allIDs = map FlowGraph.getNodeID (FlowGraph.nodes(graph))
            val allIdSet = IntSet.addList(IntSet.empty, allIDs)
            val nonVisitedList = IntSet.listItems(IntSet.difference(allIdSet, !visited))
        in case nonVisitedList of
                []     => (sinks, sources)
              | (a::l) => genSinkList(findSinks(graph, a)@sinks, IntSet.add(sources, a), graph)
        end

val updated = ref false

fun calcLiveOuts((node as AssemNode.ASNODE{ins=ins, id=id}), liveInTable) =
    let val successors = TempSet.addList(TempSet.empty, FlowGraph.succs(node))
        val succLiveIns = Table.look(liveInTable, id)
    in case succLiveIns of
           SOME(ins) => TempSet.listItems(TempSet.union(TempSet.addList(TempSet.empty, succLiveIns), successors))
           | NONE => (print ("This shouldn't happen, couldn't find successor in liveIns table!"); liveIns)
    end

fun getLiveOuts (liveInTable, liveOutTable, node as {ins=_, id=id}) =
    let val successors = FlowGraph.succs(node)
    in Table.enter(liveOutTable, id, foldl(calcLiveOuts successors []))
    end

fun calcLiveIns (uses, defs, liveOuts) =
    let val useSet = TempSet.addList(TempSet.empty, uses)
        val defSet = TempSet.addList(TempSet.empty, defs)
        val outSet = TempSet.addList(TempSet.empty, liveOuts)
    in TempSet.listItems(TempSet.union(useSet, TempSet.difference(outSet, defSet)))
    end

fun getLiveIns (liveInTable, liveOutTable, node as AssemNode.ASNODE{ins=ins, id=id}) =
    let val liveOuts = Table.look(liveOutTable, id)
        val (uses, defs) = case ins of
                                OPER{assem=_, dst=dst, src=src, jump=_} => (src, dst)
                                | LABEL(x) => ([],[])
                                | MOVE{assem=_, dst=dst, src=src} => (src, dst)
    in Table.enter(liveInTable, id, calcLiveIns(uses, defs, liveOuts))
    end

(*
Args: liveInTable, liveOutTable, nodeID
fun calcliveIns:
  -calculate liveOuts - defs
  -calculate uses UNION liveOuts-defs, return result
*)

(*
Args: liveInTable, liveOutTable, nodeID
fun calcliveOuts:
  -call FlowGraph.succs to get all successors of current node
  -Union liveIns for all Successors, return result
*)


(*

Arguments: curNode, liveInTable, liveOutTable, source
Function update:

-calculate liveOuts, store as newLiveOut
-calculate liveIns, store as newLiveIn
-if newLiveOut != liveOuts OR newLiveIns != liveIns, set converged = false
-If curNode = source, return (liveOuts, liveIns, converged)
-call FlowGraph.preds to get predecessors of current node
fold update over all predecessors with updated liveIn and liveOut tables, and return result

liveIns = uses UNION (liveouts - defs)
liveOuts = UNION over LiveIns of all successors

*)

fun update(node, liveIns, liveOuts) =
    let val newLiveOuts = calcLiveOuts(liveIns, liveOuts, node)
        val newLiveIns = calcLiveIns(liveIns, liveOuts, node)
        val predsList = FlowGraph.preds(node)
    in
        if (node = source andalso not updated)
        then (newLiveIns, newLiveOuts)
        else foldl updatePreds (newLiveIns, newLiveOuts) predslist
    end


fun updatePreds([], (liveIns, liveOuts)) = (liveIns, liveOuts)
    | updatePreds(a::preds, (liveIns, liveOuts)) = update(a, liveIns, liveOuts)
