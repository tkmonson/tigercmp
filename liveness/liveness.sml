
structure IntSet = SplaySetFn(type ord_key = int val compare = Int.compare)
structure FlowGraph = Flow.FlowGraph

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


  (*Call genSinkList to get all sinks and sources for graph -> sinklist, sourceset*)
  (*Initialize liveIn and liveOut sets (maybe as refs?) *)
  (*fun traverseSinks: For each sink in sinklist, call traverseNode*)
  (*fun traverseNode: Update liveIn and liveOut of current node; If current node is not a source, call traverseNode on all predecessors*)
