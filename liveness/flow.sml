structure FlowGraph = FuncGraph(type ord_key = int val compare = Int.compare)

structure Table = IntMapTable(type key = node
      fun getInt(g,n) = n)

datatype ControlFlow = CONFLOW of {control: FlowGraph,
                                   nodeToAssem: ORD_MAP,
                                   def: Temp.temp list Table,
                                   use: Temp.temp list Table,
                                   ismove: bool Table}

fun createGraph [] = (*Print error because we shouldn't make a graph without nodes*) FuncGraph.empty()
    | createGraph(a::l:Assem.instr list) =

    let fun createAssemNodes (graph, []) = graph
            | createAssemNodes (graph, assem::(l:Assem.instr List))
              (let val newNode{ins=_, id=newID} = AssemNode.makeNode(a)
                   val lastNode{ins=_, id=oldID} = FlowGraph.getNode (graph, (id-1))
              in (Table.enter(nodeToIDTable, ID, newNode);
                 createAssemNodes(FlowGraph.addEdge(FlowGraph.addNode (graph, id, newNode), {from=oldID, to=newID}), l))
              end)

        fun createJumpEdges (graph, []) = graph
            | createJumpEdges (graph, assem::(l:Assem.instr list)) =
                  (let newGraph =
                      case assem of
                          OPER{assem=assem, dst=dList, src=sList, jump=jListOp} =>
                              case jListOp of
                                  SOME(jList) => (*look up the node for the assem in the Table
                                                   look up the node for the jump label in Table
                                                   create an edge between the assem node and the label node
                                                   add edges to graph*)
                                  | NONE => graph
                          | LABEL{assem=assem, label=label} => (*do nothing*) graph
                          | MOVE{assem=assem, dst=dst, src=src} => (*do nothing*) graph
                  in createJumpEdges (newGraph, l)
                  end)
    in
    end

    (*make new node of a*)
