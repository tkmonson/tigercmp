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

    let val labelNodeTable = Symbol.Table.empty()

        fun createAssemNodes (graph, []) = graph
            | createAssemNodes (graph, assem::(l:Assem.instr List))
              (let val newNode{ins=_, id=newID} = AssemNode.makeNode(a)
                   val lastNode{ins=_, id=oldID} = FlowGraph.getNode (graph, (id-1))
              in (
                (case assem of
                  OPER{assem=assem, dst=dlist, src=slist, jump=jOp} =>
                  | LABEL{assem=assem, lab=label} => Symbol.Table.enter(labelNodeTable, Symbol.NAME label, newID)
                  | MOVE =>
                  );
                  createAssemNodes(FlowGraph.addEdge(FlowGraph.addNode (graph, id, newNode), {from=oldID, to=newID}), l))
              end)

        fun createControlFlow (graph, []) = graph
            | createControlFlow (graph, node{ins=assem, id=ID}::(l:AssemNode list)) =
                (let newGraph =
                  case assem of
                      OPER{assem=assem, dst=dList, src=sList, jump=jListOp} =>
                          case jListOp of
                              SOME(jList) => makeJumpEdges(graph, node, jList)
                              | NONE => graph
                      | LABEL{assem=assem, label=label} => (*do nothing*) graph
                      | MOVE{assem=assem, dst=dst, src=src} => (*do nothing*) graph
                in createControlFlow (newGraph, l)
                end)

        (*look up the node for the assem in the Table
        look up the node for the jump label in Table
        create an edge between the assem node and the label node
        add edges to graph*)
        fun makeJumpEdges (graph, node, []) = graph
            | makeJumpEdges (graph, node{ins=_, id=ID}, label::jList) =
                let val labelNode{ins=_, id=lID} = Symbol.Table.look (labelNodeTable, Symbol.NAME label
                in (FlowGraph.addEdge(graph, {from=id, to=lID}), node, jList)
                end
    in
    end

    (*make new node of a*)
