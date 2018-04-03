structure FlowGraph = FuncGraph(type ord_key = int val compare = Int.compare)

structure Table = IntMapTable(type key = node
      fun getInt(g,n) = n)

datatype ControlFlow = CONFLOW of {control: FlowGraph,
                                   def: Temp.temp list Table,
                                   use: Temp.temp list Table,
                                   ismove: bool Table}

fun createGraph [] = (*Print error because we shouldn't make a graph without nodes*) FuncGraph.empty
    | createGraph(a::l:Assem.instr list) =

    (let val labelNodeTable = Symbol.Table.empty

       (*TODO: FIRST ASSEM NODE MUST BE DEALT WITH OUTSIDE OF FN*)
        fun createAssemNodes (conFlow, labelTable, []) = (conFlow, labelTable)
            | createAssemNodes ({control=graph, def=def, use=use, ismove=ismove}, labelTable, assem::(l:Assem.instr List))
              (let val newNode{ins=_, id=newID} = AssemNode.makeNode(a)
                   val (newLabelTable, newDef, newUse, newIsMove) =
                        case assem of
                             OPER{assem=assem, dst=dlist, src=slist, jump=jOp} => (labelTable,
                                                                                   Table.enter(def, newID, dlist),
                                                                                   Table.enter(use, newID, slist),
                                                                                   Table.enter(ismove, newID, false))
                             | LABEL{assem=assem, lab=label} => (Symbol.Table.enter(labelNodeTable, Symbol.NAME label, newID),
                                                                 Table.enter(def, newID, []),
                                                                 Table.enter(use, newID, []),
                                                                 Table.enter(ismove, newID, false))
                             | MOVE{assem=assem, dst=dlist, src=slist} => (labelTable,
                                                                           Table.enter(def, newID, dlist),
                                                                           Table.enter(use, newID, slist),
                                                                           Table.enter(ismove, newID, true))
               in
                  createAssemNodes(CONFLOW{control=FlowGraph.addEdge(FlowGraph.addNode (graph, id, newNode), {from=newID-1, to=newID}),
                                           def=newdef,
                                           use=newuse,
                                           ismove=newismove},
                                   newLabelTable,
                                   l)
              end)


        (*look up the node for the assem in the Table
        look up the node for the jump label in Table
        create an edge between the assem node and the label node
        add edges to graph*)
        fun makeJumpEdges (graph, node, labelNodeTable, []) = graph
            | makeJumpEdges (graph, node{ins=_, id=ID}, labelNodeTable, label::jList) =
                let val labelNode{ins=_, id=lID} = Symbol.Table.look (labelNodeTable, Symbol.NAME label)
                in makeJumpEdges(FlowGraph.addEdge(graph, {from=id, to=lID}), node, jList)
                end

        fun createControlFlow (graph, labelNodeTable, []) = graph
            | createControlFlow (graph, labelNodeTable, node{ins=assem, id=ID}::(l:AssemNode list)) =
                (let newGraph =
                  case assem of
                      OPER{assem=assem, dst=dList, src=sList, jump=jListOp} =>
                          case jListOp of
                              SOME(jList) => makeJumpEdges(graph, node, labelNodeTable, jList)
                              | NONE => graph
                      | LABEL{assem=assem, label=label} => (*do nothing*) graph
                      | MOVE{assem=assem, dst=dst, src=src} => (*do nothing*) graph
                in createControlFlow (newGraph, l)
                end)

         val {ins=ins, id=id} = AssemNode.makeNode(a)
         val starterGraph = FlowGraph.addNode(FlowGraph.empty, id, AssemNode.ASNODE{ins=ins, id=id})
         val starterConflow = CONFLOW{control=starterGraph,
                                      def=Table.empty,
                                      use=Table.empty,
                                      ismove=Table.empty}

         val({control=c, def=d, use=u, ismove=i}, labelTable) = createAssemNodes(starterConflow, Table.empty, l)
         val finishedFlowGraph = createControlFlow(c, labelTable, FlowGraph.nodes(c))

    in
      {control=finishedFlowGraph, def=d, use=u, ismove=i}
    end)
