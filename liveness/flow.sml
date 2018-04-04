structure Flow =
struct

structure FlowGraph = FuncGraph(type ord_key = int val compare = Int.compare)

structure Table = IntMapTable(type key = int
      fun getInt(i) = i)

type 'a table  = 'a Table.table
val empty = Table.empty
val enter = Table.enter
val look = Table.look

datatype ControlFlow = CONFLOW of {control: AssemNode.node FlowGraph.graph,
                                   def: Temp.temp list table,
                                   use: Temp.temp list table,
                                   ismove: bool table}

fun createGraph [] = (*Print error because we shouldn't make a graph without nodes*) CONFLOW{control=FlowGraph.empty, def=empty, use=empty, ismove=empty}
    | createGraph(a::l:Assem.instr list) =

    (let val labelNodeTable = Symbol.empty

         fun hasJump(graph, ID) = let val AssemNode.ASNODE{ins=assem, id=_} = FlowGraph.nodeInfo(FlowGraph.getNode (graph, ID))
                                  in case assem of
                                          Assem.OPER{assem=_, dst=_, src=_, jump=jListOp} => (case jListOp of
                                                                                            SOME(list) => true
                                                                                            | NONE => false)
                                          | Assem.LABEL(x) => false
                                          | Assem.MOVE(x) => false
                                  end

       (*TODO: FIRST ASSEM NODE MUST BE DEALT WITH OUTSIDE OF FN*)
        fun createAssemNodes (conFlow, labelTable, []) = (conFlow, labelTable)
            | createAssemNodes (CONFLOW{control=graph, def=def, use=use, ismove=ismove}, labelTable, assem::(l:Assem.instr list)) =
              (let val AssemNode.ASNODE{ins=i, id=newID} = AssemNode.makeNode(a)
                   val (newLabelTable, newDef, newUse, newIsMove) =
                        case assem of
                             Assem.OPER{assem=assem, dst=dlist, src=slist, jump=jOp} => (labelTable,
                                                                                   enter(def, newID, dlist),
                                                                                   enter(use, newID, slist),
                                                                                   enter(ismove, newID, false))
                             | Assem.LABEL{assem=assem, lab=label} => (Symbol.enter(labelNodeTable, label, newID),
                                                                 enter(def, newID, []),
                                                                 enter(use, newID, []),
                                                                 enter(ismove, newID, false))
                             | Assem.MOVE{assem=assem, dst=dlist, src=slist} => (labelTable,
                                                                           enter(def, newID, [dlist]),
                                                                           enter(use, newID, [slist]),
                                                                           enter(ismove, newID, true))
                    val newGraph = if hasJump(graph, newID-1)
                                   then FlowGraph.addNode (graph, newID, AssemNode.ASNODE{ins=i, id=newID})
                                   else FlowGraph.addEdge(FlowGraph.addNode (graph, newID, AssemNode.ASNODE{ins=i, id=newID}), {from=newID-1, to=newID})
               in
                  createAssemNodes(CONFLOW{control=newGraph,
                                           def=newDef,
                                           use=newUse,
                                           ismove=newIsMove},
                                   newLabelTable,
                                   l)
              end)


        (*look up the node for the assem in the Table
        look up the node for the jump label in Table
        create an edge between the assem node and the label node
        add edges to graph*)
        fun makeJumpEdges (graph, node, labelNodeTable, []) = graph
            | makeJumpEdges (graph, AssemNode.ASNODE{ins=ins, id=ID}, labelNodeTable, label::jList) =
                let val lID = case Symbol.look (labelNodeTable, label) of
                                                SOME(id)  => id
                                              | NONE      => (*TODO: Print error here*) ~1
                in makeJumpEdges(FlowGraph.addEdge(graph, {from=ID, to=lID}), AssemNode.ASNODE{ins=ins, id=ID}, labelNodeTable, jList)
                end

        fun createControlFlow (graph, labelNodeTable, []) = graph
            | createControlFlow (graph, labelNodeTable, AssemNode.ASNODE{ins=assem, id=ID}::(l:AssemNode.node list)) =
                (let val newGraph =
                  case assem of
                      Assem.OPER{assem=_, dst=_, src=_, jump=jListOp} =>
                          (case jListOp of
                              SOME(jList) => makeJumpEdges(graph, AssemNode.ASNODE{ins=assem, id=ID}, labelNodeTable, jList)
                              | NONE => graph)
                      | Assem.LABEL(x) => (*do nothing*) graph
                      | Assem.MOVE(x) => (*do nothing*) graph
                in createControlFlow (newGraph, labelNodeTable, l)
                end)

         val AssemNode.ASNODE{ins=ins, id=id} = AssemNode.makeNode(a)
         val starterGraph = FlowGraph.addNode(FlowGraph.empty, id, AssemNode.ASNODE{ins=ins, id=id})
         val starterConflow = CONFLOW{control=starterGraph,
                                      def=empty,
                                      use=empty,
                                      ismove=empty}

         val(CONFLOW{control=c, def=d, use=u, ismove=i}, labelTable) = createAssemNodes(starterConflow, Symbol.empty, l)
         val finishedFlowGraph = createControlFlow(c, labelTable, map FlowGraph.nodeInfo (FlowGraph.nodes(c)))

    in
      CONFLOW{control=finishedFlowGraph, def=d, use=u, ismove=i}
    end)

end
