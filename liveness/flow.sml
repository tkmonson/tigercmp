structure Flow =
struct

structure FlowGraph = FuncGraph(type ord_key = int val compare = Int.compare)


structure Table = IntMapTable(type key = int
      fun getInt(i) = i)

structure TempSet = Temp.Set

type 'a table  = 'a Table.table
val empty = Table.empty
val enter = Table.enter
val look = Table.look

datatype ControlFlow = CONFLOW of {control: AssemNode.node FlowGraph.graph,
                                   def: Temp.temp list table,
                                   use: Temp.temp list table,
                                   ismove: bool table,
                                   temps: TempSet.set}

fun createGraph [] = (print("Error, trying to make graph for conflow without any nodes"); CONFLOW{control=FlowGraph.empty, def=empty, use=empty, ismove=empty, temps=TempSet.empty})
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

        fun createAssemNodes (conFlow, labelTable, []) = (conFlow, labelTable)
            | createAssemNodes (CONFLOW{control=graph, def=def, use=use, ismove=ismove, temps=temps}, labelTable, assem::(l:Assem.instr list)) =
              (let val AssemNode.ASNODE{ins=i, id=newID} = AssemNode.makeNode(assem)
                   val (newLabelTable, newDef, newUse, newIsMove, newTemps) =
                        case assem of
                             Assem.OPER{assem=assem, dst=dlist, src=slist, jump=jOp} => (labelTable,
                                                                                   enter(def, newID, dlist),
                                                                                   enter(use, newID, slist),
                                                                                   enter(ismove, newID, false),
                                                                                   TempSet.addList(temps, dlist@slist))
                             | Assem.LABEL{assem=assem, lab=label} =>
                                                                  (Symbol.enter(labelTable, label, newID),
                                                                 enter(def, newID, []),
                                                                 enter(use, newID, []),
                                                                 enter(ismove, newID, false),
                                                                 temps)
                             | Assem.MOVE{assem=assem, dst=d, src=s} => (labelTable,
                                                                           enter(def, newID, [d]),
                                                                           enter(use, newID, [s]),
                                                                           enter(ismove, newID, true),
                                                                           TempSet.addList(temps, [d, s]))

                    val newGraph = if hasJump(graph, newID-1)
                                   then FlowGraph.addNode (graph, newID, AssemNode.ASNODE{ins=i, id=newID})
                                   else FlowGraph.addEdge(FlowGraph.addNode (graph, newID, AssemNode.ASNODE{ins=i, id=newID}), {from=newID-1, to=newID})
               in
                  createAssemNodes(CONFLOW{control=newGraph,
                                           def=newDef,
                                           use=newUse,
                                           ismove=newIsMove,
                                           temps=newTemps},
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
                                              | NONE      => (print ("ERROR: Looked up label " ^ Symbol.name label ^ " that doesn't exist in makeJumpEdges\n"); ~1)
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
                                      ismove=empty,
                                      temps=TempSet.empty}

         val(CONFLOW{control=c, def=d, use=u, ismove=i, temps=t}, labelTable) = createAssemNodes(starterConflow, Symbol.empty, l)
         val finishedFlowGraph = createControlFlow(c, labelTable, map FlowGraph.nodeInfo (FlowGraph.nodes(c)))

    in
      CONFLOW{control=finishedFlowGraph, def=d, use=u, ismove=i, temps=t}
    end)

    (*program = Assem.instr list list*)
    fun printFlowGraphs program =
      let fun testFragment(fragInstrs) =
            let
              val CONFLOW{control=gr, def=_, use=_, ismove=_, temps=_} = createGraph fragInstrs
              val reset = AssemNode.curID := 0
            in FlowGraph.printGraph(AssemNode.printNode) gr
            end
      in
        app testFragment program
      end

    (*program = Assem.instr list list*)
    (*one list per fragment*)
    fun test filename =
      let val program = MipsGen.transFrags filename
      in
        printFlowGraphs program
      end

  (*Generates a list of CONFLOW, with one CONFLOW per fragment in the program*)
    fun generateFlowInfo filename =
      let val program = MipsGen.transFrags filename
          fun processFragment fragInstrs=
            let
              val CONFLOW{control=gr, def=_, use=_, ismove=_, temps=_} = createGraph fragInstrs
              val reset = AssemNode.curID := 0
            in gr
            end
      in
        map processFragment program
      end

(*Returns a list of tuples
  Each tuple has form (conflow, instr list)*)
  fun main filename =
      let val program = MipsGen.transFrags filename

        fun processFragment fragInstrs=
          let
            val cflow = createGraph fragInstrs
            val reset = AssemNode.curID := 0
          in (cflow, fragInstrs)
          end
      in
      map processFragment program
      end


end
