structure AssemNode =
struct

  datatype node = ASNODE of {ins : Assem.instr,
                                id  : int
                                }

 fun compare(ASNODE{ins=ins1, id=id1}, ASNODE{ins=ins2, id=id2}) = Int.compare(id1, id2)
 fun getID(ASNODE{ins, id}) = id

 val curID = ref 0

 fun makeNode(ins:Assem.instr) =
  let val oldID = !curID
      val newID = curID := oldID+1
  in
    ASNODE{ins=ins, id=oldID}
  end

  val dummy = ASNODE{ins=Assem.LABEL{assem="dummmy", lab=Temp.newlabel()}, id=(~1)}

  fun printNode(id, node as ASNODE{ins=ins, id=_}) = "NODE " ^ Int.toString id ^ " "
                                                      ^ Assem.format(MipsGen.printTemp) ins

end
