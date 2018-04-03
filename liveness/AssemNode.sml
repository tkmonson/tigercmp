structure AssemNode : ORD_KEY =
struct

  datatype node = ASNODE of {ins : Assem.instr,
                                id  : int
                                }

 fun compare(ASNODE{ins1, id1}, ASNODE{ins2, id2}) = Int.compare(id1, id2)
 fun getID(ASNODE{ins, id}) = id

 val curID = ref 0

 val makeNode(ins:Assem.instr) =
  let val oldID = !curID
      val newID = curID := oldID+1
  in
    ASNODE{ins=ins, id=oldID}
  end

end
