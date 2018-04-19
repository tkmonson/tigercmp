(* flow - return conflow and instr list tuple

Saumya is going to fix flow to return what we need for liveness

liveness - list of (igraph, mgraph, instr)

regalloc - list of( map of temp to color(str), instr) - takes in a list of (igraph, mgraph, instr)

return instr by calling Assem.format on new table

Need to make a call to liveness because it will call flow, then *)

fun main filename =
    let val (*igraph, mgraph, instr tuple list*)liveList = Liveness.main(filename)
        val fragInstrs = map regAllocation liveList
        val out = TextIO.stdOut (*Could make this a file name in future*)
    in map (fn instr => TextIO.output(out, instr)) (List.concat(fragInstrs)) (*Turns whole program across fragments into list of strings*)
    end
