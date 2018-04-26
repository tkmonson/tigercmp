(*
liveness - (list of (igraph, mgraph, instr), string frags)

regalloc - list of( map of temp to color(str), instr) - takes in a list of (igraph, mgraph, instr)

return instr by calling Assem.format on new table
*)
structure Main =
struct
fun main filename =
    let val (*igraph, mgraph, instr tuple list*)(liveList, stringfrags) = Liveness.main(filename)
        val fragInstrs = map RegAlloc.regAllocation liveList
        val out = TextIO.stdOut (*Could make this a file name in future*)
        val () = app (fn frag => TextIO.output(out, MipsFrame.string frag)) stringfrags
    in app (fn instr => TextIO.output(out, instr)) (List.concat(fragInstrs)) (*Turns whole program across fragments into list of strings*)
    end

end
