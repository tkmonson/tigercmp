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
        val file = String.substring(filename, 0, String.size(filename)-4)
        (* val out = TextIO.stdOut (*Could make this a file name in future*) *)
        val out = TextIO.openOut (file ^ ".s")
        val () = TextIO.output(out, ".data\n")
        val () = TextIO.output(out, ".align 2\n")
        val () = app (fn frag => TextIO.output(out, MipsFrame.string frag)) stringfrags
        (*Writes rest of MIPS body*)
        val () = TextIO.output(out, ".text\n")
        val prog = app (fn instr => TextIO.output(out, instr)) (List.concat(fragInstrs)) (*Turns whole program across fragments into list of strings*)
    in
        (TextIO.closeOut out)
    end

end
