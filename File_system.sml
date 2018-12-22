(* Прочитать файл *)
fun read_file(filename) = let
    val stream = TextIO.openIn(filename)
    val text = TextIO.inputAll(stream)
    val _ = TextIO.closeIn(stream)
in
    text
end;

(* Записать в файл *)
fun write_file(filename, text) = let
    val stream = TextIO.openOut(filename)
    val _ = TextIO.output(stream, text)
    val _ = TextIO.closeOut(stream)
in
    text
end;
