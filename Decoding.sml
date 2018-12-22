(*
    Как только нашли лист, возвращаем найденные в нём значения
    и оставшуюся закодированную последовательность бит
    
    rest_bin_code - оставщаяся строка бит, которую нужно раскодировать
    values - набор (frequency, char) - очередной расшифрованный символ и частота его встречаемости
*)
exception UnexpectedBinaryCode;
fun decode_char(rest_bin_code, Leaf(_, char)) = (char, rest_bin_code)
|   decode_char(#"0"::rest_bin_code, Node(left, _, _)) = decode_char(rest_bin_code, left)
|   decode_char(#"1"::rest_bin_code, Node(_, _, right)) = decode_char(rest_bin_code, right)
|   decode_char(_, _) = raise UnexpectedBinaryCode;

(* Декодирует список из нулей и единиц bin_code по дереву с корнем root *)
fun decode_binary_list([], _) = []
|   decode_binary_list(bin_code, root) = let
    val (char, rest_bin_code) = decode_char(bin_code, root)
    val rest_string = decode_binary_list(rest_bin_code, root)
in
    char::rest_string
end;

(* Декодирут строку из нулей и единиц bin_string по дереву с корнем root *)
fun decode_binary(bin_string, root) = implode(decode_binary_list(explode(bin_string), root));

(* Превращает текст файла в таблицу частот символов *)
fun deserialize_frequency_table(text) = let
    fun get_int(opt: 'a option) : 'a =
        case opt of
            NONE => raise Fail(text)
        | SOME a => a
    fun deserialize(table, nil, char, frequency) = (char, get_int(Int.fromString(frequency)))::table
    |   deserialize(table, current_char::tail, char, frequency) =
        if char = #"~"
            then deserialize(table, tail, current_char, "")
            else if current_char = #"~"
                then deserialize((char, get_int(Int.fromString(frequency)))::table, tail, #"~", "")
                else deserialize(table, tail, char, frequency ^ implode([current_char]))
    fun reverse(nil) = nil
    |   reverse(head::tail) = reverse(tail) @ [head]
in
    reverse(deserialize([], explode(text), #"~", ""))
end;

(* Декодирует файл *)
fun decode_file(filename) = let
    val binary = read_file("data/" ^ filename ^ ".binary.txt")
    val frequency_table = deserialize_frequency_table(read_file("data/" ^ filename ^ ".frequencies.txt"))
    val tree = get_tree(frequency_table)
    val decoded = decode_binary(binary, tree);
    val _ = write_file("data/" ^ filename ^ ".decoded.txt", decoded)
in
    decoded
end;
