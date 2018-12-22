(* Возвращает талицу частот символ => частота *)
fun get_frequency_table(text) = let
    fun increment(nil, target_char) = [(target_char, 1)]
    |   increment((char, frequency)::tail, target_char) = 
        if target_char = char
            then (char, frequency + 1)::tail
            else (char, frequency)::increment(tail, target_char)
    fun get_table(table, nil) = table
    |   get_table(table, char::tail) = get_table(increment(table, char), tail)
    fun reverse(nil) = nil
    |   reverse(head::tail) = reverse(tail) @ [head]
in
    reverse(get_table([], explode(text)))
end;

(* Строит по дереву таблицу символ => код *)
fun build_code_table(root) = let
    fun insert_in_table(table, char, code) = table @ [(char, code)]
    fun build_table(table, code, (Leaf(_, char))) = insert_in_table(table, char, code)
    |   build_table(table, code, (Node(left, _, right))) =
            let val table_from_left_subtree = build_table(table, code ^ "0", left)
            in
                build_table(table_from_left_subtree, code ^ "1", right)
            end
in
    build_table([], "", root)
end;

(* Кодирует текст с помошью таблица кодов *)
exception UnknownCharacterFound
fun encode_using_code_table(text, code_table) = let
    fun get_code(nil, target_char) = raise UnknownCharacterFound
    |   get_code((char, code)::tail, target_char) =
            if char = target_char
                then code
                else get_code(tail, target_char)
    fun encode(nil) = ""
    |   encode(char::tail) = get_code(code_table, char) ^ encode(tail)
in
    encode(explode(text))
end;

(*
    Превращает таблицу частот символов в строку
    ~ - разделитель между парами символ-частота
*)
fun serialize_frequency_table(nil) = ""
|   serialize_frequency_table((char, frequency)::tail) = let
        val head_str = implode([char]) ^ Int.toString(frequency)
        val tail_str = serialize_frequency_table(tail)
    in
        if tail_str = ""
            then head_str
            else head_str ^ "~" ^ tail_str
    end;

(* Кодирует содержимое файла по имени (без расширения из папки data) *)
fun encode_file(filename) = let
    val source_text = read_file("data/" ^ filename ^ ".txt")
    val frequency_table = get_frequency_table(source_text)
    val code_table = build_code_table(get_tree(frequency_table))
    val binary = encode_using_code_table(source_text, code_table)
    (* Сохраняем полученный код-Хаффана *)
    val _ = write_file("data/" ^ filename ^ ".binary.txt", binary)
    (* Сохраняем таблицу частот для декодирования *)
    val _ = write_file("data/" ^ filename ^ ".frequencies.txt", serialize_frequency_table(frequency_table))
in
    (binary, frequency_table)
end;
