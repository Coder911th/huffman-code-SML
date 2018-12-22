exception NotImplemented;

use "File_system.sml";
use "Tree.sml";
use "Encoding.sml";
use "Decoding.sml";

val (binary, frequency_table) = encode_file("hello");
val decoded = decode_file("hello");

(* Отладка
Control.Print.printDepth := 100;
val frequency_table = deserialize_frequency_table(read_file("data/" ^ "example" ^ ".frequencies.txt"));
val tree = get_tree(frequency_table) *)
