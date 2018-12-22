(*
    Бинарное дерево

    Два типа вершин:
    - листья - пара (встречаемость в тексте, символ)
    - фиктивные узлы - содержат суммарную встречаемость своих двух поддеревьев (letf & right)
*)
datatype Tree = Node of Tree * int * Tree | Leaf of int * char;

(* Возвращает частоту встречаемости для текущего листа/узла *)
fun get_subtree_frequency(Leaf(frequency, _)) = frequency
|   get_subtree_frequency(Node(_, frequency, _)) = frequency;

(*
    Вставляет дерево в список, который должен быть упорядочен
    по возрастанию встречаемости символов в дереве.
    
    Сумма встречаемости символов в дереве равна сумме
    встречаемости символов в каждой из его веток
*)
fun insert_tree(_, tree, []) = [tree]
|   insert_tree(frequency, new_tree, tree::others) =
        if frequency < get_subtree_frequency(tree)
            then new_tree::tree::others
            else tree::insert_tree(frequency, new_tree, others);

(*
    Преобразование таблицы [(символ, частота встречаемости)] в дерево Хаффмана
    Таблица должна быть отсортирована по возрастанию встречаемости символов

    1) Превращаем кажду строку таблицы в лист дерева, получаем список листьев отсортированных
    по частоте встречаемости
    2) Извлекаем первые два элемента из списка (с наименьшей частотой) и объединяем их фиктивным узлом.
    Частота этого фиктивного узла равна сумме частот листьев.
    3) Вставляем этот фиктивный узел в список сохраняя условие - все деревья списка должны быть отсортированы
    по возрастанию частоты
    4) Повторяем с шага 2, пока в списке не останется всего одно дерево, оно и будет результатом
*)
exception EmptyTableException;
fun get_tree(table) = let
    fun map(func, nil) = nil
    |   map(func, head::tail) = func(head)::map(func, tail)
    fun get_tree'(nil) = raise EmptyTableException
    |   get_tree'([t]) = t
    |   get_tree'(first_tree::second_tree::list_tail) =
            let
                val sum_frequency = get_subtree_frequency(first_tree) + get_subtree_frequency(second_tree)
            in
                get_tree'(insert_tree(sum_frequency, Node(first_tree, sum_frequency, second_tree), list_tail))
            end
in
    get_tree'(map(fn(char, frequency) => Leaf(frequency, char), table))
end;