let assoc = [("one",1),("two",2),("three",3)];;

List.Assoc.find assoc "two";;
List.Assoc.add assoc "four" 4 (* add a new key *) ;;
List.Assoc.add assoc "two" 4 (* overwrite an existing key *) ;;
