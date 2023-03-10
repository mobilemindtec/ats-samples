//#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

datatype bstree =
  | E of ()
  | B of (bstree, string, bstree)

fun bstree_inord(t0 : bstree, fwork : string -<cloref1> void) : void =
  
    begin
      case+ t0 of
        | E () => ()
        | B (t1, k, t2) => {
          val () = bstree_inord(t1, fwork)
          val () = fwork(k)
          val () = bstree_inord(t2, fwork)
        }
    end

fun bstree_search(t0 : bstree, k0 : string) : bool =
  
    begin
      case+ t0 of
        | E () => false
        | B (t1, k, t2) => let
          val sgn = compare(k0, k)
        in
          case+ 0 of
            | _ when sgn < 0 => bstree_search(t1, k0)
            | _ when sgn > 0 => bstree_search(t2, k0)
            | _ => true
        end
    end

fun bstree_insert(t0 : bstree, k0 : string) : bstree =
  
    begin
      case+ t0 of
        | E () => B(E, k0, E)
        | B (t1, k, t2) => let
          val sgn = compare(k0, k)
        in
          case+ 0 of
            | _ when sgn < 0 => B(bstree_insert(t1, k0), k, t2)
            | _ when sgn > 0 => B(t1, k, bstree_insert(t2, k0))
            | _ => t0
        end
    end

fun bstree_preorder(t0 : bstree, fwork : string -<cloref1> void) :
  void =
  
    begin
      case+ t0 of
        | E () => ()
        | B (t1, k, t2) => {
          val () = bstree_preorder(t1, fwork)
          val () = fwork(k)
          val () = bstree_preorder(t2, fwork)
        }
    end

val () = {
  val bst = E()
  val bst = bstree_insert(bst, "a")
  val bst = bstree_insert(bst, "z")
  val bst = bstree_insert(bst, "b")
  val bst = bstree_insert(bst, "y")
  val bst = bstree_insert(bst, "c")
  val bst = bstree_insert(bst, "x")
  val bst = bstree_insert(bst, "a")
  val bst = bstree_insert(bst, "b")
  val bst = bstree_insert(bst, "c")
  val bst = bstree_insert(bst, "d")
  val () = assertloc(bstree_search(bst, "y"))
  val () = assertloc(not(bstree_search(bst, "o")))
  val () = print("bst = ")
  val () = bstree_preorder(bst, lam k => print(k))
  val () = print_newline()
}
val _ = print("Hello, World!\n")

implement main0 () =
  ()
