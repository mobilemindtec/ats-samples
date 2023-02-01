//#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"

datatype bstree(a: t@ype) =
  | Empty(a) of ()
  | Node(a) of (bstree(a), a, bstree(a))

fun {a:t@ype} bstree_inord(t0 : bstree(a), fwork : a -<cloref1> void) :
  void =
  
    begin
      case+ t0 of
        | Empty () => ()
        | Node (t1, k, t2) => {
          val () = bstree_inord<a>(t1, fwork)
          val () = fwork(k)
          val () = bstree_inord<a>(t2, fwork)
        }
    end

fun {a:t@ype} bstree_search( t0 : bstree(a)
                           , k0 : a
                           , cmp : (a, a) -<cloref1> int
                           ) : bool =
  
    begin
      case+ t0 of
        | Empty () => false
        | Node (t1, k, t2) => let
          val sgn = cmp(k0, k)
        in
          case+ 0 of
            | _ when sgn < 0 => bstree_search<a>(t1, k0, cmp)
            | _ when sgn > 0 => bstree_search<a>(t2, k0, cmp)
            | _ => true
        end
    end

fun {a:t@ype} bstree_insert( t0 : bstree(a)
                           , k0 : a
                           , cmp : (a, a) -<cloref1> int
                           ) : bstree(a) =
  
    begin
      case+ t0 of
        | Empty () => Node(Empty, k0, Empty)
        | Node (t1, k, t2) => let
          val sgn = cmp(k0, k)
        in
          case+ 0 of
            | _ when sgn < 0 => Node(bstree_insert<a>(t1, k0, cmp), k, t2)
            | _ when sgn > 0 => Node(t1, k, bstree_insert<a>(t2, k0, cmp))
            | _ => t0
        end
    end

fun {a:t@ype} bstree_preorder( t0 : bstree(a)
                             , fwork : a -<cloref1> void
                             ) : void =
  
    begin
      case+ t0 of
        | Empty () => ()
        | Node (t1, k, t2) => {
          val () = bstree_preorder<a>(t1, fwork)
          val () = fwork(k)
          val () = bstree_preorder<a>(t2, fwork)
        }
    end

val cmp_str = lam (k0 : string, k1 : string) : int =<cloref> compare(k0 , k1)

val cmp_int = lam (k0 : int, k1 : int) : int =<cloref> k0 - k1

val () = {
  val bst = Empty()
  val bst = bstree_insert<string>(bst, "a", cmp_str)
  val bst = bstree_insert<string>(bst, "z", cmp_str)
  val bst = bstree_insert<string>(bst, "b", cmp_str)
  val bst = bstree_insert<string>(bst, "y", cmp_str)
  val bst = bstree_insert<string>(bst, "c", cmp_str)
  val bst = bstree_insert<string>(bst, "x", cmp_str)
  val bst = bstree_insert<string>(bst, "a", cmp_str)
  val bst = bstree_insert<string>(bst, "b", cmp_str)
  val bst = bstree_insert<string>(bst, "c", cmp_str)
  val bst = bstree_insert<string>(bst, "d", cmp_str)
  val () = assertloc(bstree_search<string>(bst, "y", cmp_str))
  val () = assertloc(not(bstree_search<string>(bst, "o", cmp_str)))
  val () = print("bst = ")
  val () = bstree_preorder<string>(bst, lam k => print(k))
  val () = print_newline()

  val bstint = Empty()
  val bstint = bstree_insert<int>(bstint, 8, cmp_int) 
  val bstint = bstree_insert<int>(bstint, 2, cmp_int) 
  val bstint = bstree_insert<int>(bstint, 5, cmp_int) 
  val bstint = bstree_insert<int>(bstint, 9, cmp_int) 
  val bstint = bstree_insert<int>(bstint, 1, cmp_int) 
  val bstint = bstree_insert<int>(bstint, 5, cmp_int) 
  val () = assertloc(bstree_search<int>(bstint, 2, cmp_int))
  val () = assertloc(not(bstree_search<int>(bstint, 7, cmp_int)))
  val () = print("bstint = ")
  val () = bstree_preorder<int>(bstint, lam k => print(k))
  val () = print_newline()
}

val _ = print("Hello, World!\n")

implement main0 () =
  ()
