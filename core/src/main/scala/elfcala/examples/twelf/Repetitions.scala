package elfcala.examples.twelf

import elfcala.twelf._
import elfcala.LogicalFramework._
import elfcala.LogicalFramework.Kind.Type

// Pretty printing is use to name generic types
import elfcala.PrettyPrinter

trait Preliminaries extends TwelfSignature {
  // Natural numbers

  // This is convenient to save typestrokes
  val N = 'N; val N1 = 'N1; val N2 = 'N2; val N3 = 'N3;
  val X = 'X; val A = 'A; val B = 'B; val C = 'C;

  val nat = |{ Type }
  val z   = |{ nat }
  val s   = |{ nat ->: nat }

  val add   = |{ nat ->: nat ->: nat ->: Type }
  val add_z = |{ add(z)(N)(N) }
  val add_s = |{ add(N1)(N2)(N3) ->: add(s(N1))(N2)(s(N3)) }

  val lte   = |{ nat ->: nat ->: Type }
  val lte_z = |{ lte (z) (N) }
  val lte_s = |{ lte (N1) (N2) ->: lte (s(N1)) (s(N2)) }

  val id_nat     = |{ nat ->: nat ->: Type }
  val id_nat_rfl = |{ id_nat (X) (X) }

  val eq_nat   = |{ nat ->: nat ->: Type }
  val eq_nat_z = |{ eq_nat (z) (z) }
  val eq_nat_s = |{ eq_nat (N1) (N2) ->: eq_nat (s (N1)) (s (N2)) }

  val s_injective = |{ id_nat (N1) (N2) ->: id_nat (s (N1)) (s (N2)) ->: Type }
  % { mode (s_injective) ++(A) --(B) }

  val s_injective_rfl = |{ s_injective (id_nat_rfl) (id_nat_rfl) }
  % { worlds (s_injective) (?) (?) }
  % { total (A) (s_injective) (A) (?) }

  val eq2id_nat = |{ eq_nat (N1) (N2) ->: id_nat (N1) (N2) ->: Type }
  % { mode (eq2id_nat) ++(A) --(B) }
  val eq2id_nat_z = |{ eq2id_nat (eq_nat_z) (id_nat_rfl) }
  val eq2id_nat_s = |{ s_injective (B) ('OUT) ->:
                                    eq2id_nat (A) (B) ->:
                                    eq2id_nat (eq_nat_s (A)) ('OUT) }
  % { worlds (eq2id_nat) (?) (?) }
  % { total (A) (eq2id_nat) (A) (?) }

  val eq_nat_rfl = |{ !!(N, nat)/ { eq_nat (N) (N) } ->: Type }
  % { mode (eq_nat_rfl) ++(N) --('EQ) }
  val eq_nat_rfl_z = |{ eq_nat_rfl (z) (eq_nat_z) }
  val eq_nat_rfl_s = |{ eq_nat_rfl (N) ('EQ) ->:
                                      eq_nat_rfl (s (N)) (eq_nat_s ('EQ)) }
  % { worlds (eq_nat_rfl) (?) (?) }
  % { total (N) (eq_nat_rfl) (N) (?) }

  val id2eq_nat = |{ id_nat (N1) (N2) ->: eq_nat (N1) (N2) ->: Type }
  % { mode (id2eq_nat) ++(A) --(B) }

  val _1 = |{ eq_nat_rfl (?) ('EQ) ->: id2eq_nat (id_nat_rfl) ('EQ) }
  % { worlds (id2eq_nat) (?) (?) }
  % { total (A) (id2eq_nat) (A) (?) }

  val add_inc = |{ add (A) (B) (C) ->: add (A) (s (B)) (s (C)) ->: Type }
  % { mode (add_inc) ++('E1) --('E2) }

  val _2 = |{ add_inc (add_z) (add_z) }
  val _3 = |{ add_inc (A) (B) ->: add_inc (add_s (A)) (add_s (B)) }
  % { worlds (add_inc) (?) (?) }
  % { total (A) (add_inc) (A) (?) }

  // Expressions
  var exp = |{ Type }
  var cst = |{ nat ->: exp }
  var vaR = |{ nat ->: exp }
  var pls = |{ exp ->: exp ->: exp }
}


trait Generics extends Preliminaries {
  val L = 'L; val L1 = 'L1; val L2 = 'L2

  // Generic list definition
  val genericList = generic { t =>
    val list = |{ Type }
    val nil  = |{ list }
    val cons = |{ t ->: list ->: list }

    val list_size      = |{ list ->: nat ->: Type }
    val list_size_nil  = |{ list_size (nil, z)  }
    val list_size_cons = |{ list_size (L) (N) ->:
                            list_size (cons (?, L), s(N)) }

    val sub_list      = |{ list ->: list ->: Type }
    val sub_list_nil  = |{ sub_list (L, L)  }
    val sub_list_cons = |{ sub_list (L1) (L2) ->:
                           sub_list (L1, cons(?, L2)) }
  }

  // Example of extending generics
  val genericListProofs = generic { t =>
    import genericList._

    val list_size_eq = |{ list_size(t) (L) (N1) ->:
                          list_size(t) (L) (N2) ->:
                          eq_nat (N1) (N2) ->: Type }
    % { mode (list_size_eq) ++(A) ++(B) --(C) }
    val _1 = |{ list_size_eq (list_size_nil(t)) (list_size_nil(t)) (eq_nat_z) }
    val _2 = |{ list_size_eq ('S1) ('S2) ('EQ) ->:
         list_size_eq (list_size_cons(t) ('S1)) (list_size_cons(t) ('S2)) (eq_nat_s ('EQ)) }
    % { worlds (list_size_eq) (?) (?) (?) }
    % { total (A) (list_size_eq) (A) (?) (?) }


    val list_size_id = |{ list_size(t) (L) (N1) ->:
                          list_size(t) (L) (N2) ->:
                          id_nat (N1) (N2) ->: Type }
    % { mode (list_size_id) ++(A) ++(B) --(C) }
    val _3 = |{ eq2id_nat ('EQ) ('ID) ->:
                list_size_eq ('S1) ('S2) ('EQ) ->:
                list_size_id ('S1) ('S2) ('ID) }
    % { worlds (list_size_id) (?) (?) (?) }
    % { total (A) (list_size_id) (A) (?) (?) }
  }

  genericListProofs.list_size_id(nat)
  genericListProofs.list_size_id(exp)
}

trait RecursiveExtensions extends Generics {
  // The generics trait has already generated the declarations for nat-list,
  // but they are not bound to any scala variable. We can bind the names
  // manually:
  // val nat_list_size      = Symbol("nat-list-size")
  // val nat_list_size_nil  = Symbol("nat-list-size/nil" )
  // val nat_list_size_cons = Symbol("nat-list-size/cons")
  // val sub_nat_list       = Symbol("sub-nat-list")
  // val sub_nat_list_rfl   = Symbol("sub-nat-list/rfl" )
  // val sub_nat_list_ext   = Symbol("sub-nat-list/ext")
  // val nat_list_size_eq   = Symbol("nat-list-size-eq")
  // val nat_list_size_id   = Symbol("nat-list-size-id")




}


trait Repetitions extends RecursiveExtensions {
}
