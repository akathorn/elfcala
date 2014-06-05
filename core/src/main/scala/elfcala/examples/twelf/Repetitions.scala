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

  |{ eq_nat_rfl (?) ('EQ) ->: id2eq_nat (id_nat_rfl) ('EQ) }
  % { worlds (id2eq_nat) (?) (?) }
  % { total (A) (id2eq_nat) (A) (?) }

  val add_inc = |{ add (A) (B) (C) ->: add (A) (s (B)) (s (C)) ->: Type }
  % { mode (add_inc) ++('E1) --('E2) }

  |{ add_inc (add_z) (add_z) }
  |{ add_inc (A) (B) ->: add_inc (add_s (A)) (add_s (B)) }
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

    val sub_list     = |{ list ->: list ->: Type }
    val sub_list_rfl = |{ sub_list (L, L)  }
    val sub_list_ext = |{ sub_list (L1) (L2) ->:
                          sub_list (L1, cons(?, L2)) }
  }

  // Example of extending generics
  val genericListProofs = generic { t =>
    import genericList._

    val list_size_eq = |{ list_size(t) (L) (N1) ->:
                          list_size(t) (L) (N2) ->:
                          eq_nat (N1) (N2) ->: Type }
    % { mode (list_size_eq) ++(A) ++(B) --(C) }
    |{ list_size_eq (list_size_nil(t)) (list_size_nil(t)) (eq_nat_z) }
    |{ list_size_eq ('S1) ('S2) ('EQ) ->:
         list_size_eq (list_size_cons(t) ('S1)) (list_size_cons(t) ('S2)) (eq_nat_s ('EQ)) }
    % { worlds (list_size_eq) (?) (?) (?) }
    % { total (A) (list_size_eq) (A) (?) (?) }


    val list_size_id = |{ list_size(t) (L) (N1) ->:
                          list_size(t) (L) (N2) ->:
                          id_nat (N1) (N2) ->: Type }
    % { mode (list_size_id) ++(A) ++(B) --(C) }
    |{ eq2id_nat ('EQ) ('ID) ->:
       list_size_eq ('S1) ('S2) ('EQ) ->:
       list_size_id ('S1) ('S2) ('ID) }
    % { worlds (list_size_id) (?) (?) (?) }
    % { total (A) (list_size_id) (A) (?) (?) }
  }

  genericListProofs(nat)
  genericListProofs(exp)
}

trait RecursiveExtensions extends Generics {
  val G = 'G; val I0 = 'I0; val I = 'I; val Z = 'Z
  val S1 = 'S1; val S2 = 'S2;

  import genericList._

  // Note that we could have all generic definitions enclosed in the same
  // block, sharing the same parameter t. That would be more practical, but to
  // ilustrate how we can incrementaly extend generic definitions in this
  // example the definitions are split in smaller blocks.

  val lookUp = generic { t =>
    val lkp0      = |{ list(t) ->: nat ->: t ->: Type }
    val lkp0_hit  = |{ lkp0 (cons(t)(X)(G)) (z) (X) }
    val lkp0_miss = |{ lkp0 (G) (I0) (X) ->:
                       lkp0 (cons(t)(?)(G)) (s(I0)) (X) }

    val lkp     = |{ list(t) ->: nat ->: t ->: Type }
    val lkp_rev = |{ lkp0 (G) (I0) (X) ->:
                     add (s(I)) (I0) (N) ->: // I0=N-1-I
                     list_size(t) (G) (N) ->:
                     lkp (G) (I) (X) }
  }

  import lookUp._

  val listIncSize = generic { t =>
    val list_size_inc = |{ list_size(t) (L) (N) ->:
                         !!(Z, t)/ { list_size(t) (cons(t) (Z) (L)) (s (N)) ->:
                         Type } }

    % { mode (list_size_inc) ++(A) ++(Z) --(B) }
    |{ list_size_inc (list_size_nil(t)) (?) (list_size_cons(t) (list_size_nil(t))) }
    |{ list_size_inc (A) (?) (B) ->:
       list_size_inc (list_size_cons(t) (A)) (?) (list_size_cons(t) (B)) }
    % { worlds (list_size_inc) (?) (?) (?) }
    % { total (A) (list_size_inc) (A) (?) (?) }
  }

  import listIncSize._

  val extLookUp = generic { t =>
    val shift_lkp0 = |{ lkp0(t) (G) (I0) (X) ->:
                       !!(Z, t)/ { lkp0(t) (cons(t) (Z) (G)) (s (I0)) (X) ->:
                       Type }}

    % { mode (shift_lkp0) ++(A) ++(Z) --(B) }
    |{ shift_lkp0 (lkp0_hit(t)) (?) (lkp0_miss(t) (lkp0_hit(t))) }
    |{ shift_lkp0 (A) (?) (B) ->:
       shift_lkp0 (lkp0_miss(t) (A)) (?) (lkp0_miss(t) (B)) }
    % { worlds (shift_lkp0) (?) (?) (?) }
    % { total (A) (shift_lkp0) (A) (?) (?) }


    val ext_lkp = |{ lkp(t) (G) (I) (X) ->:
                    !!(Z, t)/ { lkp(t) (cons(t) (Z) (G)) (I) (X) ->: Type } }
    % { mode (ext_lkp) ++(A) ++(Z) --(B) }
    |{ shift_lkp0 (L1) (Z) (L2) ->:
       add_inc (A) (B) ->:
       list_size_inc(t) (S1) (Z) (S2) ->:
       ext_lkp (lkp_rev(t) (L1) (A) (S1)) (Z) (lkp_rev(t) (L2) (B) (S2)) }
    % { worlds (ext_lkp) (?) (?) (?) }
    % { total (A) (ext_lkp) (A) (?) (?) }
  }


  import extLookUp._

  val E1 = 'E1; val E2 = 'E2; val E3 = 'E3; val E4 = 'E4; val V = 'V

  val ev     = |{ list(nat) ->: exp ->: nat ->: Type }
  val ev_cst = |{ ev (G) (cst (N)) (N) }
  val ev_var = |{ lkp(nat) (G) (I) (N) ->: ev (G) (vaR (I)) (N) }
  val ev_pls = |{ add (N1) (N2) (N) ->:
                  ev (G) (E2) (N2) ->:
                  ev (G) (E1) (N1) ->:
                  ev (G) (pls (E1) (E2)) (N) }

  val ext_ev = |{ ev (G) (I) (V) ->:
                 !!(Z, nat)/ { ev (cons(nat) (Z) (G)) (I) (V) ->: Type } }
  % { mode (ext_ev) ++(A) ++(Z) --(B) }
  |{ ext_ev (ev_cst) (?) (ev_cst) }
  |{ ext_lkp(nat) (L1) (?) (L2) ->:
     ext_ev (ev_var (L1)) (?) (ev_var (L2)) }
  |{ ext_ev (E2) (?) (E4) ->:
     ext_ev (E1) (?) (E3) ->:
     ext_ev (ev_pls (A) (E2) (E1)) (?) (ev_pls (A) (E4) (E3)) }

  % { worlds (ext_ev) (?) (?) (?) }
  % { total (A) (ext_ev) (A) (?) (?) }


  val G2 = 'G2; val G1 = 'G1; val B0 = 'B0; val S = 'S
  val recursiveExtension = generic { (rel, ext, envtype) =>
    val exts = |{ rel (G1) (I) (X) ->: sub_list(envtype) (G1) (G2) ->:
                  rel (G2) (I) (X) ->: Type }
    % { mode (exts) ++(A) ++(S) --(B) }
    |{ exts (A) (sub_list_rfl(envtype)) (A) }
    |{ ext (B0) (?) (B) ->:
       exts (A) (S) (B0) ->:
       exts (A) (sub_list_ext(envtype) (S)) (B) }

    % { worlds (exts) (?) (?) (?) }
    % { total (S) (exts) (?) (S) (?) }
  }


  recursiveExtension(lkp(nat), ext_lkp(nat), nat)
  recursiveExtension(ev, ext_ev, nat)

  // Bonus example!
  // Prove it for environments containing lists of lists of expressions.
  // Why? Because we can, of course!
  recursiveExtension(lkp(list(list(exp))),
                     ext_lkp(list(list(exp))),
                     list(list(exp)))
}


trait Repetitions extends RecursiveExtensions
