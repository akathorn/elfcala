package elfcala

trait Naturals extends LogicalFramework {
  val nat    = typ                                               -- "nat"
  val z      = nat                                               -- "z"
  val s      = nat *=>: nat                                      -- "s"

  val plus   = nat *=>: nat *=>: nat *=>: typ                    -- "plus"
  val plus_z = nat { n => plus(z)(n)(n) }                        -- "plus_z"
  val plus_s = nat { n1 => nat { n2 => nat { n3 =>
    plus(n1)(n2)(n3) *=>:
    plus(s(n1))(n2)(s(n3))
  }}}                                                            -- "plus_s"

}

trait Even extends Naturals {
  val even   = nat *=>: typ                                      -- "even"
  val even_z = even(z)                                           -- "even_z"
  val even_s = nat { n =>
    even (n) *=>: even (s(s(n)))
  }                                                              -- "even_s"
}


// Some proofs
trait PlusZRightNeutral extends Naturals {
  val plus_z_right_neutral = nat { n => plus(n)(z)(n) *=>: typ } -- "plus_z_right_neutral"

  val pzrn_z = plus_z_right_neutral(z)(plus_z(z))                -- "_z"
  val pzrn_s = nat { n => closeAppConst(plus(n)(z)(n)) { d =>
    plus_z_right_neutral (n) (d) *=>:
    plus_z_right_neutral (s(n)) (plus_s (n)(z)(n) (d))
  }}                                                             -- "_s"
}

trait PlusSRightInc extends Naturals {
  val plus_s_right_inc = nat { n1 => nat { n2 => nat { n3 =>
    plus(n1)(n2)(n3) *=>: plus(n1)(s(n2))(s(n3)) *=>: typ
  }}}                                                            -- "plus_s_right_inc"

  val psri_z = nat { n =>
    plus_s_right_inc(z)(n)(n) (plus_z(n)) (plus_z(s(n)))
  }                                                              -- "_z"

  val psri_s = nat { n1 => nat { n2 => nat { n3 =>
                closeAppConst(plus(n1)(n2)(n3)) { d1 =>
                  closeAppConst(plus(n1)(s(n2))(s(n3))) { d2 =>
                    plus_s_right_inc (n1)(n2)(n3) (d1) (d2) *=>:
                    plus_s_right_inc (n1)(s(n2))(s(n3)) (plus_s(n1)(n2)(n3) (d1)) (plus_s (n1)(s(n2))(s(n3)) (d2))
  }}}}}                                                          -- "_s"
}

trait PlusComm extends Naturals with PlusZRightNeutral with PlusSRightInc {
  val plus_comm = nat { n1 => nat { n2 => nat { n3 =>
    plus(n1)(n2)(n3) *=>: plus(n2)(n1)(s(n3)) *=>: typ
  }}}                                                            -- "plus_comm"

  val pc_z = nat { n =>
    closeAppConst(plus(n)(z)(n)) { d =>
      plus_z_right_neutral (n) (d) *=>:
      plus_comm (z)(n)(n) (plus_z(n)) (d)
  }}                                                             -- "_z"

  val pc_s = nat { n1 => nat { n2 => nat { n3 =>
    closeAppConst(plus(n1)(n2)(n3)) { d1 =>
      closeAppConst(plus(n2)(n1)(n3)) { d2 =>
        closeAppConst(plus(n2)(s(n1))(s(n3))) { d3 =>
          plus_s_right_inc (n2)(n1)(n3) (d2) (d3) *=>:
          plus_comm (n1)(n2)(n3) (d1) (d2) *=>:
          plus_comm (s(n1))(n2)(s(n3)) (plus_s (n1)(n2)(n3) (d1)) (d3)
  }}}}}}                                                         -- "_s"
}

trait StructEq extends Naturals {
  val nat_eq_struct = nat *=>: nat *=>: typ                             -- "nat_eq_struct"

  val nat_eq_struct_z = nat_eq_struct (z)(z)                       -- "nat_eq_struct_z"
  val nat_eq_struct_s = nat { n1 => nat { n2 =>
    nat_eq_struct (n1)(n2) *=>:
    nat_eq_struct (s(n1))(s(n2))
  }}                       -- "nat_eq_struct_s"
}

trait Eq extends Naturals {
  val nat_eq = nat *=>: nat *=>: typ                             -- "nat_eq"

  val nat_eq_id = nat { n => nat_eq (n)(n) }                       -- "nat_eq_id"
}


trait PlusUnique extends Naturals with Eq {
  val nat_eq_s = nat { n1 => nat { n2 =>
    nat_eq (n1) (n2) *=>: nat_eq (s(n1))(s(n2)) *=>: typ
  }}                             -- "nat_eq_s"

  val nes_id = nat { n =>
    nat_eq_s (n)(n) (nat_eq_id (n)) (nat_eq_id (s(n)))
  }                             -- "nes_id"

  val plus_unique = nat { n1 => nat { n2 => nat { n3 => nat { n4 =>
    plus (n1)(n2)(n3) *=>: plus(n1)(n2)(n4) *=>: nat_eq(n3)(n4) *=>: typ
  }}}}                             -- "plus_unique"

  val pu_z = nat { n1 => nat { n2 =>
    plus_unique (z)(n1)(n1)(n2) (plus_z (n1)) (plus_z (n2)) (nat_eq_id (n2))
  }}    -- "pu_z"

  val pu_s = nat { n1 => nat { n2 => nat { n3 => nat { n4 =>
    closeAppConst(nat_eq(n1)(n2)) { eq1 =>
      closeAppConst(nat_eq(s(n1))(s(n2))) { eq2 =>
        closeAppConst(plus(n1)(n2)(n3)) { d1 =>
          closeAppConst(plus(n1)(n2)(n4)) { d2 =>
            nat_eq_s (n3)(n4) (eq1) (eq2) *=>:
            plus_unique (n1)(n2)(n3)(n4) (d1) (d2) (eq1) *=>:
            plus_unique (s(n1))(n2)(s(n3))(s(n4)) (plus_s (n1)(n2)(n3) (d1)) (plus_s (n1)(n2)(n4) (d2)) (eq2)
  }}}}}}}}    -- "pu_s"
}


