
pub type Reg = u8;

#[derive(Clone, Copy, Debug)]
pub enum Instruction {
    NOp,
    Hlt,

    // Loads/Stores
    LdB { dst: Reg, ptr: Reg },
    LdW { dst: Reg, ptr: Reg },
    StB { src: Reg, ptr: Reg },
    StW { src: Reg, ptr: Reg },

    // Immediate loads
    LdI { dst: Reg, imm: u8 },
    LUI { dst: Reg, imm: u8 },

    // Arithmetic and logic
    Add { dst: Reg, a: Reg, b: Reg },
    Sub { dst: Reg, a: Reg, b: Reg },
    And { dst: Reg, a: Reg, b: Reg },
    Or  { dst: Reg, a: Reg, b: Reg },
    XOr { dst: Reg, a: Reg, b: Reg },
    Not { dst: Reg, src: Reg },
    Inc { dst: Reg, imm: u8 },
    Dec { dst: Reg, imm: u8 },

    // Shifts
    SL  { dst: Reg, src: Reg, amt: Reg },
    SLI { dst: Reg, src: Reg, amt: u8 },
    SR  { dst: Reg, src: Reg, amt: Reg },
    SRI { dst: Reg, src: Reg, amt: u8 },
    SA  { dst: Reg, src: Reg, amt: Reg },
    SAI { dst: Reg, src: Reg, amt: u8 },

    // Comparisons
    TEq { a: Reg, b: Reg },
    TNE { a: Reg, b: Reg },
    TGT { a: Reg, b: Reg },
    TLT { a: Reg, b: Reg },
    TGE { a: Reg, b: Reg },
    TLE { a: Reg, b: Reg },

    // Branches and jumps
    JR  {                     off: i8 },
    BR  { cnd: Reg,           off: i8 },
    J   {           ptr: Reg, off: u8 },
    B   { cnd: Reg, ptr: Reg, off: u8 },
}

//
// INSTRUCTION ENCODING
//
//   f e d c   b a 9 8   7 6 5 4   3 2 1 0
//
//   0 0 0 0   0 0 0 0   0 0 0 0   0 0 0 0  nop
//                                 [  ≠0 ]                  spare 14
//                                 1 1 1 1  hlt
//                       [ d≠0 ]   [ p   ]  ldb d, [p]
//             [ d≠0 ]   [ a   ]   [ b   ]  add d, a, b
//
//   0 0 0 1   0 0 0 0   0 0 0 0   * * * *                  spare ^4
//                       [ d≠0 ]   [ p   ]  ldw d, [p]
//             [ d≠0 ]   [ a   ]   [ b   ]  sub d, a, b
//
//   0 0 1 0   0 0 0 0   [ s   ]   [ p   ]  stb [p], s
//             [ d≠0 ]   [ a   ]   [ b   ]  and d, a, b
//
//   0 0 1 1   0 0 0 0   [ s   ]   [ p   ]  stw [p], s
//             [ d≠0 ]   [ a   ]   [ b   ]  or  d, a, b
//
//   0 1 0 0   0 0 0 0   0 0 0 0   * * * *                  spare ^4
//                       [ d≠0 ]   [ imm ]  inc d, $imm
//             [ d≠0 ]   [ a   ]   [ b   ]  xor d, a, b
//
//   0 1 0 1   0 0 0 0   0 0 0 0   * * * *                  spare ^4
//                       [ d≠0 ]   [ imm ]  dec d, $imm
//             [  ≠0 ]   * * * *   0 0 0 0                  spare ^4
//             [ d≠0 ]   [ s   ]   [ n≠0 ]  sl d, s, n
//
//   0 1 1 0   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   [ s   ]   0 0 0 0  not d, s
//             [ d≠0 ]   [ s   ]   [ n≠0 ]  sr d, s, n
//
//   0 1 1 1   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   * * * *   0 0 0 0                  spare ^4
//             [ d≠0 ]   [ s   ]   [ n≠0 ]  sa d, s, n
//
//   1 0 0 0   0 0 0 0   [ a   ]   [ b   ]  teq a, b
//             0 0 0 1   [ a   ]   [ b   ]  tne a, b
//             0 0 1 0   [ a   ]   [ b   ]  tgt a, b
//             0 0 1 1   [ a   ]   [ b   ]  tlt a, b
//             0 1 0 0   [ a   ]   [ b   ]  tge a, b
//             0 1 0 1   [ a   ]   [ b   ]  tle a, b
//             0 1 1 *   * * * *   * * * *                  spare ^9
//             1 * * *   * * * *   * * * *                  spare ^11
//
//   1 0 0 1   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   [ imm           ]  ldi d, $imm
//
//   1 0 1 0   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   [ imm           ]  lui d, $imm
//
//   1 0 1 1   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   [ s   ]   [ sh  ]  sli d, s, $sh
//
//   1 1 0 0   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   [ s   ]   [ sh  ]  sri d, s, $sh
//
//   1 1 0 1   0 0 0 0   * * * *   * * * *                  spare ^8
//             [ d≠0 ]   [ s   ]   [ sh  ]  sai d, s, $sh
//
//   1 1 1 0   0 0 0 0   [ off           ]  jr $off
//             [ c≠0 ]   [ off           ]  br c, $off
//
//   1 1 1 1   0 0 0 0   [  p  ]   [ off ]  j p+$off
//             [ c≠0 ]   [  p  ]   [ off ]  b c, p+$off
//
impl Instruction {
    pub fn decode(word: u16) -> Option<Instruction> {
        let op = word >> 12;
        let x = ((word >> 8) & 0xf) as u8;
        let y = ((word >> 4) & 0xf) as u8;
        let z = ((word >> 0) & 0xf) as u8;

        let lo8 = ((word >> 0) & 0xff) as u8;

        use Instruction as I;

        let inst = match (op, x, y, z) {
            (0x0, 0x0, 0x0, 0x0)                         => I::NOp,
            (0x0, 0x0, 0x0, 0xf)                         => I::Hlt,
            (0x0, 0x0, dst, ptr) if dst != 0             => I::LdB { dst, ptr },
            (0x0, dst, a,   b  ) if dst != 0             => I::Add { dst, a, b },

            (0x1, 0x0, dst, ptr) if dst != 0             => I::LdW { dst, ptr },
            (0x1, dst, a,   b  ) if dst != 0             => I::Sub { dst, a, b },

            (0x2, 0x0, src, ptr)                         => I::StB { src, ptr },
            (0x2, dst, a,   b  )                         => I::And { dst, a, b },

            (0x3, 0x0, src, ptr)                         => I::StW { src, ptr },
            (0x3, dst, a,   b  )                         => I::Or  { dst, a, b },

            (0x4, 0x0, dst, imm) if dst != 0             => I::Inc { dst, imm },
            (0x4, dst, a,   b  ) if dst != 0             => I::XOr { dst, a, b },

            (0x5, 0x0, dst, imm) if dst != 0             => I::Dec { dst, imm },
            (0x5, dst, src, amt) if dst != 0 && amt != 0 => I::SL  { dst, src, amt },

            (0x6, dst, src, 0x0) if dst != 0             => I::Not { dst, src },
            (0x6, dst, src, amt) if dst != 0 && amt != 0 => I::SR  { dst, src, amt },

            (0x7, dst, src, amt) if dst != 0 && amt != 0 => I::SA  { dst, src, amt },

            (0x8, 0x0, a,   b  )                         => I::TEq { a, b },
            (0x8, 0x1, a,   b  )                         => I::TNE { a, b },
            (0x8, 0x2, a,   b  )                         => I::TGT { a, b },
            (0x8, 0x3, a,   b  )                         => I::TLT { a, b },
            (0x8, 0x4, a,   b  )                         => I::TGE { a, b },
            (0x8, 0x5, a,   b  )                         => I::TLE { a, b },

            (0x9, dst, _,   _  ) if dst != 0             => I::LdI { dst, imm: lo8 },

            (0xa, dst, _,   _  ) if dst != 0             => I::LUI { dst, imm: lo8 },

            (0xb, dst, src, amt) if dst != 0             => I::SLI { dst, src, amt },

            (0xc, dst, src, amt) if dst != 0             => I::SRI { dst, src, amt },

            (0xd, dst, src, amt) if dst != 0             => I::SAI { dst, src, amt },

            (0xe, 0x0, _,   _  )                         => I::JR  { off: lo8 as i8 },
            (0xe, cnd, _,   _  ) if cnd != 0             => I::BR  { cnd, off: lo8 as i8 },

            (0xf, 0x0, ptr, off)                         => I::J   { ptr, off },
            (0xf, cnd, ptr, off) if cnd != 0             => I::B   { cnd, ptr, off },

            _ => { return None; }
        };

        Some(inst)
    }

    pub fn encode(self) -> Option<u16> {
        enum Mk { N(u8, u8, u8, u8), I(u8, u8, u8) };

        use Instruction as I;

        let make = match self {
             I::NOp                                           => Mk::N(0x0, 0x0, 0x0, 0x0),
             I::Hlt                                           => Mk::N(0x0, 0x0, 0x0, 0xf),
             I::LdB { dst, ptr }      if dst != 0             => Mk::N(0x0, 0x0, dst, ptr),
             I::Add { dst, a, b }     if dst != 0             => Mk::N(0x0, dst, a,   b  ),

             I::LdW { dst, ptr }      if dst != 0             => Mk::N(0x1, 0x0, dst, ptr),
             I::Sub { dst, a, b }     if dst != 0             => Mk::N(0x1, dst, a,   b  ),

             I::StB { src, ptr }                              => Mk::N(0x2, 0x0, src, ptr),
             I::And { dst, a, b }                             => Mk::N(0x2, dst, a,   b  ),

             I::StW { src, ptr }                              => Mk::N(0x3, 0x0, src, ptr),
             I::Or  { dst, a, b }                             => Mk::N(0x3, dst, a,   b  ),

             I::Inc { dst, imm }      if dst != 0             => Mk::N(0x4, 0x0, dst, imm),
             I::XOr { dst, a, b }     if dst != 0             => Mk::N(0x4, dst, a,   b  ),

             I::Dec { dst, imm }      if dst != 0             => Mk::N(0x5, 0x0, dst, imm),
             I::SL  { dst, src, amt } if dst != 0 && amt != 0 => Mk::N(0x5, dst, src, amt),

             I::Not { dst, src }      if dst != 0             => Mk::N(0x6, dst, src, 0x0),
             I::SR  { dst, src, amt } if dst != 0 && amt != 0 => Mk::N(0x6, dst, src, amt),

             I::SA  { dst, src, amt } if dst != 0 && amt != 0 => Mk::N(0x7, dst, src, amt),

             I::TEq { a, b }                                  => Mk::N(0x8, 0x0, a,   b  ),
             I::TNE { a, b }                                  => Mk::N(0x8, 0x1, a,   b  ),
             I::TGT { a, b }                                  => Mk::N(0x8, 0x2, a,   b  ),
             I::TLT { a, b }                                  => Mk::N(0x8, 0x3, a,   b  ),
             I::TGE { a, b }                                  => Mk::N(0x8, 0x4, a,   b  ),
             I::TLE { a, b }                                  => Mk::N(0x8, 0x5, a,   b  ),

             I::LdI { dst, imm      } if dst != 0             => Mk::I(0x9, dst, imm     ),

             I::LUI { dst, imm      } if dst != 0             => Mk::I(0xa, dst, imm     ),

             I::SLI { dst, src, amt } if dst != 0             => Mk::N(0xb, dst, src, amt),

             I::SRI { dst, src, amt } if dst != 0             => Mk::N(0xc, dst, src, amt),

             I::SAI { dst, src, amt } if dst != 0             => Mk::N(0xd, dst, src, amt),

             I::JR  { off      }                              => Mk::I(0xe, 0x0, off as u8),
             I::BR  { cnd, off      } if cnd != 0             => Mk::I(0xe, cnd, off as u8),

             I::J   { ptr, off }                              => Mk::N(0xf, 0x0, ptr, off),
             I::B   { cnd, ptr, off } if cnd != 0             => Mk::N(0xf, cnd, ptr, off),

             _ => { return None; }
        };

        let word = match make {
            Mk::N(op, x, y, z) if op < 16 && x < 16 && y < 16 && z < 16 =>
                ((op as u16) << 12) | ((x as u16) << 8) | ((y as u16) << 4) | ((z as u16) << 0),
            Mk::I(op, x, imm) if op < 16 && x < 16 =>
                ((op as u16) << 12) | ((x as u16) << 8) | (imm as u16),
            _ => { return None; }
        };

        Some(word)
    }
}

