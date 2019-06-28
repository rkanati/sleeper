
use {
    crate::{
        instruction::{self, Reg},
    },
    std::{
        cmp::{min, max},
        collections::HashMap,
        iter::repeat,
        slice,
    },
};

type SymTab = HashMap<&'static str, i32>;

#[derive(Clone, Copy)]
pub enum Imm {
    Lit(i32),
    Ref(&'static str),
    HiB(&'static str),
    LoB(&'static str),
}

impl Imm {
    fn resolve(self, symtab: &SymTab) -> Option<i32> {
        let check = |x: &i32| *x >= -32768 && *x <= 65536;

        match self {
            Imm::Lit(value) => Some(value),
            Imm::Ref(name)  => symtab.get(name).cloned(),
            Imm::HiB(name)  => symtab.get(name).cloned().filter(check).map(|x| (x >> 8) & 0xff),
            Imm::LoB(name)  => symtab.get(name).cloned().filter(check).map(|x| (x >> 0) & 0xff),
        }
    }

    fn resolve_rel8(self, symtab: &SymTab, pc: u32) -> Option<i8> {
        let abs = self.resolve(symtab)?;
        let value = (abs - pc as i32);

        if value > (i8::max_value() as i32) || value < (i8::min_value() as i32) || value & 1 != 0 {
            None
        }
        else {
            Some((value >> 1) as i8)
        }
    }

    fn resolve_u4(self, symtab: &SymTab) -> Option<u8> {
        let value = self.resolve(symtab)?;

        if value > 15 || value < 0 {
            None
        }
        else {
            Some(value as u8)
        }
    }

    fn resolve_byte(self, symtab: &SymTab) -> Option<u8> {
        let value = self.resolve(symtab)?;

        if value > (u8::max_value() as i32) || value < (i8::min_value() as i32) {
            None
        }
        else {
            Some(value as u8)
        }
    }
}

#[derive(Clone, Copy)]
pub enum Inst {
    NOp,
    Hlt,
    LdB(Reg, Reg),
    LdW(Reg, Reg),
    StB(Reg, Reg),
    StW(Reg, Reg),
    LdI(Reg, Imm),
    LUI(Reg, Imm),
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
     Or(Reg, Reg, Reg),
    XOr(Reg, Reg, Reg),
    Not(Reg, Reg),
    Inc(Reg, Imm),
    Dec(Reg, Imm),
     SL(Reg, Reg, Reg),
     SR(Reg, Reg, Reg),
     SA(Reg, Reg, Reg),
    SLI(Reg, Reg, Imm),
    SRI(Reg, Reg, Imm),
    SAI(Reg, Reg, Imm),
    TEq(Reg, Reg),
    TNE(Reg, Reg),
    TGT(Reg, Reg),
    TLT(Reg, Reg),
    TGE(Reg, Reg),
    TLE(Reg, Reg),
     JR(Imm),
     BR(Reg, Imm),
      J(Reg, Imm),
      B(Reg, Reg, Imm)
}

impl Inst {
    fn assemble(self, symtab: &SymTab, pc: u32) -> Option<u16> {
        use instruction::Instruction as O;

        let inst = match self {
            Inst::NOp                => O::NOp,
            Inst::Hlt                => O::Hlt,
            Inst::LdB(dst, ptr)      => O::LdB { dst, ptr },
            Inst::LdW(dst, ptr)      => O::LdW { dst, ptr },
            Inst::StB(src, ptr)      => O::StB { src, ptr },
            Inst::StW(src, ptr)      => O::StW { src, ptr },
            Inst::LdI(dst, imm)      => O::LdI { dst, imm: imm.resolve_byte(symtab)? },
            Inst::LUI(dst, imm)      => O::LUI { dst, imm: imm.resolve_byte(symtab)? },
            Inst::Add(dst, a, b)     => O::Add { dst, a, b },
            Inst::Sub(dst, a, b)     => O::Sub { dst, a, b },
            Inst::And(dst, a, b)     => O::And { dst, a, b },
            Inst:: Or(dst, a, b)     => O::Or  { dst, a, b },
            Inst::XOr(dst, a, b)     => O::XOr { dst, a, b },
            Inst::Not(dst, src)      => O::Not { dst, src },
            Inst::Inc(dst, imm)      => O::Inc { dst, imm: imm.resolve_u4(symtab)? },
            Inst::Dec(dst, imm)      => O::Dec { dst, imm: imm.resolve_u4(symtab)? },
            Inst:: SL(dst, src, amt) => O::SL  { dst, src, amt },
            Inst:: SR(dst, src, amt) => O::SR  { dst, src, amt },
            Inst:: SA(dst, src, amt) => O::SA  { dst, src, amt },
            Inst::SLI(dst, src, amt) => O::SLI { dst, src, amt: amt.resolve_u4(symtab)? },
            Inst::SRI(dst, src, amt) => O::SRI { dst, src, amt: amt.resolve_u4(symtab)? },
            Inst::SAI(dst, src, amt) => O::SAI { dst, src, amt: amt.resolve_u4(symtab)? },
            Inst::TEq(a, b)          => O::TEq { a, b },
            Inst::TNE(a, b)          => O::TNE { a, b },
            Inst::TGT(a, b)          => O::TGT { a, b },
            Inst::TLT(a, b)          => O::TLT { a, b },
            Inst::TGE(a, b)          => O::TGE { a, b },
            Inst::TLE(a, b)          => O::TLE { a, b },
            Inst:: JR(off)           => O::JR  {           off: off.resolve_rel8(symtab, pc)? },
            Inst:: BR(cnd, off)      => O::BR  { cnd,      off: off.resolve_rel8(symtab, pc)? },
            Inst::  J(ptr, off)      => O::J   {      ptr, off: off.resolve_u4(symtab)? },
            Inst::  B(cnd, ptr, off) => O::B   { cnd, ptr, off: off.resolve_u4(symtab)? },
        };

        inst.encode()
    }
}

#[derive(Clone, Copy)]
pub enum Asm {
    Label(&'static str),
    Const(&'static str, i32),
    Orig(u16),
    Reserve(u32),
    DataBytes(&'static [u8]),
    DataWords(&'static [u8]),
    Inst(Inst),
}

type Binary = Vec<u8>;

pub fn assemble(asm: &[Asm]) -> Option<Binary> {
    // compute addresses
    let (base, size, symtab) = {
        let mut ptr = 0u32;
        let mut minmax = (u32::max_value(), 0);
        let mut symtab = HashMap::new();

        for item in asm {
            // align ptr
            if ptr & 1 != 0 { ptr += 1 }

            match *item {
                Asm::Label(name) => {
                    if let Some(_) = symtab.insert(name, ptr as i32) {
                        return None;
                    }
                },
                Asm::Const(name, value) => {
                    if let Some(_) = symtab.insert(name, value) {
                        return None;
                    }
                },
                Asm::Orig(addr)    => ptr = addr as u32,
                Asm::Reserve(n)    => ptr += n,
                Asm::DataBytes(bs) => ptr += bs.len() as u32,
                Asm::DataWords(ws) => ptr += ws.len() as u32,
                Asm::Inst(_)       => ptr += 2,
            }

            minmax = (min(minmax.0, ptr), max(minmax.1, ptr));
        }

        (minmax.0, (minmax.1 - minmax.0) as usize, symtab)
    };

    // assemble for real
    let mut out: Binary = repeat(0u8).take(size).collect();
    let mut pc = 0u32;

    for item in asm {
        if pc & 1 != 0 { pc += 1 }

        match *item {
            Asm::Label(_) => { }
            Asm::Const(_, _) => { }
            Asm::Orig(addr) => {
                pc = addr as u32;
            }
            Asm::Reserve(n) => {
                pc += n;
            }
            Asm::DataBytes(bs) => {
                for b in bs {
                    let idx = (pc - base) as usize;
                    out[idx] = *b;
                    pc += 1;
                }
            }
            Asm::DataWords(ws) => {
                let bp = ws.as_ptr() as *const u8;
                let bs = unsafe { slice::from_raw_parts(bp, ws.len() * 2) };
                for b in bs {
                    let idx = (pc - base) as usize;
                    out[idx] = *b;
                    pc += 1;
                }
            },
            Asm::Inst(i) => {
                pc += 2;
                let word = i.assemble(&symtab, pc)?;
                let bs = word.to_le_bytes();
                let idx = (pc - base - 2) as usize;
                out[idx + 0] = bs[0];
                out[idx + 1] = bs[1];
            }
        }
    }

    Some(out)
}

