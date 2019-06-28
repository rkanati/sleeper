
mod assembler;
mod instruction;

use {
    crate::{
        assembler::{Asm, assemble},
        instruction::{Reg, Instruction},
    },
    std::{
        iter::repeat,
        str,
    },
};

enum BusAction {
    LoadByte { dest: Reg, addr: u16 },
    LoadWord { dest: Reg, addr: u16 },
    StoreByte { val: u8,  addr: u16 },
    StoreWord { val: u16, addr: u16 },
}

enum Action {
    Proceed,
    Jump(u16),
    Bus(BusAction),
    Halt,
}

#[derive(Clone, Copy, Default)]
struct Regs([u16; 16]);

impl std::ops::Index<Reg> for Regs {
    type Output = u16;
    fn index(&self, i: Reg) -> &u16 { &self.0[i as usize] }
}

impl std::ops::IndexMut<Reg> for Regs {
    fn index_mut(&mut self, i: Reg) -> &mut u16 { &mut self.0[i as usize] }
}

trait SignExtend<T>: Copy {
    fn sign_extend(self) -> T;
}

impl SignExtend<u16> for u8 {
    fn sign_extend(self) -> u16 {
        (self as i8).sign_extend()
    }
}

impl SignExtend<u16> for i8 {
    fn sign_extend(self) -> u16 {
        (self as i16) as u16
    }
}

struct CPU {
    r: Regs,
    pc: u16,
    ir: Option<u16>,
}

impl CPU {
    fn execute_instruction(&mut self, inst: Instruction) -> Action {
        use { BusAction as BA, Instruction as I };

        self.r[0] = 0;

        match inst {
            I::NOp => { },
            I::Hlt => return Action::Halt,

            // Loads/Stores
            I::LdB { dst, ptr } => {
                return Action::Bus(BA::LoadByte { dest: dst, addr: self.r[ptr] });
            },
            I::LdW { dst, ptr } => {
                return Action::Bus(BA::LoadWord { dest: dst, addr: self.r[ptr] });
            },
            I::StB { src, ptr } => {
                return Action::Bus(BA::StoreByte { val: self.r[src] as u8, addr: self.r[ptr] });
            },
            I::StW { src, ptr } => {
                return Action::Bus(BA::StoreWord { val: self.r[src], addr: self.r[ptr] });
            },

            // Copies
            I::LdI { dst, imm } => self.r[dst] = imm as u16,
            I::LUI { dst, imm } => self.r[dst] = (self.r[dst] & 0xff) | ((imm as u16) << 8),

            // Arithmetic and logic
            I::Add { dst, a, b } => self.r[dst] = self.r[a].wrapping_add(self.r[b]),
            I::Sub { dst, a, b } => self.r[dst] = self.r[a].wrapping_sub(self.r[b]),
            I::And { dst, a, b } => self.r[dst] = self.r[a] & self.r[b],
            I::Or  { dst, a, b } => self.r[dst] = self.r[a] | self.r[b],
            I::XOr { dst, a, b } => self.r[dst] = self.r[a] ^ self.r[b],
            I::Not { dst, src }  => self.r[dst] = !self.r[src],
            I::Inc { dst, imm }  => self.r[dst] = self.r[dst].wrapping_add(imm as u16),
            I::Dec { dst, imm }  => self.r[dst] = self.r[dst].wrapping_sub(imm as u16),

            // Shifts
            I::SL  { dst, src, amt } => self.r[dst] = self.r[src]       << (self.r[amt] & 0xf),
            I::SLI { dst, src, amt } => self.r[dst] = self.r[src]       << (amt         & 0xf),
            I::SR  { dst, src, amt } => self.r[dst] = self.r[src]       >> (self.r[amt] & 0xf),
            I::SRI { dst, src, amt } => self.r[dst] = self.r[src]       >> (amt         & 0xf),
            I::SA  { dst, src, amt } => self.r[dst] =(self.r[src] as i16>>(self.r[amt]&0xf)) as u16,
            I::SAI { dst, src, amt } => self.r[dst] =(self.r[src] as i16>>(amt        &0xf)) as u16,

            // Comparisons
            I::TEq { a, b } => self.r[14] = (self.r[a] == self.r[b]) as u16,
            I::TNE { a, b } => self.r[14] = (self.r[a] != self.r[b]) as u16,
            I::TGT { a, b } => self.r[14] = (self.r[a] >  self.r[b]) as u16,
            I::TLT { a, b } => self.r[14] = (self.r[a] <  self.r[b]) as u16,
            I::TGE { a, b } => self.r[14] = (self.r[a] >= self.r[b]) as u16,
            I::TLE { a, b } => self.r[14] = (self.r[a] <= self.r[b]) as u16,

            // Branches and jumps
            I::JR { off } =>
                return Action::Jump(self.pc.wrapping_add(off.sign_extend() << 1)),
            I::BR { cnd, off } => if self.r[cnd] != 0 {
                return Action::Jump(self.pc.wrapping_add(off.sign_extend() << 1));
            },
            I::J { ptr, off } =>
                return Action::Jump(self.r[ptr].wrapping_add((off as u16) << 1)),
            I::B { cnd, ptr, off } => if self.r[cnd] != 0 {
                return Action::Jump(self.r[ptr].wrapping_add((off as u16) << 1));
            },
        }

        Action::Proceed
    }

    fn run(&mut self) -> Option<BusAction> {
        loop {
            let inst = match self.ir.take() {
                None => return Some(BusAction::LoadWord { dest: 0, addr: self.pc }),
                Some(word) => Instruction::decode(word).expect("Instruction decode error")
            };

          //println!("    [{:04x}] {:?}", self.pc, inst);

            self.pc = self.pc.wrapping_add(2);

            match self.execute_instruction(inst) {
                Action::Bus(ba)  => return Some(ba),
                Action::Jump(to) => self.pc = to,
                Action::Proceed  => { },
                Action::Halt     => break None,
            }
        }
    }

    fn load_register(&mut self, dest: Reg, word: u16) {
        if dest == 0 {
            self.ir = Some(word);
        }
        else {
            self.r[dest] = word;
        }
    }

    fn new() -> CPU {
        CPU {
            r:  Default::default(),
            pc: 0xff00,
            ir: None,
        }
    }
}

fn main() {
    use assembler::{Inst::*, Asm::*, Imm::*};

    let source: &[Asm] = &[
        Orig(0xff00),
        Inst(LdI(2, LoB("message"))),
        Inst(LUI(2, HiB("message"))),

        Inst(LdI(3, LoB("uart"))),
        Inst(LUI(3, HiB("uart"))),

        Label("loop"),
        Inst(LdB(1, 2)),
        Inst(TEq(1, 0)),
        Inst(BR(14, Ref("done"))),
        Inst(StB(1, 3)),
        Inst(Inc(2, Lit(1))),
        Inst(JR(Ref("loop"))),

        Label("done"),
        Inst(LdI(1, Lit(b'\n' as i32))),
        Inst(StB(1, 3)),
        Inst(Hlt),

        Label("message"),
        DataBytes(b"Hello, World!\x00"),

        Const("uart", 0xe000),
    ];

    let program = assemble(source).unwrap();

    let mut memory: Box<[u8]> =
        repeat(0x00).take(0xff00)
        .chain(program.iter().cloned())
        .chain(repeat(0x00))
        .take(0x1_0000)
        .collect();

    let mut cpu = CPU::new();

    while let Some(action) = cpu.run() {
        match action {
            BusAction::LoadByte { dest, addr } => {
                let byte = memory[addr as usize];
                cpu.load_register(dest, byte as u16);
            }
            BusAction::LoadWord { dest, addr } => {
                assert!(addr & 1 == 0);
                let i = addr as usize;
                let word = ((memory[i+1] as u16) << 8) | (memory[i] as u16);
                cpu.load_register(dest, word);
            }
            BusAction::StoreByte { val, addr } => {
                if addr == 0xe000 {
                    if val.is_ascii_graphic() || val == 0x20 { print!("{}", val as char); }
                    else if val == b'\n'      { println!(); }
                    else                      { print!("\u{fffe}"); }
                }
                else {
                    memory[addr as usize] = val;
                }
            }
            BusAction::StoreWord { val, addr } => {
                if addr == 0xe000 {
                    println!("OUT: 0x{:04x}", val);
                }
                else {
                    assert!(addr & 1 == 0);
                    memory[(addr + 0) as usize] = (val >> 0) as u8;
                    memory[(addr + 1) as usize] = (val >> 8) as u8;
                }
            }
        }
    }
}

