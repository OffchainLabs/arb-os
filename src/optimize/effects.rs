/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::translate;
use crate::mavm::{AVMOpcode, Instruction, Opcode};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Effect {
    PushStack,
    PopStack,
    DupStack(usize),
    ReadStack,
    SwapStack(usize),
    MoveToStack,
    MoveToAux,
    PushAux,
    PopAux,
    ReadAux,
    ReadLocal(u32),
    WriteLocal(u32),
    ReadGlobal,
    WriteGlobal,
    WritePC,
    Unsure,
}

pub trait Effects {
    fn effects(&self) -> Vec<Effect>;
}

impl Effects for Instruction {
    fn effects(&self) -> Vec<Effect> {
        let mut effects = match self.immediate {
            Some(_) => vec![Effect::PushStack],
            None => vec![],
        };
        effects.extend(self.opcode.effects());
        effects
    }
}

impl Effects for Opcode {
    fn effects(&self) -> Vec<Effect> {
        use Effect::*;
        use Opcode::*;
        match self {
            MakeFrame(..) | Label(..) | MoveLocal(..) | DropLocal(..) => vec![],
            GetLocal(slot) => vec![ReadLocal(*slot), PushStack],
            SetLocal(slot) => vec![ReadStack, PopStack, WriteLocal(*slot)],
            ReserveCapture(slot, _) => vec![WriteLocal(*slot), Unsure],
            Capture(..) => vec![Unsure],
            MakeClosure(..) => vec![Unsure],
            TupleGet(..) => vec![ReadStack, PopStack, PushStack],
            TupleSet(..) => vec![ReadStack, PopStack, ReadStack, PopStack, PushStack],
            GetGlobalVar(..) => vec![ReadGlobal, PushStack],
            SetGlobalVar(..) => vec![ReadStack, PopStack, WriteGlobal],
            CjumpTo(..) => vec![ReadStack, PopStack, ReadStack, PopStack, WritePC],
            JumpTo(..) => vec![ReadStack, PopStack, WritePC],
            Return => vec![WritePC],
            FuncCall(prop) => {
                let mut effects = vec![];
                if prop.view {
                    effects.push(ReadGlobal);
                }
                for _ in 0..(prop.nargs + 1) {
                    effects.push(ReadStack);
                    effects.push(PopStack);
                }
                for _ in 0..prop.nouts {
                    effects.push(PushStack);
                }
                if prop.write {
                    effects.push(WriteGlobal);
                }
                if prop.sensitive {
                    effects.push(Unsure);
                }
                effects
            }
            Pop(depth) => {
                let code = vec![Instruction::from(self.clone())];
                let code = translate::expand_pops(code);
                code.into_iter()
                    .map(|curr| curr.opcode.effects())
                    .flatten()
                    .filter(|effect| !matches!(effect, MoveToAux | MoveToStack))
                    .collect()
            }
            AVMOpcode(avm_op) => avm_op.effects(),
            BackwardLabelTarget(..) => {
                unreachable!("The optimizer shouldn't see this {:?}", self)
            }
        }
    }
}

impl Effects for AVMOpcode {
    fn effects(&self) -> Vec<Effect> {
        use Effect::*;
        macro_rules! avm {
            ($first:ident $(,$opcode:ident)*) => {
                AVMOpcode::$first $(| AVMOpcode::$opcode)*
            };
        }

        #[rustfmt::skip]
        let effects = match self {
            avm!(Noop) => vec![],

            avm!(Pop)     => vec![PopStack],
            avm!(AuxPush) => vec![MoveToAux],
            avm!(AuxPop)  => vec![MoveToStack],

            avm!(Dup0) => vec![DupStack(1)],
            avm!(Dup1) => vec![DupStack(2)],
            avm!(Dup2) => vec![DupStack(3)],

            avm!(Swap1) => vec![SwapStack(1)],
            avm!(Swap2) => vec![SwapStack(2)],

            avm!(NewBuffer, Spush) => vec![PushStack],

            avm!(Rpush, PushGas, PCpush, ErrCodePoint) => vec![ReadGlobal, PushStack],

            avm!(DebugPrint, Rset, ErrSet, SetGas, Log) => vec![
                ReadStack, PopStack,
                WriteGlobal
            ],

            avm!(Jump) => vec![
                ReadStack, PopStack,
                WritePC
            ],

            avm!(Cjump) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                WritePC
            ],

            avm!(IsZero, BitwiseNeg, Hash, Type, Keccakf, Blake2f, Tlen, EcPairing) => vec![
                ReadStack, PopStack,
                PushStack,
            ],

            avm!(
                Add, Mul, Sub, Div, Sdiv, Mod, Smod, Exp, SignExtend,
                LessThan, GreaterThan, SLessThan, SGreaterThan, Equal,
                BitwiseAnd, BitwiseOr, BitwiseXor, Byte, EthHash2,
                ShiftLeft, ShiftRight, ShiftArith, PushInsn, Tget,
                GetBuffer8, GetBuffer64, GetBuffer256
            ) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                PushStack,
            ],

            avm!(
                AddMod, MulMod, Sha256f, Ripemd160f, PushInsnImm,
                SetBuffer8, SetBuffer64, SetBuffer256, Tset
            ) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadStack, PopStack,
                PushStack,
            ],

            avm!(EcRecover) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadStack, PopStack,
                PushStack,
            ],

            avm!(EcMul) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadStack, PopStack,
                PushStack,
                PushStack,
            ],

            avm!(EcAdd) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadStack, PopStack,
                PushStack,
                PushStack,
            ],

            avm!(Xset) => vec![
                ReadStack, PopStack,
                ReadStack, PopStack,
                ReadAux,
                PopAux,
                PushAux,
            ],

            avm!(Xget) => vec![
                ReadStack, PopStack,
                ReadAux,
                PopAux,
                PushStack,
                PushAux,
            ],

            avm!(
                Zero, Error, StackEmpty, AuxStackEmpty, ErrPush,
                Breakpoint, Send, Inbox, InboxPeek, Halt,
                OpenInsn, Sideload
            ) => vec![Unsure],
        };
        effects
    }
}
