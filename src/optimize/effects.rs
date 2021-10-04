/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::FrameSize;
use crate::mavm::{AVMOpcode, Opcode};

#[derive(Clone, Copy, Debug)]
pub enum Effect {
    PushStack,
    PopStack,
    ReadStack(usize),
    SwapStack(usize),
    MoveToStack,
    MoveToAux,
    PushAux,
    PopAux,
    ReadAux,
    ReadLocal(u32),
    WriteLocal(u32),
    PhiLocal(u32, u32),
    ReadGlobal,
    WriteGlobal,
    OpenFrame(FrameSize),
    Unsure,
}

pub trait Effects {
    fn effects(&self) -> Vec<Effect>;
}

impl Effects for Opcode {
    fn effects(&self) -> Vec<Effect> {
        match self {
            Opcode::MakeFrame(size, ..) => vec![Effect::OpenFrame(*size)],
            Opcode::GetLocal(slot) => vec![Effect::ReadLocal(*slot), Effect::PushStack],
            Opcode::SetLocal(slot) => vec![
                Effect::ReadStack(1),
                Effect::PopStack,
                Effect::WriteLocal(*slot),
            ],
            Opcode::ReserveCapture(slot, _) => vec![Effect::WriteLocal(*slot)],
            Opcode::MoveLocal(dest, source) => {
                vec![Effect::PhiLocal(*dest, *source)]
            }
            Opcode::Capture(..) => vec![],
            Opcode::MakeClosure(..) => vec![],
            Opcode::TupleGet(..) => vec![Effect::ReadStack(1), Effect::PopStack, Effect::PushStack],
            Opcode::TupleSet(..) => vec![
                Effect::ReadStack(1),
                Effect::PopStack,
                Effect::ReadStack(1),
                Effect::PopStack,
                Effect::PushStack,
            ],
            Opcode::GetGlobalVar(..) => vec![],
            Opcode::SetGlobalVar(..) => vec![],
            Opcode::UncheckedFixedArrayGet(..) => vec![],
            Opcode::Label(..) => vec![],
            Opcode::JumpTo(..) => vec![],
            Opcode::CjumpTo(..) => vec![],
            Opcode::Return => vec![],
            Opcode::FuncCall(prop) => {
                let mut effects = vec![];
                if prop.view {
                    effects.push(Effect::ReadGlobal);
                }
                for _ in 0..prop.nargs {
                    effects.push(Effect::ReadStack(1));
                    effects.push(Effect::PopStack);
                }
                for _ in 0..prop.nouts {
                    effects.push(Effect::PushStack);
                }
                if prop.write {
                    effects.push(Effect::WriteGlobal);
                }
                effects
            }
            Opcode::AVMOpcode(avm_op) => avm_op.effects(),
            Opcode::BackwardLabelTarget(..) | Opcode::Pop(..) => {
                unreachable!("The optimizer shouldn't see this")
            }
        }
    }
}

impl Effects for AVMOpcode {
    fn effects(&self) -> Vec<Effect> {
        macro_rules! avm {
            ($first:ident $(,$opcode:ident)*) => {
                AVMOpcode::$first $(| AVMOpcode::$opcode)*
            };
        }

        #[rustfmt::skip]
        let effects = match self {
            avm!(Noop) => vec![],

            avm!(Pop)     => vec![Effect::PopStack],
            avm!(AuxPush) => vec![Effect::MoveToAux],
            avm!(AuxPop)  => vec![Effect::MoveToStack],

            avm!(Dup0) => vec![Effect::ReadStack(1), Effect::PushStack],
            avm!(Dup1) => vec![Effect::ReadStack(2), Effect::PushStack],
            avm!(Dup2) => vec![Effect::ReadStack(3), Effect::PushStack],

            avm!(Swap1) => vec![Effect::SwapStack(1)],
            avm!(Swap2) => vec![Effect::SwapStack(2)],

            avm!(NewBuffer, Spush, ErrCodePoint) => vec![Effect::PushStack],

            avm!(Rpush, PushGas, PCpush) => vec![Effect::ReadGlobal, Effect::PushStack],

            avm!(DebugPrint, Rset, ErrSet, SetGas, Log, Jump, Cjump) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::WriteGlobal
            ],

            avm!(IsZero, BitwiseNeg, Hash, Type, Keccakf, Blake2f, Tlen, EcPairing) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::PushStack,
            ],

            avm!(
                Add, Mul, Sub, Div, Sdiv, Mod, Smod, Exp, SignExtend,
                LessThan, GreaterThan, SLessThan, SGreaterThan, Equal,
                BitwiseAnd, BitwiseOr, BitwiseXor, Byte, EthHash2,
                ShiftLeft, ShiftRight, ShiftArith, PushInsn, Tget,
                GetBuffer8, GetBuffer64, GetBuffer256
            ) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::PushStack,
            ],

            avm!(
                AddMod, MulMod, Sha256f, Ripemd160f, PushInsnImm,
                SetBuffer8, SetBuffer64, SetBuffer256, Tset
            ) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::PushStack,
            ],

            avm!(EcRecover) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::PushStack,
            ],

            avm!(EcMul) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::PushStack,
                Effect::PushStack,
            ],

            avm!(EcAdd) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::PushStack,
                Effect::PushStack,
            ],

            avm!(Xset) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadAux,
                Effect::PopAux,
                Effect::PushAux,
            ],

            avm!(Xget) => vec![
                Effect::ReadStack(1), Effect::PopStack,
                Effect::ReadAux,
                Effect::PopAux,
                Effect::PushStack,
                Effect::PushAux,
            ],

            avm!(
                Zero, Error, StackEmpty, AuxStackEmpty, ErrPush,
                Breakpoint, Send, Inbox, InboxPeek, Halt,
                OpenInsn, Sideload
            ) => vec![Effect::Unsure],
        };
        effects
    }
}
