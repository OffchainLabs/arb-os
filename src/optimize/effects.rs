/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::mavm::{AVMOpcode, Opcode};

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
    WriteLocal,
    ReadLocal,
    WriteGlobal,
    ReadGlobal,
    Unsure,
}

trait Effects {
    fn effects(&self) -> Vec<Effect>;
}

impl Effects for Opcode {
    fn effects(&self) -> Vec<Effect> {
        match self {
            Opcode::MakeFrame(..) => vec![Effect::PushStack],
            Opcode::GetLocal(..) => vec![Effect::ReadLocal],
            Opcode::SetLocal(..) => vec![Effect::WriteLocal],
            Opcode::ReserveCapture(..) => vec![Effect::WriteLocal],
            Opcode::MoveLocal(..) => vec![Effect::ReadLocal, Effect::WriteLocal],
            Opcode::ReserveCapture(..) => vec![Effect::WriteLocal],
            Opcode::Capture(..) => vec![],
            Opcode::MakeClosure(..) => vec![],
            Opcode::TupleGet(..) => vec![],
            Opcode::TupleSet(..) => vec![],
            Opcode::GetGlobalVar(..) => vec![],
            Opcode::SetGlobalVar(..) => vec![],
            Opcode::UncheckedFixedArrayGet(..) => vec![],
            Opcode::Label(Label) => vec![],
            Opcode::JumpTo(..) => vec![],
            Opcode::CjumpTo(..) => vec![],
            Opcode::Return => vec![],
            Opcode::FuncCall(prop) => {
                let mut effects = vec![];
                for _ in 0..prop.nargs {
                    effects.push(Effect::ReadStack(1));
                    effects.push(Effect::PopStack);
                }
                for _ in 0..prop.nouts {
                    effects.push(Effect::PushStack);
                }
                effects
            }
            Opcode::AVMOpcode(avm_op) => avm_op.effects(),
            Opcode::BackwardLabelTarget(..) => unreachable!("The optimizer shouldn't see this"),
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

            avm!(DebugPrint, Rset, ErrSet, SetGas, Jump, Cjump) => vec![
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
                Breakpoint, Log, Send, Inbox, InboxPeek, Halt,
                OpenInsn, Sideload
            ) => vec![Effect::Unsure],
        };
        effects
    }
}
