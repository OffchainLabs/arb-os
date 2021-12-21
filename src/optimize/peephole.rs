/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved.
 */

use crate::compile::DebugInfo;
use crate::mavm::{AVMOpcode, Instruction, Opcode, Value};
use crate::opcode;

/// Find pairs of instructions whose combined effects could be re-expressed more cheaply
pub fn peephole(mut code: Vec<Instruction>) -> Vec<Instruction> {
    use AVMOpcode::*;

    code.push(opcode!(Noop, Value::from(0)));

    let mut code = code.into_iter().peekable();
    let mut out = Vec::with_capacity(code.len());

    macro_rules! drop {
        ($count:expr) => {{
            for _ in 0..$count {
                out.pop();
            }
        }};
    }

    while code.peek().is_some() {
        macro_rules! retry {
            () => {{
                out.push(code.next().unwrap());
                continue;
            }};
        }

        if out.len() < 2 {
            retry!();
        }

        let curr = &out[out.len() - 2];
        let next = &out[out.len() - 1];

        let curr_op = match &curr.opcode {
            Opcode::AVMOpcode(op) => op,
            _ => retry!(),
        };
        let next_op = match &next.opcode {
            Opcode::AVMOpcode(op) => op,
            _ => retry!(),
        };

        match (curr_op, next_op, &curr.immediate, &next.immediate) {
            (IsZero, IsZero, None, None) => drop!(2),
            (Dup0, Pop, None, None) => drop!(2),
            (Dup0, Swap1, _, None) => drop!(1),
            (Swap1, Swap1, None, None) => drop!(2),
            (Swap2, Swap2, None, None) => drop!(2),
            (Xget, Pop, Some(_), None) => drop!(2),
            (AuxPop, AuxPush, None, None) => drop!(2),
            (AuxPush, AuxPop, None, None) => drop!(2),
            (AuxPush, AuxPop, Some(value), None) | (AuxPop, AuxPush, Some(value), None) => {
                let value = value.clone();
                drop!(2);
                out.push(opcode!(Noop, value));
            }
            (AuxPush, AuxPop, None, Some(value)) => {
                let value = value.clone();
                drop!(2);
                out.push(opcode!(Swap1, value));
            }
            (Xset, Xget, Some(a), Some(b)) if a == b => {
                let offset = a.clone();
                drop!(2);
                out.push(opcode!(Dup0));
                out.push(opcode!(Xset, offset));
            }
            (Tget, Pop, Some(_), None) => {
                drop!(2);
                out.push(opcode!(Pop));
            }
            (_, Noop, _, None) => drop!(1),
            (_, Pop, _, Some(_)) => drop!(1),
            (Noop, _, Some(value), None) => {
                let mut other = next.clone();
                other.immediate = Some(value.clone());
                drop!(2);
                out.push(other);
            }
            (Pop, _, Some(_), _)
            | (Swap1, Add, None, None)
            | (Swap1, Mul, None, None)
            | (Swap1, Equal, None, None)
            | (Swap1, BitwiseAnd, None, None)
            | (Swap1, BitwiseOr, None, None)
            | (Swap1, BitwiseXor, None, None) => {
                out.swap_remove(out.len() - 2);
            }
            _ => {
                out.push(code.next().unwrap());
            }
        }
    }

    out.pop(); // pop the Noop
    out
}
