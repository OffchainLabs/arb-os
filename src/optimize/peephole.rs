use crate::compile::DebugInfo;
use crate::mavm::{Instruction, Opcode};

pub fn filter_pair(code: Vec<Instruction>, create: Opcode, cancel: Opcode) -> Vec<Instruction> {
    let mut queued = 0;
    let mut output = vec![];
    for curr in code {
        if curr.opcode == create {
            queued += 1
        } else if curr.opcode == cancel {
            if queued == 0 {
                output.push(Instruction::from_opcode(cancel, DebugInfo::default()));
            } else {
                queued -= 1;
            }
        } else {
            for _ in 0..queued {
                output.push(Instruction::from_opcode(create, DebugInfo::default()));
            }
            queued = 0;
            output.push(curr);
        }
    }
    output
}
