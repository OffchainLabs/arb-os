use crate::compile::DebugInfo;
use crate::mavm::{Instruction, Opcode};

pub fn filter_pair(code: Vec<Instruction>, create: Opcode, cancel: Opcode) -> Vec<Instruction> {
    let mut queued = 0;
    let mut output = Vec::with_capacity(code.len());
    let mut debug = DebugInfo::default();

    for curr in code {
        debug = curr.debug_info;

        if curr.opcode == create {
            queued += 1
        } else if curr.opcode == cancel {
            if queued == 0 {
                output.push(Instruction::from_opcode(cancel, debug));
            } else {
                queued -= 1;
            }
        } else {
            for _ in 0..queued {
                output.push(Instruction::from_opcode(create, debug));
            }
            queued = 0;
            output.push(curr);
        }
    }
    for _ in 0..queued {
        output.push(Instruction::from_opcode(create, debug));
    }
    output
}
