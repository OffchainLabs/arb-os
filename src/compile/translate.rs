
use crate::mavm::{AVMOpcode, Instruction, LabelGenerator, Opcode, Value};

pub fn expand_calls(code: Vec<Instruction>, label_gen: &mut LabelGenerator) -> Vec<Instruction> {
    let mut out = vec![];

    for curr in code {
        let debug = curr.debug_info;

        macro_rules! opcode {
            ($opcode:ident) => {
                Instruction::from_opcode(Opcode::AVMOpcode(AVMOpcode::$opcode), debug)
            };
            ($opcode:ident, $immediate:expr) => {
                Instruction::from_opcode_imm(
                    Opcode::AVMOpcode(AVMOpcode::$opcode),
                    $immediate,
                    debug,
                )
            };
        }

        match &curr.opcode {
            Opcode::FuncCall(prop) => {
                let return_label = label_gen.next();
                let codepoint_call = label_gen.next();

                if prop.returns {
                    out.push(opcode!(Swap1, Value::Label(return_label))); // bury the return label
                }

                // check whether we're calling on a codepoint or closure tuple
                out.push(opcode!(Dup0));                                // return ? ?
                out.push(opcode!(Type));                                // return ? type
                out.push(opcode!(Equal, Value::from(1)));               // return ? (1 for codepoint)
                out.push(opcode!(Cjump, Value::Label(codepoint_call))); // return ?

                // not a codepoint, let's unpack
                out.push(opcode!(Dup0));                 // return (closure, frame) (closure, frame)
                out.push(opcode!(Tget, Value::from(1))); // return (closure, frame) frame
                out.push(opcode!(Swap2));                // frame (closure, frame) return
                out.push(opcode!(Swap1));                // frame return (closure, frame)
                out.push(opcode!(Tget, Value::from(0))); // frame return closure

                out.push(Instruction::from_opcode(Opcode::Label(codepoint_call), debug));
                out.push(opcode!(Jump));
                out.push(Instruction::from_opcode(Opcode::Label(return_label), debug));
            }
            _ => out.push(curr),
        }
    }
    out
}
