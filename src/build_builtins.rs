use crate::mavm::Value;
use crate::uint256::Uint256;


pub struct BuiltinArray {
    size: usize,
    top_step: usize,
    contents: Vec<Value>,
}

impl BuiltinArray {
    pub fn new(size: usize, base_val: Value) -> Self {
        let mut top_step = 1;
        while 8*top_step < size {
            top_step *= 8;
        }
        BuiltinArray{ size, top_step, contents: vec![base_val; 8*top_step] }
    }

    pub fn set(&mut self, idx: usize, val: Value) {
        self.contents[idx] = val;
    }

    pub fn get(&self, idx: usize) -> Value {
        self.contents[idx].clone()
    }

    pub fn to_value(&self) -> Value {
        Value::Tuple(vec![
            Value::Int(Uint256::from_usize(self.size)),
            Value::Int(Uint256::from_usize(self.top_step)),
            BuiltinArray::tuple_tree(self.top_step, &self.contents),
        ])
    }

    fn tuple_tree(top_step: usize, arr:&[Value]) -> Value {
        let mut v = Vec::new();
        if top_step == 1 {
            return Value::Tuple(arr.to_vec());
        }
        for i in 0..8 {
            v.push(BuiltinArray::tuple_tree(top_step/8, &arr[(8*i)..(8*(i+1))]));
        }
        Value::Tuple(v)
    }
}

