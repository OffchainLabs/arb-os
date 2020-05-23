/*
 * Copyright 2020, Offchain Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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

