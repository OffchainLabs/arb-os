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
use crate::run::bytestack_from_bytes;
use bytes::{BufMut, BytesMut};
use std::fs::File;
use std::io::Write;
use std::path::Path;

pub fn generate_contract_template_file_or_die(path: &Path) {
    let display = path.display();
    let mut file = match File::create(&path) {
        Err(why) => panic!("couldn't create {}: {}", display, why),
        Ok(file) => file,
    };

    match file.write_all(&mini_code_for_templates().freeze()[..]) {
        Err(why) => panic!("couldn't write to {}: {}", display, why),
        Ok(_) => {}
    }
}

fn mini_code_for_avm_value(buf: &mut BytesMut, val: Value) {
    match val {
        Value::Int(ui) => {
            buf.extend(ui.to_string().as_bytes());
        }
        Value::Tuple(tup) => {
            buf.put_u8(b'(');
            for t in &*tup {
                mini_code_for_avm_value(buf, t.clone());
                buf.put_u8(b',');
            }
            buf.put_u8(b')');
        }
        _ => {
            panic!("Can't generate code for this AVM value: {}", val);
        }
    }
}

fn mini_code_for_bytes(buf: &mut BytesMut, b: &[u8]) {
    mini_code_for_avm_value(buf, bytestack_from_bytes(b));
}

fn mini_code_getter_for_bytes(buf: &mut BytesMut, name: &str, b: &[u8]) {
    buf.put(&b"public func "[..]);
    buf.put(name.as_bytes());
    buf.put(&b"() -> MarshalledBytes {\n  return unsafecast<MarshalledBytes>("[..]);
    mini_code_for_bytes(buf, b);
    buf.put(&b");\n}\n\n"[..]);
}

fn mini_code_for_templates() -> BytesMut {
    let mut buf = BytesMut::with_capacity(1024);
    buf.put(&b"// DO NOT EDIT -- this is machine-generated code.\n\n"[..]);
    buf.put(&b"import type MarshalledBytes;\n\n"[..]);
    mini_code_for_erc20(&mut buf);
    mini_code_for_erc721(&mut buf);
    buf
}

fn mini_code_for_erc20(buf: &mut BytesMut) {
    mini_code_getter_for_bytes(
        buf,
        "getErc20code",
        &hex::decode("608060405234801561001057600080fd5b506004361061009e5760003560e01c8063a457c2d711610066578063a457c2d71461026b578063a9059cbb146102d1578063dd62ed3e14610337578063e58306f9146103af578063f3fef3a3146103fd5761009e565b8063095ea7b3146100a357806318160ddd1461010957806323b872dd1461012757806339509351146101ad57806370a0823114610213575b600080fd5b6100ef600480360360408110156100b957600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919050505061044b565b604051808215151515815260200191505060405180910390f35b610111610469565b6040518082815260200191505060405180910390f35b6101936004803603606081101561013d57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610473565b604051808215151515815260200191505060405180910390f35b6101f9600480360360408110156101c357600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291908035906020019092919050505061054c565b604051808215151515815260200191505060405180910390f35b6102556004803603602081101561022957600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff1690602001909291905050506105ff565b6040518082815260200191505060405180910390f35b6102b76004803603604081101561028157600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610647565b604051808215151515815260200191505060405180910390f35b61031d600480360360408110156102e757600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610714565b604051808215151515815260200191505060405180910390f35b6103996004803603604081101561034d57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610732565b6040518082815260200191505060405180910390f35b6103fb600480360360408110156103c557600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803590602001909291905050506107b9565b005b6104496004803603604081101561041357600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610800565b005b600061045f6104586108ae565b84846108b6565b6001905092915050565b6000600254905090565b6000610480848484610aad565b6105418461048c6108ae565b61053c856040518060600160405280602881526020016112f660289139600160008b73ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006104f26108ae565b73ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610d639092919063ffffffff16565b6108b6565b600190509392505050565b60006105f56105596108ae565b846105f0856001600061056a6108ae565b73ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008973ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610e2390919063ffffffff16565b6108b6565b6001905092915050565b60008060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020549050919050565b600061070a6106546108ae565b8461070585604051806060016040528060258152602001611388602591396001600061067e6108ae565b73ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008a73ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610d639092919063ffffffff16565b6108b6565b6001905092915050565b60006107286107216108ae565b8484610aad565b6001905092915050565b6000600160008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054905092915050565b600173ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146107f257600080fd5b6107fc8282610eab565b5050565b61080a3382611066565b606473ffffffffffffffffffffffffffffffffffffffff1663a1db978283836040518363ffffffff1660e01b8152600401808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200182815260200192505050600060405180830381600087803b15801561089257600080fd5b505af11580156108a6573d6000803e3d6000fd5b505050505050565b600033905090565b600073ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff16141561093c576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260248152602001806113646024913960400191505060405180910390fd5b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff1614156109c2576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260228152602001806112ae6022913960400191505060405180910390fd5b80600160008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508173ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff167f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b925836040518082815260200191505060405180910390a3505050565b600073ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff161415610b33576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602581526020018061133f6025913960400191505060405180910390fd5b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415610bb9576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260238152602001806112696023913960400191505060405180910390fd5b610c24816040518060600160405280602681526020016112d0602691396000808773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610d639092919063ffffffff16565b6000808573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002081905550610cb7816000808573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610e2390919063ffffffff16565b6000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508173ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040518082815260200191505060405180910390a3505050565b6000838311158290610e10576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825283818151815260200191508051906020019080838360005b83811015610dd5578082015181840152602081019050610dba565b50505050905090810190601f168015610e025780820380516001836020036101000a031916815260200191505b509250505060405180910390fd5b5060008385039050809150509392505050565b600080828401905083811015610ea1576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252601b8152602001807f536166654d6174683a206164646974696f6e206f766572666c6f77000000000081525060200191505060405180910390fd5b8091505092915050565b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415610f4e576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252601f8152602001807f45524332303a206d696e7420746f20746865207a65726f20616464726573730081525060200191505060405180910390fd5b610f6381600254610e2390919063ffffffff16565b600281905550610fba816000808573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610e2390919063ffffffff16565b6000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055508173ffffffffffffffffffffffffffffffffffffffff16600073ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040518082815260200191505060405180910390a35050565b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff1614156110ec576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602181526020018061131e6021913960400191505060405180910390fd5b6111578160405180606001604052806022815260200161128c602291396000808673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002054610d639092919063ffffffff16565b6000808473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020819055506111ae8160025461121e90919063ffffffff16565b600281905550600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef836040518082815260200191505060405180910390a35050565b600061126083836040518060400160405280601e81526020017f536166654d6174683a207375627472616374696f6e206f766572666c6f770000815250610d63565b90509291505056fe45524332303a207472616e7366657220746f20746865207a65726f206164647265737345524332303a206275726e20616d6f756e7420657863656564732062616c616e636545524332303a20617070726f766520746f20746865207a65726f206164647265737345524332303a207472616e7366657220616d6f756e7420657863656564732062616c616e636545524332303a207472616e7366657220616d6f756e74206578636565647320616c6c6f77616e636545524332303a206275726e2066726f6d20746865207a65726f206164647265737345524332303a207472616e736665722066726f6d20746865207a65726f206164647265737345524332303a20617070726f76652066726f6d20746865207a65726f206164647265737345524332303a2064656372656173656420616c6c6f77616e63652062656c6f77207a65726fa265627a7a7230582008f67bbb7f3798a4a37cccb57525d43e358a4946085d9f30c7437c3e3613f4c164736f6c634300050a0032").unwrap()
    );

    buf.put(
        &b"public func getErc20storage() -> map<uint, uint> {\n return newmap<uint, uint>;\n}\n\n"
            [..],
    );

    // hash adddress to avoid collision with other addresses generated by the old Arbitrum compiler
    buf.put(&b"public func getErc20address() -> address {\n  return address(hash(0x895521964D724c8362A36608AAf09A3D7d0A0445));\n}\n\n"[..])
}

fn mini_code_for_erc721(buf: &mut BytesMut) {
    mini_code_getter_for_bytes(
        buf,
        "getErc721code",
        &hex::decode("608060405234801561001057600080fd5b50600436106101005760003560e01c80636352211e11610097578063b88d4fde11610066578063b88d4fde14610573578063e58306f914610678578063e985e9c5146106c6578063f3fef3a31461074257610100565b80636352211e146103c457806370a08231146104325780638462151c1461048a578063a22cb4651461052357610100565b806323b872dd116100d357806323b872dd146102445780632f745c59146102b257806342842e0e146103145780634f6ccce71461038257610100565b806301ffc9a714610105578063081812fc1461016a578063095ea7b3146101d857806318160ddd14610226575b600080fd5b6101506004803603602081101561011b57600080fd5b8101908080357bffffffffffffffffffffffffffffffffffffffffffffffffffffffff19169060200190929190505050610790565b604051808215151515815260200191505060405180910390f35b6101966004803603602081101561018057600080fd5b81019080803590602001909291905050506107f7565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b610224600480360360408110156101ee57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610892565b005b61022e610a79565b6040518082815260200191505060405180910390f35b6102b06004803603606081101561025a57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610a86565b005b6102fe600480360360408110156102c857600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610afc565b6040518082815260200191505060405180910390f35b6103806004803603606081101561032a57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050610bbb565b005b6103ae6004803603602081101561039857600080fd5b8101908080359060200190929190505050610bdb565b6040518082815260200191505060405180910390f35b6103f0600480360360208110156103da57600080fd5b8101908080359060200190929190505050610c5b565b604051808273ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200191505060405180910390f35b6104746004803603602081101561044857600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610d23565b6040518082815260200191505060405180910390f35b6104cc600480360360208110156104a057600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190505050610df8565b6040518080602001828103825283818151815260200191508051906020019060200280838360005b8381101561050f5780820151818401526020810190506104f4565b505050509050019250505060405180910390f35b6105716004803603604081101561053957600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803515159060200190929190505050610e59565b005b6106766004803603608081101561058957600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190803590602001906401000000008111156105f057600080fd5b82018360208201111561060257600080fd5b8035906020019184600183028401116401000000008311171561062457600080fd5b91908080601f016020809104026020016040519081016040528093929190818152602001838380828437600081840152601f19601f820116905080830192505050505050509192919290505050611011565b005b6106c46004803603604081101561068e57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050611089565b005b610728600480360360408110156106dc57600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff169060200190929190803573ffffffffffffffffffffffffffffffffffffffff1690602001909291905050506110d0565b604051808215151515815260200191505060405180910390f35b61078e6004803603604081101561075857600080fd5b81019080803573ffffffffffffffffffffffffffffffffffffffff16906020019092919080359060200190929190505050611164565b005b6000806000837bffffffffffffffffffffffffffffffffffffffffffffffffffffffff19167bffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916815260200190815260200160002060009054906101000a900460ff169050919050565b600061080282611212565b610857576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602c8152602001806123fa602c913960400191505060405180910390fd5b6002600083815260200190815260200160002060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff169050919050565b600061089d82610c5b565b90508073ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff161415610924576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602181526020018061244f6021913960400191505060405180910390fd5b8073ffffffffffffffffffffffffffffffffffffffff16610943611284565b73ffffffffffffffffffffffffffffffffffffffff16148061097257506109718161096c611284565b6110d0565b5b6109c7576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252603881526020018061236f6038913960400191505060405180910390fd5b826002600084815260200190815260200160002060006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550818373ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff167f8c5be1e5ebec7d5bd14f71427d1e84f3dd0314c0f7b2291e5b200ac8c7c3b92560405160405180910390a4505050565b6000600780549050905090565b610a97610a91611284565b8261128c565b610aec576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260318152602001806124706031913960400191505060405180910390fd5b610af7838383611380565b505050565b6000610b0783610d23565b8210610b5e576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602b8152602001806122c2602b913960400191505060405180910390fd5b600560008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208281548110610ba857fe5b9060005260206000200154905092915050565b610bd683838360405180602001604052806000815250611011565b505050565b6000610be5610a79565b8210610c3c576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602c8152602001806124a1602c913960400191505060405180910390fd5b60078281548110610c4957fe5b90600052602060002001549050919050565b6000806001600084815260200190815260200160002060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff169050600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415610d1a576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260298152602001806123d16029913960400191505060405180910390fd5b80915050919050565b60008073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415610daa576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602a8152602001806123a7602a913960400191505060405180910390fd5b610df1600360008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000206113a4565b9050919050565b6060610e03826113b2565b805480602002602001604051908101604052809291908181526020018280548015610e4d57602002820191906000526020600020905b815481526020019060010190808311610e39575b50505050509050919050565b610e61611284565b73ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415610f02576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260198152602001807f4552433732313a20617070726f766520746f2063616c6c65720000000000000081525060200191505060405180910390fd5b8060046000610f0f611284565b73ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060006101000a81548160ff0219169083151502179055508173ffffffffffffffffffffffffffffffffffffffff16610fbc611284565b73ffffffffffffffffffffffffffffffffffffffff167f17307eab39ab6107e8899845ad3d59bd9653f200f220920489ca2b5937696c3183604051808215151515815260200191505060405180910390a35050565b61102261101c611284565b8361128c565b611077576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260318152602001806124706031913960400191505060405180910390fd5b611083848484846113fa565b50505050565b600173ffffffffffffffffffffffffffffffffffffffff163373ffffffffffffffffffffffffffffffffffffffff16146110c257600080fd5b6110cc828261146c565b5050565b6000600460008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002060009054906101000a900460ff16905092915050565b61116e338261148d565b606473ffffffffffffffffffffffffffffffffffffffff1663f3e414f883836040518363ffffffff1660e01b8152600401808373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200182815260200192505050600060405180830381600087803b1580156111f657600080fd5b505af115801561120a573d6000803e3d6000fd5b505050505050565b6000806001600084815260200190815260200160002060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff169050600073ffffffffffffffffffffffffffffffffffffffff168173ffffffffffffffffffffffffffffffffffffffff161415915050919050565b600033905090565b600061129782611212565b6112ec576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602c815260200180612343602c913960400191505060405180910390fd5b60006112f783610c5b565b90508073ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff16148061136657508373ffffffffffffffffffffffffffffffffffffffff1661134e846107f7565b73ffffffffffffffffffffffffffffffffffffffff16145b80611377575061137681856110d0565b5b91505092915050565b61138b8383836114c7565b6113958382611722565b61139f82826118c0565b505050565b600081600001549050919050565b6000600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000209050919050565b611405848484611380565b61141184848484611987565b611466576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260328152602001806122ed6032913960400191505060405180910390fd5b50505050565b6114768282611b77565b61148082826118c0565b61148981611d8f565b5050565b6114978282611ddb565b6114a18282611722565b600060066000838152602001908152602001600020819055506114c381611f6a565b5050565b8273ffffffffffffffffffffffffffffffffffffffff166114e782610c5b565b73ffffffffffffffffffffffffffffffffffffffff1614611553576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260298152602001806124266029913960400191505060405180910390fd5b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff1614156115d9576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252602481526020018061231f6024913960400191505060405180910390fd5b6115e281612024565b611629600360008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000206120e2565b611670600360008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020612105565b816001600083815260200190815260200160002060006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550808273ffffffffffffffffffffffffffffffffffffffff168473ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60405160405180910390a4505050565b600061177a6001600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208054905061211b90919063ffffffff16565b9050600060066000848152602001908152602001600020549050818114611867576000600560008673ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200190815260200160002083815481106117e757fe5b9060005260206000200154905080600560008773ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020838154811061183f57fe5b9060005260206000200181905550816006600083815260200190815260200160002081905550505b600560008573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208054809190600190036118b99190612270565b5050505050565b600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020805490506006600083815260200190815260200160002081905550600560008373ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000208190806001815401808255809150509060018203906000526020600020016000909192909190915055505050565b60006119a88473ffffffffffffffffffffffffffffffffffffffff16612165565b6119b55760019050611b6f565b60008473ffffffffffffffffffffffffffffffffffffffff1663150b7a026119db611284565b8887876040518563ffffffff1660e01b8152600401808573ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020018473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16815260200183815260200180602001828103825283818151815260200191508051906020019080838360005b83811015611a97578082015181840152602081019050611a7c565b50505050905090810190601f168015611ac45780820380516001836020036101000a031916815260200191505b5095505050505050602060405180830381600087803b158015611ae657600080fd5b505af1158015611afa573d6000803e3d6000fd5b505050506040513d6020811015611b1057600080fd5b8101908080519060200190929190505050905063150b7a0260e01b7bffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916817bffffffffffffffffffffffffffffffffffffffffffffffffffffffff1916149150505b949350505050565b600073ffffffffffffffffffffffffffffffffffffffff168273ffffffffffffffffffffffffffffffffffffffff161415611c1a576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260208152602001807f4552433732313a206d696e7420746f20746865207a65726f206164647265737381525060200191505060405180910390fd5b611c2381611212565b15611c96576040517f08c379a000000000000000000000000000000000000000000000000000000000815260040180806020018281038252601c8152602001807f4552433732313a20746f6b656e20616c7265616479206d696e7465640000000081525060200191505060405180910390fd5b816001600083815260200190815260200160002060006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff160217905550611d2f600360008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff168152602001908152602001600020612105565b808273ffffffffffffffffffffffffffffffffffffffff16600073ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60405160405180910390a45050565b6007805490506008600083815260200190815260200160002081905550600781908060018154018082558091505090600182039060005260206000200160009091929091909150555050565b8173ffffffffffffffffffffffffffffffffffffffff16611dfb82610c5b565b73ffffffffffffffffffffffffffffffffffffffff1614611e67576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825260258152602001806124cd6025913960400191505060405180910390fd5b611e7081612024565b611eb7600360008473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff1681526020019081526020016000206120e2565b60006001600083815260200190815260200160002060006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff16021790555080600073ffffffffffffffffffffffffffffffffffffffff168373ffffffffffffffffffffffffffffffffffffffff167fddf252ad1be2c89b69c2b068fc378daa952ba7f163c4a11628f55a4df523b3ef60405160405180910390a45050565b6000611f85600160078054905061211b90919063ffffffff16565b9050600060086000848152602001908152602001600020549050600060078381548110611fae57fe5b906000526020600020015490508060078381548110611fc957fe5b906000526020600020018190555081600860008381526020019081526020016000208190555060078054809190600190036120049190612270565b506000600860008681526020019081526020016000208190555050505050565b600073ffffffffffffffffffffffffffffffffffffffff166002600083815260200190815260200160002060009054906101000a900473ffffffffffffffffffffffffffffffffffffffff1673ffffffffffffffffffffffffffffffffffffffff16146120df5760006002600083815260200190815260200160002060006101000a81548173ffffffffffffffffffffffffffffffffffffffff021916908373ffffffffffffffffffffffffffffffffffffffff1602179055505b50565b6120fa6001826000015461211b90919063ffffffff16565b816000018190555050565b6001816000016000828254019250508190555050565b600061215d83836040518060400160405280601e81526020017f536166654d6174683a207375627472616374696f6e206f766572666c6f7700008152506121b0565b905092915050565b60008060007fc5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a47060001b9050833f91506000801b82141580156121a75750808214155b92505050919050565b600083831115829061225d576040517f08c379a00000000000000000000000000000000000000000000000000000000081526004018080602001828103825283818151815260200191508051906020019080838360005b83811015612222578082015181840152602081019050612207565b50505050905090810190601f16801561224f5780820380516001836020036101000a031916815260200191505b509250505060405180910390fd5b5060008385039050809150509392505050565b81548183558181111561229757818360005260206000209182019101612296919061229c565b5b505050565b6122be91905b808211156122ba5760008160009055506001016122a2565b5090565b9056fe455243373231456e756d657261626c653a206f776e657220696e646578206f7574206f6620626f756e64734552433732313a207472616e7366657220746f206e6f6e20455243373231526563656976657220696d706c656d656e7465724552433732313a207472616e7366657220746f20746865207a65726f20616464726573734552433732313a206f70657261746f7220717565727920666f72206e6f6e6578697374656e7420746f6b656e4552433732313a20617070726f76652063616c6c6572206973206e6f74206f776e6572206e6f7220617070726f76656420666f7220616c6c4552433732313a2062616c616e636520717565727920666f7220746865207a65726f20616464726573734552433732313a206f776e657220717565727920666f72206e6f6e6578697374656e7420746f6b656e4552433732313a20617070726f76656420717565727920666f72206e6f6e6578697374656e7420746f6b656e4552433732313a207472616e73666572206f6620746f6b656e2074686174206973206e6f74206f776e4552433732313a20617070726f76616c20746f2063757272656e74206f776e65724552433732313a207472616e736665722063616c6c6572206973206e6f74206f776e6572206e6f7220617070726f766564455243373231456e756d657261626c653a20676c6f62616c20696e646578206f7574206f6620626f756e64734552433732313a206275726e206f6620746f6b656e2074686174206973206e6f74206f776ea265627a7a723058207822a9b34e46fe3b87e29b3ef685970ea8094ba7585b6009dcbafe23363a57d564736f6c634300050a0032").unwrap()
    );

    buf.put(
        &b"public func getErc721storage() -> map<uint, uint> {\n  return newmap<uint, uint>\n"[..],
    );
    buf.put(
        &b"   with { [0x67be87c3ff9960ca1e9cfac5cab2ff4747269cf9ed20c9b7306235ac35a491c5] = 1 }\n"
            [..],
    );
    buf.put(
        &b"   with { [0xf7815fccbf112960a73756e185887fedcb9fc64ca0a16cc5923b7960ed780800] = 1 }\n"
            [..],
    );
    buf.put(
        &b"   with { [0x77b7bbe0e49b76487c9476b5db3354cf5270619d0037ccb899c2a4c4a75b4318] = 1 };\n"
            [..],
    );
    buf.put(&b"}\n\n"[..]);

    // hash adddress to avoid collision with other addresses generated by the old Arbitrum compiler
    buf.put(&b"public func getErc721address() -> address {\n  return address(hash(0x0b55929f4095f677C9Ec1F4810C3E59CCD6D33C7));\n}\n\n"[..])
}
