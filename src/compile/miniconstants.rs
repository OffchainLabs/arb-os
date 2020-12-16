/*
 * Copyright 2020, Offchain Labs, Inc. All rights reserved
 */

use crate::uint256::Uint256;
use std::collections::HashMap;


pub fn init_constant_table() -> HashMap<String, Uint256> {
    let mut ret = HashMap::new();
    for (s, i) in &[
        // addresses of precompiled contracts
        ("Address_ArbSys", 100),
        ("Address_ArbAddressTable", 102),
        ("Address_ArbBLS", 103),
        ("Address_ArbFunctionTable", 104),
        ("Address_ArbosTest", 105),
        ("Address_ArbOwner", 107),
        
        // indices of EVM operations
        ("EvmOp_stop", 0),
        ("EvmOp_sha3", 1),
        ("EvmOp_address", 2),
        ("EvmOp_balance", 3),
        ("EvmOp_selfbalance", 4),
        ("EvmOp_origin", 5),
        ("EvmOp_caller", 6),
        ("EvmOp_callvalue", 7),
        ("EvmOp_calldataload", 8),
        ("EvmOp_calldatasize", 9),
        ("EvmOp_calldatacopy", 10),
        ("EvmOp_codesize", 11),
        ("EvmOp_codecopy", 12),
        ("EvmOp_extcodesize", 13),
        ("EvmOp_extcodecopy", 14),
        ("EvmOp_extcodehash", 15),
        ("EvmOp_returndatasize", 16),
        ("EvmOp_returndatacopy", 17),
        ("EvmOp_timestamp", 18),
        ("EvmOp_number", 19),
        ("EvmOp_msize", 20),
        ("EvmOp_mload", 21),
        ("EvmOp_mstore", 22),
        ("EvmOp_mstore8", 23),
        ("EvmOp_sload", 24),
        ("EvmOp_sstore", 25),
        ("EvmOp_getjumpaddr", 26),
        ("EvmOp_msize", 27),
        ("EvmOp_log0", 28),
        ("EvmOp_log1", 29),
        ("EvmOp_log2", 30),
        ("EvmOp_log3", 31),
        ("EvmOp_log4", 32),
        ("EvmOp_call", 33),
        ("EvmOp_callcode", 34),
        ("EvmOp_delegatecall", 35),
        ("EvmOp_staticcall", 36),
        ("EvmOp_revert", 37),
        ("EvmOp_revert_knownPc", 38),
        ("EvmOp_return", 39),
        ("EvmOp_selfdestruct", 40),
        ("EvmOp_create", 41),
        ("EvmOp_create2", 42),
        ("EvmOp_chainId", 43),
        ("NumEvmOps", 44),
    ] {
        ret.insert(s.to_string(), Uint256::from_u64(*i));
    }
    ret
}