use super::abi::ArbInfo;
use super::*;

#[test]
fn test_payment_to_self() {
    let _ = _evm_payment_to_self(None, false).unwrap();
}

pub fn _evm_payment_to_self(log_to: Option<&Path>, debug: bool) -> Result<(), ethabi::Error> {
    let mut machine = load_from_file(Path::new("arb_os/arbos.mexe"));
    machine.start_at_zero();

    let my_addr = Uint256::from_u64(1025);

    machine.runtime_env.insert_eth_deposit_message(
        my_addr.clone(),
        my_addr.clone(),
        Uint256::from_u64(20000),
    );

    let arbinfo = ArbInfo::_new(false);
    let balance = arbinfo._get_balance(&mut machine, &my_addr)?;
    assert_eq!(balance, Uint256::from_u64(20000));

    let tx_id = machine.runtime_env.insert_tx_message(
        my_addr.clone(),
        Uint256::from_u64(1000000000),
        Uint256::zero(),
        my_addr.clone(),
        Uint256::from_u64(10000),
        &vec![],
        false,
    );

    let _ = if debug {
        machine.debug(None)
    } else {
        machine.run(None)
    };

    let receipts = machine.runtime_env.get_all_receipt_logs();
    let last_rcpt = receipts.len() - 1;
    assert_eq!(receipts[last_rcpt].get_request_id(), tx_id);
    assert!(receipts[last_rcpt].succeeded());

    let new_balance = arbinfo._get_balance(&mut machine, &my_addr)?;
    assert_eq!(new_balance, Uint256::from_u64(20000));

    if let Some(path) = log_to {
        machine
            .runtime_env
            .recorder
            .to_file(path, machine.get_total_gas_usage().to_u64().unwrap())
            .unwrap();
    }

    Ok(())
}
