pragma solidity >=0.4.21 <0.7.0;

interface ArbOwner {
    // Support actions that can be taken by the chain's owner.
    // All methods will revert, unless the caller is the chain's owner.

    function giveOwnership(address newOwnerAddr) external;

    function addToReserveFunds() external payable;

    function setFeesEnabled(bool enabled) external;
    function getFeeRecipients() external view returns (address, address);
    function setFeeRecipients(address netFeeRecipient, address congestionFeeRecipient) external;
    function setFairGasPriceSender(address addr) external;
    function setGasAccountingParams(uint speedLimitPerBlock, uint gasPoolMax, uint maxTxGasLimit) external;

    function setSecondsPerSend(uint blocksPerSend) external;

    // Run a tx that is signed by some address A, but with the tx's gas limit supplied by the owner.
    // This allows the owner to force execution of a keyless deployer, even if the deployer doesn't request
    //       enough (L2) gas to run
    // signedTxMsg must be an L2 message of compressed ECDSA-signed tx type, signed by requiredSigner,
    //       with callvalue equal to the callvalue of this call to runTxWithExtraGas
    // if forcedNonce != MaxUint, signedTx.signer.nonce will be set equal to forcedNonce before trying to run the tx
    // if signedTx fails, this tx will fail with the same result code and returndata
    // if this tx returns successfully, it will return the returndata produced by signedTx
    // no receipt is produced for signedTx; the only receipt is for this call to runTxWithExtraGas
    function runTxWithExtraGas(bytes calldata signedTxMsg, uint gas, address requiredSigner, uint forcedNonce) external payable;

    // To upgrade ArbOS, the owner calls startArbosUpgrade, then calls continueArbosUpgrade one or more times to upload
    // the code to be installed as the upgrade, then calls finishArbosUpgrade to complete the upgrade and start executing the new code.
    function startCodeUpload() external;
    function continueCodeUpload(bytes calldata marshalledCode) external;
    function getUploadedCodeHash() external view returns(bytes32);
    function finishCodeUploadAsArbosUpgrade(bytes32 requiredCodeHash) external;
    function finishCodeUploadAsPluggable(uint id, bool keepState) external;

    // Bind an address to a pluggable, so the pluggable can be a contract.
    function bindAddressToPluggable(address addr, uint pluggableId) external;
}

