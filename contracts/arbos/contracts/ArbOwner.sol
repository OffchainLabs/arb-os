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

    function setBlocksPerSend(uint blocksPerSend) external;

    // Change the sequencer or its parameters
    // if sequencerAddr is zero, operate without a sequencer
    function changeSequencer(address sequencerAddr, uint maxDelayBlocks, uint maxDelaySeconds) external;

    // To upgrade ArbOS, the ower calls startArbosUpgrade, then calls continueArbosUpgrade one or more times to upload 
    // the code to be installed as the upgrade, then calls finishArbosUpgrade to complete the upgrade and start executing the new code.
    function startCodeUpload() external;
    function continueCodeUpload(bytes calldata marshalledCode) external;
    function finishCodeUploadAsArbosUpgrade() external;
    function finishCodeUploadAsPluggable(uint id, bool keepState) external;

    // Bind an address to a pluggable, so the pluggable can be a contract.
    function bindAddressToPluggable(address addr, uint pluggableId) external;
}

