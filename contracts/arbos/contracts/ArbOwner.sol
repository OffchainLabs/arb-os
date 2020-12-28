pragma solidity >=0.4.21 <0.7.0;

interface ArbOwner {
    // Support actions that can be taken by the chain's owner.
    // All methods will revert, unless the caller is the chain's owner.

    function giveOwnership(address newOwnerAddr) external;

    function addToReserveFunds() external payable;

    function getFeeRecipient() external view returns (address);
    function setFeeRecipient(address recipient) external;
    function getFeeRates() external view returns (uint, uint, uint, uint);
    function setFeeRates(uint num1, uint denom1, uint num2, uint denom2) external;
    function getFeeMaxes() external view returns (uint, uint, uint, uint);
    function setFeeMaxes(uint num1, uint denom1, uint num2, uint denom2) external;

    // Change the sequencer or its parameters
    // if sequencerAddr is zero, operate without a sequencer
    function changeSequencer(address sequencerAddr, uint maxDelayBlocks, uint maxDelaySeconds) external;

    // To upgrade ArbOS, the ower calls startArbosUpgrade, then calls continueArbosUpgrade one or more times to upload 
    // the code to be installed as the upgrade, then calls finishArbosUpgrade to complete the upgrade and start executing the new code.
    function startArbosUpgrade() external;
    function continueArbosUpgrade(bytes calldata marshalledCode) external;
    function finishArbosUpgrade() external;
}

