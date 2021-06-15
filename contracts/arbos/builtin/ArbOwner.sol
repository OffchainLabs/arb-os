pragma solidity >=0.4.21 <0.7.0;

interface ArbOwner {
    // Support actions that can be taken by the chain's owner.
    // All methods will revert, unless the caller is the chain's owner.

    function giveOwnership(address newOwnerAddr) external;

    function addToReserveFunds() external payable;

    function setFeesEnabled(bool enabled) external;
    function getFeeRecipients() external view returns (address, address);
    function setFeeRecipients(address netFeeRecipient, address congestionFeeRecipient) external;

    function setFairGasPriceSender(address addr, bool isFairGasPriceSender) external;
    function isFairGasPriceSender(address addr) external view returns(bool);
    function getAllFairGasPriceSenders() external view returns(bytes memory);

    function setGasAccountingParams(uint speedLimitPerBlock, uint gasPoolMax, uint maxTxGasLimit) external;

    function setSecondsPerSend(uint blocksPerSend) external;

    function setL1GasPriceEstimate(uint priceInGwei) external;

    // Deploy a contract on the chain
    // The contract is deployed as if it was submitted by deemedSender with deemedNonce
    // Reverts if there is already a contract at that address
    // Returns the address of the deployed contract
    function deployContract(bytes calldata constructorData, address deemedSender, uint deemedNonce) external payable returns(address);

    // To upgrade ArbOS, the owner calls startArbosUpgrade, then calls continueArbosUpgrade one or more times to upload
    // the code to be installed as the upgrade, then calls finishArbosUpgrade to complete the upgrade and start executing the new code.
    function startCodeUpload() external;
    function continueCodeUpload(bytes calldata marshalledCode) external;
    function getUploadedCodeHash() external view returns(bytes32);

    // Install the currently uploaded code as an ArbOS upgrade.
    // Revert if the hash of the uploaded code bytes does not equal newCodeHash
    // Revert if (oldCodeHash != 0) && (oldCodeHash != [hash of code bytes from the previous ArbOS upgrade]
    function finishCodeUploadAsArbosUpgrade(bytes32 newCodeHash, bytes32 oldCodeHash) external;

    // Manage the set of allowed senders
    // address 0 and the chain owner are always allowed to send, even if not on the list
    function allowAllSenders() external;
    function allowOnlyOwnerToSend() external;
    function isAllowedSender(address addr) external view returns(bool);
    function addAllowedSender(address addr) external;
    function removeAllowedSender(address addr) external;
    function getAllAllowedSenders() external view returns(bytes memory);  // reverts if all or nearly all senders are allowed

    function getTotalOfEthBalances() external view returns(uint);
}

