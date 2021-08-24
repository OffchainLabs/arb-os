pragma solidity >=0.4.21 <0.7.0;

interface ArbOwner {
    // Support actions that can be taken by the chain's owner.
    // All methods will revert, unless the caller is the chain's owner.

    function addToReserveFunds() external payable;

    function setFairGasPriceSender(address addr, bool isFairGasPriceSender) external;
    function isFairGasPriceSender(address addr) external view returns(bool);
    function getAllFairGasPriceSenders() external view returns(bytes memory);

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
    function finishCodeUploadAsPluggable(uint id, bool keepState) external;

    // Install the currently uploaded code as an ArbOS upgrade.
    // Revert if the hash of the uploaded code bytes does not equal newCodeHash
    // Revert if (oldCodeHash != 0) && (oldCodeHash != [hash of code bytes from the previous ArbOS upgrade]
    function finishCodeUploadAsArbosUpgrade(bytes32 newCodeHash, bytes32 oldCodeHash) external;

    // Bind an address to a pluggable, so the pluggable can be a contract.
    function bindAddressToPluggable(address addr, uint pluggableId) external;

    // Get and set chain parameters
    function getChainParameter(uint which) external view returns(uint);
    function setChainParameter(uint which, uint value) external;     // reverts if param doesn't already exist
    function createChainParameter(uint which, uint value) external;  // sets param, even if it didn't already exist
    function serializeAllParameters() external view returns(bytes memory);

    // Manage the set of allowed senders
    // address 0 and the chain owner are always allowed to send, even if not on the list
    function allowAllSenders() external;
    function allowOnlyOwnerToSend() external;
    function isAllowedSender(address addr) external view returns(bool);
    function addAllowedSender(address addr) external;
    function removeAllowedSender(address addr) external;
    function getAllAllowedSenders() external view returns(bytes memory);  // reverts if all or nearly all senders are allowed

    // Manage the set of chain owners
    function addChainOwner(address newOwner) external;
    function removeChainOwner(address ownerToRemove) external;    // revert if ownerToRemove is not an owner
    function isChainOwner(address addr) external view returns(bool);
    function getAllChainOwners() external view returns(bytes memory);

    // Manage exceptions to L1->L2 address remapping
    function addMappingException(uint from, uint to) external;
    function removeMappingException(uint from, uint to) external;
    function isMappingException(uint from, uint to) external view returns(bool);
    function getAllMappingExceptions() external view returns (bytes memory);

    function getTotalOfEthBalances() external view returns(uint);
}

