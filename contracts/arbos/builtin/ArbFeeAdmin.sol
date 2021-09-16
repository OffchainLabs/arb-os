pragma solidity >=0.4.21 <0.7.0;

interface ArbFeeAdmin {
    function isFeeAdministrator(address addr) external view returns(bool);

    function addFeeAdministrator(address addr) external;

    function removeFeeAdministrator(address addr) external;

    function getAllFeeAdministrators() external view returns(bytes memory);

    function setNetworkFeeRecipient(address addr) external;

    function getNetworkFeeRecipient() external view returns(address);

    function setCongestionFeeRecipient(address addr) external;

    function getCongestionFeeRecipient() external view returns(address);
}

