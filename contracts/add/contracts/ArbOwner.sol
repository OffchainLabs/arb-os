pragma solidity >=0.4.21 <0.7.0;

interface ArbOwner {
    function giveOwnership(address newOwnerAddr) external;

    function startArbosUpgrade() external;
    function continueArbosUpgrade(bytes calldata code) external;
    function finishArbosUpgrade() external;
}

