/*
 * Copyright 2019-2020, Offchain Labs, Inc.
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

pragma solidity ^0.6.0;

import "./ArbSys.sol";
import "../openzeppelin-contracts/contracts/token/ERC721/ERC721.sol";

contract ArbERC721 is ERC721("", "") {
    function tokensOfOwner(address owner) public view returns (uint256[] memory) {
        uint256[] memory ret;
        uint numTokens = balanceOf(owner);
        for(uint i=0; i<numTokens; i++) {
            ret[i] = tokenOfOwnerByIndex(owner, i);
        }
        return ret;
    }

    function adminMint(address account, uint256 tokenId) public {
        // This function is only callable through admin logic since address 1 cannot make calls
        require(msg.sender == address(1));
        _mint(account, tokenId);
    }

    function withdraw(address account, uint256 tokenId) public {
        require(msg.sender == ownerOf(tokenId));
        _burn(tokenId);
        ArbSys(100).withdrawERC721(account, tokenId);
    }
}
