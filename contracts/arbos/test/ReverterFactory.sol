//SPDX-License-Identifier: Unlicense

contract Reverter {
  constructor(uint mode) public {
      require(mode == 0, "Mode not 0");
  }
}

contract ReverterFactory {
    constructor(uint mode) public {
        new Reverter(mode);
    }
}

