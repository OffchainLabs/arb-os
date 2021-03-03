/**
 * @type import('hardhat/config').HardhatUserConfig
 */
require("hardhat-typechain");

module.exports = {
  solidity: "0.6.11",
  paths: {
    sources: "./arbos",
  },
  defaultNetwork: "localhost",
  networks: {
    localhost: {
      url: "http://127.0.0.1:7545",
      accounts: {
        mnemonic: 'jar deny prosper gasp flush glass core corn alarm treat leg smart',
        path: "m/44'/60'/0'/0",
        initialIndex: 0,
        count: 10,
      },
    },
  },
  typechain: {
    outDir: "src/types",
    target: "ethers-v5",
  },
};
