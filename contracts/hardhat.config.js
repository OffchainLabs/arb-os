const solcVersion = process.env.SOLC_VERSION || "0.8.10"

if(solcVersion !== "0.6.11" && solcVersion !== "0.8.10" && solcVersion !== "0.7.5")
  throw new Error("Select a supported solidity version.")

/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
  solidity: {
    compilers: [
      { version: solcVersion }
    ],
  },
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
  }
};
