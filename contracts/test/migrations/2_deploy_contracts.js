const Fibonacci = artifacts.require("Fibonacci");
const PaymentChannel = artifacts.require("PaymentChannel");

module.exports = async function(deployer) {
  
  await deployer.deploy(Fibonacci)
  await deployer.deploy(PaymentChannel, Fibonacci.address)
};
