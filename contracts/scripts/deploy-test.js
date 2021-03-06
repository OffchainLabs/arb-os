async function main() {
  const Fibonacci = await ethers.getContractFactory("Fibonacci");
  const fibonacci = await Fibonacci.deploy();
  console.log("Fibonacci deployed to:", fibonacci.address);

  const PaymentChannel = await ethers.getContractFactory("Fibonacci");
  const paymentChannel = await PaymentChannel.deploy(fibonacci.address);
  console.log("PaymentChannel deployed to:", paymentChannel.address);
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error(error);
    process.exit(1);
  });
