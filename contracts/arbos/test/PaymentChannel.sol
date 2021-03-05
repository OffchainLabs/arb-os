pragma solidity >=0.4.21 <0.7.0;

import "./Fibonacci.sol";

contract PaymentChannel {
	event Deposited(address indexed payee, uint256 weiAmount);
	event Withdrawn(address indexed payee, uint256 weiAmount);
	event Transfer(address indexed from, address indexed to, uint256 value);

	mapping (address => uint) balances;
	Fibonacci fib;

	constructor(address fibAddress) public {
		fib = Fibonacci(fibAddress);
	}

	function deposit() public payable {
		balances[msg.sender] += msg.value;

		emit Deposited(msg.sender, msg.value);
	}

	function getBalance(address addr) public view returns (uint) {
		return balances[addr];
	}

	function withdraw(uint amount) public {
		require(amount <= balances[msg.sender]);
		balances[msg.sender] -= amount;
		msg.sender.transfer(amount);

		emit Withdrawn(msg.sender, amount);
	}

	function transfer(address dest, uint amount) public {
		require(amount <= balances[msg.sender]);
		balances[msg.sender] -= amount;
		balances[dest] += amount;

		emit Transfer(msg.sender, dest, amount);
	}

	function transferFib(address dest, uint count) public {
	    //Fibonacci newFib = new Fibonacci();

		fib.generateFib(count + 1);
		uint amount = fib.getFib(count) + balances[address(5)];

		require(amount <= balances[msg.sender]);
		balances[msg.sender] -= amount;
		balances[dest] += amount;

		emit Transfer(msg.sender, dest, amount);
	}

	function testCreate() public returns (uint) {
		Fibonacci newFib = new Fibonacci();
		newFib.generateFib(3);
		return newFib.getFib(2);
	}
}
