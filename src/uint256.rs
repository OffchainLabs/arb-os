use std::cmp::Ordering;
use std::fmt;
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Rem, Sub};
use num_bigint::{BigUint, BigInt, Sign, ToBigInt};
use num_traits::cast::ToPrimitive;
use num_traits::identities::{Zero, One};
use num_traits::sign::Signed;

#[derive(Debug, Clone, PartialEq, Eq, Ord, Hash)]
pub struct Uint256 {
	val: BigUint,
}

impl Uint256 {
	pub fn from_u64(x: u64) -> Self {
		Uint256{ val: BigUint::from(x) }
	}

	pub fn from_usize(x: usize) -> Self {
		Uint256{ val: BigUint::from(x) }
	}

	pub fn from_string(s: &str) -> Option<Self> {
		match BigUint::parse_bytes(s.as_bytes(), 10) {
			Some(bui) => match Uint256::trim(&bui) {
				(val, true) => Some(Uint256{ val }),
				(_, false) => None,
			}
			None => None
		}
	}
	
	pub fn to_usize(&self) -> Option<usize> {
		self.val.to_usize()
	}

	pub fn zero() -> Self {
		Uint256{ val: BigUint::zero() }
	}

	pub fn one() -> Self {
		Uint256{ val: BigUint::one() }
	}

	pub fn unary_minus(&self) -> Option<Self> {
		let s = self.to_signed();
		if s == BigInt::new(Sign::Minus, vec![0x8000_0000, 0, 0, 0, 0, 0, 0, 0]) {
			None
		} else {
			Some(Uint256{ val: Uint256::bigint_to_biguint(s.neg()) })
		}
	}

	pub fn bitwise_neg(&self) -> Self {
		let mut acc = Vec::new();
		for x in self.val.to_u32_digits() {
			acc.push(!x);
		}
		Uint256{ val: BigUint::new(acc) }
	}

	pub fn bitwise_and(&self, other: &Self) -> Self {
		let val = self.val.clone().bitand(other.val.clone());
		Uint256{ val }
	}

	pub fn bitwise_or(&self, other: &Self) -> Self {
		let val = self.val.clone().bitor(other.val.clone());
		Uint256{ val }
	}

	pub fn bitwise_xor(&self, other: &Self) -> Self {
		let val = self.val.clone().bitxor(other.val.clone());
		Uint256{ val }
	}

	pub fn add(&self, other: &Self) -> Self {
		let res = self.val.clone().add(&other.val);
		let (val, _) = Uint256::trim(&res);
		Uint256{ val }
	}

	pub fn sub(&self, other: &Self) -> Self {
		let res = self.val.clone().sub(&other.val);
		let (val, _) = Uint256::trim(&res);
		Uint256{ val }
	}

	pub fn mul(&self, other: &Self) -> Self {
		let res = self.val.clone().mul(&other.val);
		let (val, _) = Uint256::trim(&res);
		Uint256{ val }
	}

	pub fn div(&self, other: &Self) -> Option<Self> {
		if other.val.is_zero() {
			None
		} else {
			let val = self.val.clone().div(&other.val);
			Some(Uint256{ val })
		}
	}

	pub fn modulo(&self, other: &Self) -> Option<Self> {
		if other.val.is_zero() {
			None
		} else {
			let val = self.val.clone().rem(&other.val);
			Some(Uint256{ val })
		}
	}

	pub fn sdiv(&self, other: &Self) -> Option<Self> {
		if other.val.is_zero() {
			None
		} else {
			let val = self.to_signed().div(&other.to_signed());
			Some(Uint256{ val: Uint256::bigint_to_biguint(val) })
		}
	}

	pub fn smodulo(&self, other: &Self) -> Option<Self> {  //TODO: verify that semantics match AVM
		if other.val.is_zero() {
			None
		} else {
			let val = self.to_signed().rem(&other.to_signed());  
			Some(Uint256{ val: Uint256::bigint_to_biguint(val) })
		}
	}

	pub fn s_less_than(&self, other: &Self) -> bool {
		self.to_signed() < other.to_signed()
	}

	fn trim(bui: &BigUint) -> (BigUint, bool) {
		if bui.bits() <= 256 {
			(bui.clone(), true)
		} else {
			let mask = BigUint::new(vec![0xffff_ffff, 8]);
			(bui.bitand(mask), false)
		}
	}

	fn to_signed(&self) -> BigInt {
		if self.val.bits() < 256 {
			self.val.to_bigint().unwrap()
		} else {
			let unshifted = self.val.to_bigint().unwrap();
			let shift = BigInt::new(Sign::Plus, vec![1, 0, 0, 0, 0, 0, 0, 0, 0]);
			unshifted.sub(shift)		
		}
	}

	fn bigint_to_biguint(bi: BigInt) -> BigUint {
		if bi.is_negative() {
			bi.add(BigInt::new(Sign::Plus, vec![1, 0, 0, 0, 0, 0, 0, 0, 0])).to_biguint().unwrap()
		} else {
			bi.to_biguint().unwrap()
		}
	}
}

impl PartialOrd for Uint256 {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.val.cmp(&other.val))
	}
}

impl fmt::Display for Uint256 {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		let cutoff: usize = 1<<32;
		if self.val < BigUint::from(cutoff) {
			write!(f, "{}", self.val)
		} else {
			write!(f, "{:#x}", self.val)
		}
	}
}
