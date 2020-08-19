# ArbGas Charging and Validator Reimbursement in Arbitrum Rollup

This document summarizes the economic model for reimbursing validator costs and managing congestion in Arbitrum Rollup.  

An Arbitrum chain collects ArbGas charges from users, for two purposes.  First, fees are collected in order to reimburse a chain's invited validators for their costs.  Second, if a chain is congested, so that not all user transactions can be served, a congestion auction rations access to the chain's services.

Initially, charges are denominated in ETH.  The use of other approved tokens will be supported soon afterward. For clarity, this document refers only to ETH; it should be obvious how to apply the same mechanisms to other approved tokens.

## Invited validators

A chain will typically have a set of invited validators. These are validators who have been promised reimbursement for their costs, at agreed-upon rates. The set of invited validators is configured when a chain is created.

Of course, anyone can be a validator of any Arbitrum chain--that's what makes Arbitrum trustless.  The only thing that makes invited validators different from ordinary validators is that invited validators get paid.

## ArbGas

ArbGas is the Arbitrum equivalent of Ethereum's gas.  It measures the amount of resources needed for one validator to execute operations for a chain. Every Arbitrum Virtual Machine (AVM) instruction consumes a fixed amount of ArbGas, so the ArbGas cost of a computation is determined by adding up the ArbGas over all of the AVM instructions in the computation.

(ArbGas charges are not exactly proportional to Ethereum gas, because some operations are relatively more expensive on Arbitrum and others relatively less expensive.)

Transactions submitted to the chain will be charged for the ArbGas they use, according to the algorithms described in this document. Typically, Arbitrum transaction costs will be vastly lower than the same transaction would cost on Ethereum--but Arbitrum gas is not free, because validators need to be paid and any congestion that occurs must be managed.

## Reimbursement rates

The total costs incurred by invited validators are reflected in reimbursement rates, which are parameters of a chain.

The **ArbGas reimbursement rate**, measured in wei per unit of ArbGas, reflects the total cost to all validators of supporting computation on a chain.

The **time reimbursement rate**, measured in wei per second, reflects the total cost to all validators of keeping a chain alive for a period of time. This includes the costs of maintaining the chain's state, and of the capital lock-up associated with one validator having a stake placed on the chain.

## Overhead

Each chain tracks a tally of the **overhead cost** owed by the chain's users.  Overhead reflects costs of the chain that were not directly incurred by any user. 

Two things add to the overhead tally. First, for every second of time that passes, the overhead tally is increased by the time reimbursement rate, reflecting the cost of keeping the chain alive. Second, the ArbGas costs of any work performed by the chain's level 2 management software (ArbOS) is added to the overhead tally.

The overhead tally reflects costs actually incurred by the invited validators, and these costs must be reimbursed. So the algorithm will add a surcharge to user transaction costs to cover overhead. As described below, this surcharge will vary dynamically, and will be just large enough to cover the actual overhead costs.

(Also as described below, there are also circumstances where the overhead tally is negative, meaning that more funds are available to cover overhead than are needed. This will cause the "surcharge" for overhead to be negative, essentially refunding the overpayment to users.)

## The Price of ArbGas

The ArbGas price paid by a transaction is the sum of three terms:

* the ArbGas reimbursement rate, to cover the direct cost of validating the transaction
* a surcharge to cover overhead, determined by the algorithm given below
* if the chain is congested, a congestion fee, which is determined by the congestion auction algorithm described below

Like in Ethereum, each transaction is submitted with a maximum amount of ArbGas it should be allotted, and an ArbGas price bid.  A transaction will never be charged more than its ArbGas price bid, but it may be charged less. If a transaction's submitter has insufficient funds to pay for its ArbGas, the transaction will be rejected.

The overhead surcharge is determined by first computing a surcharge target, which is equal to the overhead tally divided by the amount of ArbGas that would be consumed if the chain operated at full speed for two minutes. Then, a smoothed, weighted average of the surcharge target is used as the overhead surcharge.  The smoothing is designed to reduce fluctuation in the overhead surcharge.

If the overhead surcharge is negative (reflecting a refund to users), the amount of the refund is capped at half of the ArbGas reimbursement rate.  The cap is designed to maintain users' incentive to limit their usage, while still supporting a significant refund.

If the chain is not congested, that is, if the total ArbGas requested by currently queued transactions can all be serviced within a minute or so given the chain's capacity, then the congestion fee will be zero.  Otherwise, a congestion auction chooses a subset of transactions that will run, and sets the congestion fee based on their bids, as described below.

## The Congestion Auction

The congestion auction operates if the gas requested by queued transactions is more than the chain can accomodate.  The transactions that have submitted the largest gas price bids will get to run, and the rest will be rejected and will not run.

The auction is a second-price type of auction, meaning that the price paid by winners is equal to the highest bid that was rejected. This makes the auction strategy-proof, in the sense that the optimum strategy for a bidder is to make their bid equal to the maximum amount that they would be willing to pay in order to get service. 

An auction is held for each L1 block, with the transactions submitted to the chain's inbox during that L1 block as the participants in the auction.

In order to maintain the strategy-proof property, the auction does not determine the order in which transactions execute. It only determines which transactions will execute. The order is determined separately, and a party's bid cannot affect the order.

## Where Congestion Fees Go

The base charge and the overhead surcharge are sufficient to cover validators' costs, so the validators don't need to receive congestion fees to make the economics work for them.  In addition, paying congestion fees to the validators would create perverse incentives for the validators, giving them a reason to cause congestion in order to increase their profit.

To prevent this, congestion fees are not paid to the validators but instead are used to pay down the overhead tally, and then to make the overhead tally negative. The result is that these funds will be used to subsidize users' transaction fees in the future by providing a negative overhead surcharge until the funds are exhausted.  

(Congestion fee revenue is not used to subsidize future congestion fees, because that would servce no purpose. If congestion fees were subsidized by, say, 20%, this would just cause clients to increase their congestion fee bids by 20%, negating the effect of the subsidy.)

## How Validators Get Paid

At the beginning of each L1 block that contains activity on a chain, any fees collected that should be payable to that chain's validators will be credited to the validators' accounts on the chain.  Validators can withdraw these funds back to L1 using the chain's normal withdrawal mechanism.