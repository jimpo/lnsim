# lnsim

A [Lightning Network](https://lightning.network/) simulation tool. The program is designed to
assist with research on routing, fee markets, and misbehaving nodes on the Lightning Network.

This project is very much incomplete at present.

## Simulation model

Each simulation trial is self-contained, generating a network topology from scratch based on the
organic behavior of nodes sending transactions to one another and additional logic they are
programmed with. Individuals nodes determine network topology -- it is not orchestrated at a higher
level by, for example, explicitly instructing nodes to connect to specific other nodes.

The input to the simulation is a JSON specification file, which lists all nodes that will ever be
active over the course of the run, a sequence of transactions that are to occur between nodes, and
additional other signals that can be thought of as being provided externally by end users of the
network clients. For example, an additional input in the specification file lists nodes that
increase their budget to auto-connect to other nodes (eg. simulating the "autopilot" feature of
[LND](https://github.com/lightningnetwork/lnd)).

## Build

lnsim uses the [Gradle](https://gradle.org/) build system. If you don't know how to use Gradle,
neither do I, so I recommend you use [IntelliJ IDEA](https://www.jetbrains.com/idea/) and it will
do the right thing.

## Running a simulation

To run a simulation, you first need to generate a specification file. 
[txSmear](https://github.com/wrbrand/txSmear) is a project that generates input specifications
based on historical transactions from the Ethereum blockchain.

The program entrypoint is the `edu.stanford.lnsim.SimulationRunner` class.