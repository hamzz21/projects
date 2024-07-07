pragma solidity ^0.5.16;

contract SmartLockContract {
    address payable public owner;
    bool public isLocked;

    constructor() public {
        owner = msg.sender;
        isLocked = true;
    }

    function unlock() public payable {
        require(msg.value >= 1 ether, "Not enough Ether provided.");
        isLocked = false;
    }

    function lock() public {
        require(msg.sender == owner, "Only the owner can lock.");
        isLocked = true;
    }

    function withdraw() public {
        require(msg.sender == owner, "Only the owner can withdraw.");
        owner.transfer(address(this).balance);
    }
}
