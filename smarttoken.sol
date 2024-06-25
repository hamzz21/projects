pragma solidity ^0.5.17;

contract LockToken {
    address public owner;
    mapping (address => uint256) public balanceOf;

    event Transfer(address indexed from, address indexed to, uint256 value);

    constructor(uint256 initialSupply) public {
        owner = msg.sender;
        balanceOf[msg.sender] = initialSupply;
    }

    function transfer(address _to, uint256 _value) public returns (bool success) {
        require(balanceOf[msg.sender] >= _value);
        balanceOf[msg.sender] -= _value;
        balanceOf[_to] += _value;
        emit Transfer(msg.sender, _to, _value);
        return true;
    }
}

contract SmartLockContract {
    address payable public owner;
    bool public isLocked;
    LockToken public acceptedToken;

    constructor(LockToken _acceptedToken) public {
        owner = msg.sender;
        isLocked = true;
        acceptedToken = _acceptedToken;
    }

    function unlock(uint256 amount) public {
        require(acceptedToken.transfer(address(this), amount));
        isLocked = false;
    }

    function lock() public {
        require(msg.sender == owner);
        isLocked = true;
    }

    function withdraw() public {
        require(msg.sender == owner);
        uint256 balance = acceptedToken.balanceOf(address(this));
        acceptedToken.transfer(owner, balance);
    }
}