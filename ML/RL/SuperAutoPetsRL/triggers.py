from enum import Enum


class Trigger(Enum):
    """Docstring for Triggers."""

    Buy = 1
    BuyFriend = 2
    Summoned = 3
    FriendSummoned = 4
    SellPet = 5
    FriendSold = 6
    LevelUp = 7
    BuyFood = 8
    EatFood = 9
    FriendEatsFood = 10
    StartOfTurn = 11
    StartOfBattle = 12
    BeforeAttack = 13
    FriendAheadAttacks = 14
    FriendAheadFaints = 15
    FriendFaints = 16
    Hurt = 17
    Faint = 18
    Knockout = 19
    EndOfTurn = 20
    BuyTier1Pet = 21
    CastsAbility = 22
