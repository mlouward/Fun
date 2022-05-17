from enum import Enum
from typing import Dict, List, Union


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


class Effect:
    """Description of an effect that can be applied to an pet."""

    def __init__(
        self,
        trigger: Trigger,
        description: str,
        parameters: List[int],
        untilEndOfBattle: Union[bool, None] = None,
        kind: Union[str, None] = None,
        target: Union[Dict, None] = None,
        n: Union[int, None] = None,
        max_triggers: Union[int, None] = None,
    ) -> None:
        self.trigger = trigger
        self.description = description
        self.parameters = parameters
        self.untilEndOfBattle = untilEndOfBattle
        self.kind = kind
        self.target = target
        self.n = n
        self.max_triggers = max_triggers

    @property
    def untilEndOfBattle(self) -> Union[bool, None]:
        """The untilEndOfBattle property."""
        return self._untilEndOfBattle

    @untilEndOfBattle.setter
    def untilEndOfBattle(self, value: Union[bool, None]):
        self._untilEndOfBattle = value

    @property
    def kind(self) -> Union[str, None]:
        """The kind property."""
        return self._kind

    @kind.setter
    def kind(self, value: Union[str, None]):
        self._kind = value

    @property
    def target(self):
        """The target property."""
        return self._target

    @target.setter
    def target(self, value):
        self._target = value

    @property
    def trigger(self):
        """The trigger property."""
        return self._trigger

    @trigger.setter
    def trigger(self, value: Trigger):
        self._trigger = value

    @property
    def description(self):
        """The description property."""
        return self._description

    @description.setter
    def description(self, value: str):
        self._description = value

    @property
    def parameters(self):
        """The parameters property."""
        return self._parameters

    @parameters.setter
    def parameters(self, value: List[int]):
        self._parameters = value

    @property
    def n(self):
        """The n property."""
        return self._n

    @n.setter
    def n(self, value):
        self._n = value

    @property
    def max_triggers(self):
        """The max_triggers property."""
        return self._max_triggers

    @max_triggers.setter
    def max_triggers(self, value):
        self._max_triggers = value

    def __str__(self) -> str:
        return f"{self.trigger} {self.description} {self.parameters}"
