from __future__ import annotations

from typing import List, Set, TYPE_CHECKING

if TYPE_CHECKING:
    from .pet import Pet
    from .food import Food
    from .triggers import Trigger


class Effect:
    """Description of an effect that can be applied to an pet."""

    def __init__(
        self,
        trigger: Trigger | None = None,
        triggered_by: str = "",
        description: str = "",
        parameters: List[int] | None = None,
        untilEndOfBattle: bool | None = None,
        kind: str | None = None,
        target: Set[str] | dict[str, str] | None = None,
        from_pet: dict[str, str] | None = None,
        to_pet: dict[str, str] | None = None,
        n: int | List[int] | None = None,
        max_triggers: int | None = None,
        affected_pet: Pet | None = None,
        team: str | None = None,
        tier: int | None = None,
        attack: int | None = None,
        health: int | None = None,
        status: str | None = None,
        food: Food | None = None,
        damage: int | None = None,
    ) -> None:
        self.trigger = trigger
        self.triggered_by = triggered_by
        self.description = description
        self.parameters = parameters
        self.untilEndOfBattle = untilEndOfBattle
        self.kind = kind
        self.target = target
        self.n = n
        self.max_triggers = max_triggers
        self.affected_pet = affected_pet
        self.team = team
        self.from_pet = from_pet
        self.to_pet = to_pet
        self.tier = tier
        self.attack = attack
        self.health = health
        self.status = status
        self.food = food
        self.damage = damage

    @property
    def damage(self) -> int | None:
        """The damage property."""
        return self._damage

    @damage.setter
    def damage(self, value: int | None) -> None:
        self._damage = value

    @property
    def food(self) -> Food | None:
        """The food property."""
        return self._food

    @food.setter
    def food(self, value: Food | None):
        self._food = value

    @property
    def status(self) -> str | None:
        """The status property."""
        return self._status

    @status.setter
    def status(self, value: str | None):
        self._status = value

    @property
    def health(self) -> int | None:
        """The health property."""
        return self._health

    @health.setter
    def health(self, value: int | None):
        self._health = value

    @property
    def attack(self) -> int | None:
        """The attack property."""
        return self._attack

    @attack.setter
    def attack(self, value: int | None):
        self._attack = value

    @property
    def tier(self) -> int | None:
        """The tier property."""
        return self._tier

    @tier.setter
    def tier(self, value: int | None):
        self._tier = value

    @property
    def to_pet(self):
        """The to_pet property."""
        return self._to_pet

    @to_pet.setter
    def to_pet(self, value):
        self._to_pet = value

    @property
    def from_pet(self):
        """The from_pet property."""
        return self._from_pet

    @from_pet.setter
    def from_pet(self, value):
        self._from_pet = value

    @property
    def team(self) -> str | None:
        """The team property."""
        return self._team

    @team.setter
    def team(self, value: str | None):
        self._team = value

    @property
    def affected_pet(self) -> Pet | None:
        """The pet property."""
        return self._pet

    @affected_pet.setter
    def affected_pet(self, value: Pet | None):
        self._pet = value

    @property
    def triggered_by(self):
        """The triggered_by property."""
        return self._triggered_by

    @triggered_by.setter
    def triggered_by(self, value):
        self._triggered_by = value

    @property
    def untilEndOfBattle(self) -> bool | None:
        """The untilEndOfBattle property."""
        return self._untilEndOfBattle

    @untilEndOfBattle.setter
    def untilEndOfBattle(self, value: bool | None):
        self._untilEndOfBattle = value

    @property
    def kind(self) -> str | None:
        """The kind property."""
        return self._kind

    @kind.setter
    def kind(self, value: str | None):
        self._kind = value

    @property
    def target(self):
        """The target property."""
        return self._target

    @target.setter
    def target(self, value):
        self._target = value

    @property
    def trigger(self) -> Trigger | None:
        """The trigger property."""
        return self._trigger

    @trigger.setter
    def trigger(self, value: Trigger | None):
        self._trigger = value

    @property
    def description(self):
        """The description property."""
        return self._description

    @description.setter
    def description(self, value: str):
        self._description = value

    @property
    def parameters(self) -> List[int] | None:
        """The parameters property."""
        return self._parameters

    @parameters.setter
    def parameters(self, value: List[int] | None):
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
        return str({k: v for k, v in self.__dict__.items() if v})

    def __eq__(self, __o: object) -> bool:
        if not isinstance(__o, Effect):
            return False
        return self.__dict__ == __o.__dict__
