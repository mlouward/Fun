from __future__ import annotations

from typing import Dict, List, Set, Union

import pet
from food import Food
from triggers import Trigger


class Effect:
    """Description of an effect that can be applied to an pet."""

    def __init__(
        self,
        trigger: Union[Trigger, None] = None,
        triggered_by: str = "",
        description: str = "",
        parameters: Union[List[int], None] = None,
        untilEndOfBattle: Union[bool, None] = None,
        kind: Union[str, None] = None,
        target: Union[Union[Set[str], Dict[str, str]], None] = None,
        from_pet: Union[Dict[str, str], None] = None,
        to_pet: Union[Dict[str, str], None] = None,
        n: Union[int, List[int], None] = None,
        max_triggers: Union[int, None] = None,
        affected_pet: Union[pet.Pet, None] = None,
        team: Union[str, None] = None,
        tier: Union[int, None] = None,
        attack: Union[int, None] = None,
        health: Union[int, None] = None,
        status: Union[str, None] = None,
        food: Union[Food, None] = None,
        damage: Union[int, None] = None,
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
    def damage(self) -> Union[int, None]:
        """The damage property."""
        return self._damage

    @damage.setter
    def damage(self, value: Union[int, None]) -> None:
        self._damage = value

    @property
    def food(self) -> Union[Food, None]:
        """The food property."""
        return self._food

    @food.setter
    def food(self, value: Union[Food, None]):
        self._food = value

    @property
    def status(self) -> Union[str, None]:
        """The status property."""
        return self._status

    @status.setter
    def status(self, value: Union[str, None]):
        self._status = value

    @property
    def health(self) -> Union[int, None]:
        """The health property."""
        return self._health

    @health.setter
    def health(self, value: Union[int, None]):
        self._health = value

    @property
    def attack(self) -> Union[int, None]:
        """The attack property."""
        return self._attack

    @attack.setter
    def attack(self, value: Union[int, None]):
        self._attack = value

    @property
    def tier(self) -> Union[int, None]:
        """The tier property."""
        return self._tier

    @tier.setter
    def tier(self, value: Union[int, None]):
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
    def team(self) -> Union[str, None]:
        """The team property."""
        return self._team

    @team.setter
    def team(self, value: Union[str, None]):
        self._team = value

    @property
    def affected_pet(self) -> Union[pet.Pet, None]:
        """The pet property."""
        return self._pet

    @affected_pet.setter
    def affected_pet(self, value: Union[pet.Pet, None]):
        self._pet = value

    @property
    def triggered_by(self):
        """The triggered_by property."""
        return self._triggered_by

    @triggered_by.setter
    def triggered_by(self, value):
        self._triggered_by = value

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
    def trigger(self) -> Union[Trigger, None]:
        """The trigger property."""
        return self._trigger

    @trigger.setter
    def trigger(self, value: Union[Trigger, None]):
        self._trigger = value

    @property
    def description(self):
        """The description property."""
        return self._description

    @description.setter
    def description(self, value: str):
        self._description = value

    @property
    def parameters(self) -> Union[List[int], None]:
        """The parameters property."""
        return self._parameters

    @parameters.setter
    def parameters(self, value: Union[List[int], None]):
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
        return str({k: v for k, v in self.__dict__.items() if v is not None})
