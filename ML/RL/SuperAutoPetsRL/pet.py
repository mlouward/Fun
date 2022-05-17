from __future__ import annotations

from typing import List

from effect import Effect
from food import Food


class Pet:
    def __init__(
        self,
        name: str,
        damage: int,
        health: int,
        tier: int,
        effect: Effect,
        cost: int = 3,
        experience: int = 1,
        held_food: Food | None = None,
    ) -> None:
        self.name = name
        self.damage = damage
        self.health = health
        self.tier = tier
        self.effect = effect
        self.cost = cost
        self.experience = experience
        self.held_food = held_food

    @property
    def name(self) -> str:
        """The name property."""
        return self._name

    @name.setter
    def name(self, value):
        self._name = value

    @property
    def health(self) -> int:
        """The health property."""
        return self._health

    @health.setter
    def health(self, value):
        self._health = min(50, value)

    @property
    def damage(self) -> int:
        """The damage property."""
        return self._damage

    @damage.setter
    def damage(self, value):
        self._damage = min(50, value)

    @property
    def tier(self) -> int:
        """The tier property."""
        return self._tier

    @tier.setter
    def tier(self, value):
        if value not in range(1, 7):
            raise ValueError(f"Tier must be between 1 and 6, got {value}")
        self._tier = value

    @property
    def effect(self) -> Effect:
        """The effect property."""
        return self._effect

    @effect.setter
    def effect(self, value: Effect):
        if not isinstance(value, Effect):
            raise TypeError(f"Effect must be of type Effect, got {type(value)}")
        self._effect = value

    @property
    def held_food(self) -> Food | None:
        """The held_food property."""
        return self._held_food

    @held_food.setter
    def held_food(self, value: Food | None):
        if not isinstance(value, Food) and value is not None:
            raise TypeError(f"Food must be of type Food or None, got {type(value)}")
        self._held_food = value

    @property
    def cost(self) -> int:
        """The cost property."""
        return self._cost

    @cost.setter
    def cost(self, value: int):
        if value < 0 or value > 3:
            raise ValueError(f"Cost must be between 0 and 3, got {value}")
        self._cost = value

    @property
    def experience(self) -> int:
        """The experience property."""
        return self._experience

    @experience.setter
    def experience(self, value):
        if value not in range(1, 7):
            raise ValueError(f"Experience must be between 1 and 6, got {value}")
        self._experience = value

    def get_level(self) -> int:
        """
        Returns the level of the pet given its experience.
        """
        if self.experience in range(1, 7):
            return self.experience // 3 + 1
        else:
            raise RuntimeError(f"Unexpected experience value ({self.experience}")

    def combine_pets(self, other: Pet):
        """
        Checks if the two pets can be combined.
        If so, combines them into the original one.
        """
        if other.name != self.name:
            raise RuntimeError(
                f"Cannot combine pets of different types ({self.name} and {other.name}"
            )
        if other.experience == 5 or self.experience == 5:
            raise RuntimeError("Cannot combine pets with max experience")
        self.health = max(self.health, other.health) + 1
        self.damage = max(self.damage, other.damage) + 1
        self.experience += other.experience

    def get_effect_values(self) -> List[int]:
        """
        Returns the effect values of the pet given its level
        """
        if self.name.lower() in {
            "mosquito",
            "elephant",
            "giraffe",
            "turtle",
            "crocodile",
        }:
            self.effect.target.n *= self.get_level()
            return self.effect.parameters
        elif self.name.lower() == "rat":
            self.effect.n *= self.get_level()
            return self.effect.parameters
        elif self.name.lower() == "cat":
            return [1 + self.get_level()]
        elif self.name.lower() == "gorilla":
            return self.effect.max_triggers * self.get_level()
        return self.effect.parameters * self.get_level()

    def get_resell_cost(self) -> int:
        """
        Returns how much money a pet is worth to sell.
        """
        return self.get_level() + (
            self.get_effect_values()[0] if self.name.lower() == "pig" else 0
        )

    @staticmethod
    def create_pet(pet_name: str):
        pass

    def __eq__(self, __o: object) -> bool:
        return (
            isinstance(__o, Pet)
            and self.name == __o.name
            and self.tier == __o.tier
            and self.experience == __o.experience
            and self.health == __o.health
            and self.damage == __o.damage
            and self.effect == __o.effect
            and self.held_food == __o.held_food
        )

    def __str__(self) -> str:
        return (
            f"Name: {self.name} - {self.health}/{self.damage} - {self.experience}xp"
            f" (level {self.get_level()})\nTier: {self.tier} - effect: {self.effect} -"
            f" food:{self.held_food}"
        )
