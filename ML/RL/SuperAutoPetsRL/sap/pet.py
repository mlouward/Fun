from __future__ import annotations

import uuid
from typing import List

import effect
from data import data
from food import Food


class Pet:
    def __init__(
        self,
        name: str,
        damage: int,
        health: int,
        tier: int,
        pet_effect: effect.Effect = effect.Effect(),
        pet_effects: List[effect.Effect] | None = None,
        cost: int = 3,
        experience: int = 1,
        held_status: str | None = None,
    ) -> None:
        self.id = uuid.uuid4().hex
        self.name = name
        self.damage = damage
        self.health = health
        self.tier = tier
        self.pet_effect = pet_effect
        self.cost = cost
        self.experience = experience
        self.held_status = held_status
        self.pet_effects = pet_effects

    @property
    def id(self):
        """The id property."""
        return self._id

    @id.setter
    def id(self, value):
        self._id = value

    @property
    def pet_effects(self):
        """The pet_effects property."""
        return self._pet_effects

    @pet_effects.setter
    def pet_effects(self, value):
        self._pet_effects = value

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
    def pet_effect(self) -> effect.Effect:
        """The effect property."""
        return self._effect

    @pet_effect.setter
    def pet_effect(self, value: effect.Effect):
        if not isinstance(value, effect.Effect):
            raise TypeError(f"Effect must be of type Effect, got {type(value)}")
        self._effect = value

    @property
    def held_status(self) -> str | None:
        """The held_status property."""
        return self._held_status

    @held_status.setter
    def held_status(self, value: str | None):
        # if not isinstance(value, Food) and value is not None:
        #     raise TypeError(f"Food must be of type Food or None, got {type(value)}")
        self._held_status = value

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

    def get_effect_values(self) -> List[int] | None:
        """
        Returns the effect values of the pet given its level
        """
        if not self.pet_effect:
            raise RuntimeError("Pet has no effect")
        if self.name.lower() in {
            "mosquito",
            "elephant",
            "giraffe",
            "turtle",
            "crocodile",
        }:
            self.pet_effect.target.n *= self.get_level()
            return self.pet_effect.parameters
        elif self.name.lower() == "rat":
            self.pet_effect.n *= self.get_level()
            return self.pet_effect.parameters
        elif self.name.lower() == "cat":
            return [1 + self.get_level()]
        elif self.name.lower() == "gorilla":
            return self.pet_effect.max_triggers * self.get_level()
        if self.pet_effect.parameters:
            return self.pet_effect.parameters * self.get_level()
        raise RuntimeError(f"Unrecognized pet: '{self.name}'")

    @staticmethod
    def create_pet(pet_name: str) -> Pet | None:
        """
        Initializes a pet with its stats and effect.

            :param str pet_name: The name of the pet to initialize
            :return: The initialized pet
            :rtype: Pet
            :raises ValueError: If the pet name is not in the data
            :raises RuntimeError: If the pet effects could not be set
        """
        if not pet_name.lower() in data["pets"]:
            raise ValueError(f"Unknown pet name: '{pet_name}'")
        pet_data = data["pets"][pet_name.lower()]
        pet = Pet(
            pet_name.lower(),
            pet_data["baseAttack"],
            pet_data["baseHealth"],
            pet_data["tier"],
            effect.Effect(
                pet_data["ability"]["trigger"],
                pet_data["ability"]["triggeredBy"],
                pet_data["ability"]["description"],
                # Can be None if does not scale with level
                pet_data["parameters"] if "parameters" in pet_data else None,
            ),
            held_status=pet_data["heldStatus"] if "heldStatus" in pet_data else None,
        )

        # TODO move to "on_trigger"
        # # handle multiple effect options (ox, dog, ...)
        # if pet_data["effect"]["kind"] == "oneOf":
        #     chosen_effect = {"effect": rd.choice(pet_data["effect"]["effects"])}
        #     Pet.__set_effect_data(pet, chosen_effect)
        # elif pet_data["effect"]["kind"] == "allOf":
        #     for unique_effect in pet_data["effect"]["effects"]:
        #         Pet.__set_effect_data(pet, unique_effect)

        # handle no effect (scorpion)
        if "effect" not in pet_data:
            return pet
        else:
            # "Normal" animal effect to set
            Pet.__set_effect_data(pet, pet_data)
        if pet.pet_effect:
            return pet
        raise RuntimeError(f"Could not set effect data for: '{pet_name}'")

    @staticmethod
    def __set_effect_data(pet: Pet, pet_data: dict) -> None:
        """
        Sets the appropriate effect fields for the pet.
            :param Pet pet: The pet to set the effect for
            :param dict pet_data: The pet data to use
        """
        pet.pet_effect.kind = pet_data["effect"]["kind"]
        if pet_data["effect"]["kind"] == "modifyStats":
            pet.pet_effect.target = pet_data["effect"]["target"]
            pet.pet_effect.untilEndOfBattle = pet_data["effect"]["untilEndOfBattle"]
        elif pet_data["effect"]["kind"] == "summonPet":
            pet.pet_effect.affected_pet = pet_data["effect"]["pet"]
            pet.pet_effect.n = pet_data["effect"]["n"]
            pet.pet_effect.team = pet_data["effect"]["team"]
        elif pet_data["effect"]["kind"] in (
            "dealDamage",
            "reduceHealth",
            "swallow",
            "repeatAbility",
        ):
            # Ex: crocodile, damage is fixed by level
            if "damage" in pet_data["effect"]:
                pet.pet_effect.damage = pet_data["effect"]["damage"]
            pet.pet_effect.target = pet_data["effect"]["target"]
        elif pet_data["effect"]["kind"] in ("transferStats", "transferAbility"):
            pet.pet_effect.from_pet = pet_data["effect"]["from"]
            pet.pet_effect.to_pet = pet_data["effect"]["to"]
        elif pet_data["effect"]["kind"] == "summonRandomPet":
            pet.pet_effect.tier = pet_data["effect"]["tier"]
            pet.pet_effect.attack = pet_data["effect"]["attack"]
            pet.pet_effect.health = pet_data["effect"]["health"]
        elif pet_data["effect"]["kind"] == "applyStatus":
            pet.pet_effect.target = pet_data["effect"]["target"]
            pet.pet_effect.status = pet_data["effect"]["status"]
        elif pet_data["effect"]["kind"] in (
            "discountFood",
            "foodMultiplier",
            "gainGold",
        ):
            # nothing specific to do, added for completeness
            pass
        elif pet_data["effect"]["kind"] == "refillShops":
            pet.pet_effect.food = pet_data["effect"]["food"]
        elif pet_data["effect"]["kind"] in ("oneOf", "allOf"):
            pet.pet_effects = pet_data["effect"]["effects"]
        else:
            raise ValueError(f"Unknown pet effect: '{pet_data['effect']['kind']}'")

    def apply_food_effect(self, food: Food) -> None:
        """
        Applies the food effect to the pet.

            :param Food food: The food to apply
        """
        pass

    def __eq__(self, __o: object) -> bool:
        return isinstance(__o, Pet) and self.__dict__ == __o.__dict__

    def __str__(self) -> str:
        return (
            f"Name: {self.name} - {self.damage}/{self.health} - {self.experience}xp"
            f" (level {self.get_level()}) Tier: {self.tier} -"
            f" food:{self.held_status}\nEffect: {self.pet_effect}\n"
        )
