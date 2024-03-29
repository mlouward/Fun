from __future__ import annotations

import random as rd
from typing import List

from .data import data
from .food import Food
from .pet import Pet

# Build dict of tiers to list of available pets
pet_tier_lookup: dict[int, List[str]] = {1: [], 2: [], 3: [], 4: [], 5: [], 6: []}
for name, pet in data["pets"].items():
    if pet["tier"] > 0:  # ignore summons
        pet_tier_lookup[pet["tier"]].append(name)
del name, pet

# Build dict of tiers to list of available foods
food_tier_lookup: dict[int, List[str]] = {1: [], 2: [], 3: [], 4: [], 5: [], 6: []}
for name, food in data["foods"].items():
    if food["tier"] > 0:  # ignore milk
        food_tier_lookup[food["tier"]].append(name)
del name, food


SHOP_SIZE = 7


class Shop:
    def __init__(
        self,
        turn=1,
        shop_attack=0,
        shop_health=0,
    ) -> None:
        """
        Initialize shop.
            :param turn: turn of shop
            :param shop_attack: bonus attack of all pets in shop
            :param shop_health: bonus health of all pets in shop
        """
        if turn < 1:
            raise ValueError("Turn must be greater than 0.")
        self.tier = 1
        self.turn = 1
        self.shop_size_pets = 3
        self.shop_size_foods = 1
        self.shop_slots_pets = [None] * self.shop_size_pets  # Initial pets in shop
        self.shop_slots_foods = [None] * self.shop_size_foods  # Initial foods in shop
        self.shop_slots_frozen = [False] * SHOP_SIZE
        for _ in range(1, turn):
            self.increment_turn()
        self.shop_attack = shop_attack
        self.shop_health = shop_health
        self.generate_shop_pets()
        self.generate_shop_foods()

    @property
    def shop_slots_frozen(self):
        """The shop_slots_frozen property."""
        return self._shop_slots_frozen

    @shop_slots_frozen.setter
    def shop_slots_frozen(self, value):
        self._shop_slots_frozen = value

    @property
    def shop_size_pets(self) -> int:
        """Size of the shop."""
        return self._shop_size_pets

    @shop_size_pets.setter
    def shop_size_pets(self, value: int):
        self._shop_size_pets = value

    @property
    def shop_size_foods(self) -> int:
        """Size of the shop."""
        return self._shop_size_foods

    @shop_size_foods.setter
    def shop_size_foods(self, value: int):
        self._shop_size_foods = value

    @property
    def shop_slots_pets(self) -> List[Pet | None]:
        """List of pets available in shop."""
        return self._shop_slots_pets

    @shop_slots_pets.setter
    def shop_slots_pets(self, value: List[Pet | None]):
        self._shop_slots_pets = value

    @property
    def shop_slots_foods(self) -> List[Food | None]:
        """List of foods available in shop."""
        return self._shop_slots_foods

    @shop_slots_foods.setter
    def shop_slots_foods(self, value: List[Food | None]):
        self._shop_slots_foods = value

    @property
    def turn(self):
        """Turn number."""
        return self._turn

    @turn.setter
    def turn(self, value):
        self._turn = value

    @property
    def shop_attack(self):
        """How many atk are added to all shop animals"""
        return self._shop_attack

    @shop_attack.setter
    def shop_attack(self, value):
        self._shop_attack = value

    @property
    def shop_health(self):
        """How many hp are added to all shop animals"""
        return self._shop_health

    @shop_health.setter
    def shop_health(self, value):
        self._shop_health = value

    @property
    def tier(self):
        """The pet tier property."""
        return self._tier

    @tier.setter
    def tier(self, value):
        self._tier = value

    def update_shop_after_battle(self):
        """Create new shop."""
        self.increment_turn()
        self.generate_shop_pets()
        self.generate_shop_foods()

    def increment_turn(self):
        """Increment turn by 1, updates shop tier and shop size."""
        self.turn += 1
        self.tier = min(6, (self.turn + 1) // 2)
        # On turn 5, shop size increases for foods
        if self.turn == 3:
            self.shop_size_foods = 2
            self.shop_slots_foods.append(None)
        # On turn 5, shop size increases for animals
        if self.turn == 5:
            self.shop_size_pets = 4
            self.shop_slots_pets.append(None)
        # On turn 9, shop size increases for animals
        elif self.turn == 9:
            self.shop_size_pets = 5
            self.shop_slots_pets.append(None)

    def get_random_pet(self, is_level_up=False) -> Pet | None:
        """Get random pet from shop."""
        # Get a pet from the current tier + 1
        if is_level_up:
            pet_tier = min(6, self.tier + 1)
        # Get random pet from tier inferior or equal to current shop tier
        else:
            pet_tier = rd.randint(1, self.tier)
        # Get random pet from tier
        pet_name = rd.choice(pet_tier_lookup[pet_tier])
        # Create pet
        new_pet = Pet.create_pet(pet_name)
        if not new_pet:
            raise ValueError("Pet is None.")
        new_pet.damage += self.shop_attack
        new_pet.health += self.shop_health
        return new_pet

    def get_random_food(self) -> Food | None:
        """Get random food from shop for the current tier."""
        food_name = rd.choice(food_tier_lookup[self.tier])
        new_food = Food.create_food(food_name)
        if not new_food:
            raise ValueError("Food is None.")
        return new_food

    def toggle_freeze_slot(self, slot: int):
        """Toggle freeze/unfreeze slot.
        :param slot: slot to freeze/unfreeze (from 1 to 7)
        :return: None
        :raises: IndexError if slot is out of range
        """
        # Check that we are not trying to freeze unavailable slots
        if slot in range(1, self.shop_size_pets + 1) or slot in range(
            8 - self.shop_size_foods, 8
        ):
            self.shop_slots_frozen[slot - 1] ^= 1
        else:
            raise IndexError(f"Slot {slot} is not available.")

    def generate_shop_pets(self):
        """Generate shop pets."""
        for i in range(self.shop_size_pets):
            # Check if the slot is not frozen before rerolling
            if not self.shop_slots_frozen[i]:
                self.shop_slots_pets[i] = self.get_random_pet()

    def generate_shop_foods(self):
        """Generate shop foods."""
        for i in range(self.shop_size_foods):
            # Check if the slot is not frozen before rerolling
            if not self.shop_slots_frozen[SHOP_SIZE - self.shop_size_foods + i]:
                self.shop_slots_foods[i] = self.get_random_food()

    def apply_food_effect(self, food: Food):
        """
        Apply food effect to all pets.
            :param Food food: food to apply
        """
        pass

    def __str__(self) -> str:
        animals_str = ""
        for pet in self.shop_slots_pets:
            if pet:
                animals_str += f"- {pet}\n"
        foods_str = ""
        for food in self.shop_slots_foods:
            if food:
                foods_str += f"- {str(food)}\n"
        return (
            f"Shop: Turn {self.turn}, {self.shop_size_pets} slots,"
            f" +{self.shop_attack} atk, +{self.shop_health} hp, tier {self.tier}\n"
            f"Animals:\n{animals_str}Foods:\n{foods_str}"
        )
