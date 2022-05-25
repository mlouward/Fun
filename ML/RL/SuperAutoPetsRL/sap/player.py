from __future__ import annotations

from typing import List

from pet import Pet
from food import Food
from shop import Shop


class Player:
    def __init__(
        self,
        name: str | None = None,
        money: int = 10,
        hp: int = 10,
        wins: int = 0,
        shop: Shop = Shop(),
        board_pets: List[Pet | None] = [None] * 5,
        board_foods: List[Food | None] = [None] * 2,
    ) -> None:
        """
        Initialize player.
            :param name: name of player
            :param money: money of player
            :param hp: health of player
            :param shop: shop of player
        """
        self.name = name
        self.money = money
        self.hp = hp
        self.wins = wins
        self.shop = shop
        self.board_pets = board_pets
        self.board_foods = board_foods

    @property
    def wins(self) -> int:
        """The wins property."""
        return self._wins

    @wins.setter
    def wins(self, value: int):
        self._wins = value

    @property
    def board_pets(self) -> List[Pet | None]:
        """The board property."""
        return self._board_pets

    @board_pets.setter
    def board_pets(self, value: List[Pet | None]):
        self._board_pets = value

    @property
    def board_foods(self) -> List[Food | None]:
        """The board property."""
        return self._board_foods

    @board_foods.setter
    def board_foods(self, value: List[Food | None]):
        self._board_foods = value

    @property
    def hp(self):
        """The hp property."""
        return self._hp

    @hp.setter
    def hp(self, value):
        self._hp = value

    @property
    def name(self) -> str | None:
        """The name property."""
        return self._name

    @name.setter
    def name(self, value: str | None):
        self._name = value

    @property
    def money(self) -> int:
        """The money property."""
        return self._money

    @money.setter
    def money(self, value: int):
        self._money = value

    @property
    def shop(self) -> Shop:
        """The shop property."""
        return self._shop

    @shop.setter
    def shop(self, value: Shop):
        self._shop = value

    def buy_pet(self, pet: Pet, shop_slot: int) -> None:
        """
        Buy a pet.
            :param pet: pet to buy
            :param shop_slot: slot of the pet in the shop
        """
        # TODO: Activate triggers
        # Add pet to player's board
        if self.money >= pet.cost:
            # Insert it to the first "None" slot if there is ons
            if not all(self.board_pets):
                self.board_pets[self.board_pets.index(None)] = pet
            # otherwise, if there is a pet with the same name, combine them
            elif any([pet.name == p.name for p in self.board_pets if p is not None]):
                for p in self.board_pets:
                    if p is not None and p.name == pet.name:
                        self.combine_pets(p, pet, from_shop=True)
                        break

            # Combine pets if there is one already and board is full

            self.money -= pet.cost
            # Remove from shop and unfreeze the slot
            self.shop.shop_slots_pets[shop_slot] = None
            self.shop.shop_slots_frozen[shop_slot] = False
        else:
            raise ValueError("Not enough money")

    def sell_pet(self, pet_index: int) -> None:
        """
        Sell a pet.
            :param pet_index: index of the pet to sell
        """
        # TODO: Activate triggers
        pet = self.board_pets[pet_index]
        if pet is not None:
            self.money += pet.get_level()
            self.board_pets[pet_index] = None

    def combine_pets(
        self, original: Pet | None, other: Pet | None, from_shop=False
    ) -> bool:
        """
        Checks if the two pets can be combined.
        If so, combines them into the original one.
            :param original: original pet
            :param other: other pet
            :param from_shop: if the pet is from the shop
            :return: True if the two pets can be combined, False otherwise
        """
        if not other or not original:
            return False
        if other.name != original.name:
            print(
                f"Cannot combine pets of different types ({original.name} and"
                f" {other.name}"
            )
            return False
        if other.experience == 5 or original.experience == 5:
            print("Cannot combine pets with max experience")
            return False
        # delete the original pet from shop slot or board slot
        if from_shop:
            # Find index of bought pet in shop
            shop_pet = next(
                (
                    other
                    for p in self.shop.shop_slots_pets
                    if p is not None and other.id == p.id
                ),
                None,
            )
            if not shop_pet:
                return False
            # unfreeze slot and delete 'other' from shop
            self.shop.shop_slots_frozen[
                self.shop.shop_slots_pets.index(shop_pet)
            ] = False
            self.shop.shop_slots_pets[self.shop.shop_slots_pets.index(shop_pet)] = None
        else:
            # Find index of 'other' pet on the board
            board_pet = next(
                (other for p in self.board_pets if p is not None and other.id == p.id),
                None,
            )
            if not board_pet:
                return False
            # remove 'other' pet from board
            self.board_pets[self.board_pets.index(board_pet)] = None

        # Update stats, spawn new pet if level up
        # TODO: trigger level up
        original.health = max(original.health, other.health) + 1
        original.damage = max(original.damage, other.damage) + 1
        old_exp1 = original.experience
        old_exp2 = other.experience
        original.experience += other.experience

        # Handle level-up: add a new pet in shop of self.tier + 1
        if original.experience >= 3 and (old_exp1 < 2 and old_exp2 < 2):
            # if all shop pets slots are full, the pet overrides the first food
            # We will not override it in our case
            # TODO override ?
            self.shop.shop_slots_pets[-1] = self.shop.get_random_pet(is_level_up=True)
        if original.experience == 6 and (old_exp1 < 5 and old_exp2 < 5):
            self.shop.shop_slots_pets[-1] = self.shop.get_random_pet(is_level_up=True)

        return True

    def exchange_pets_position(self, pet_index: int, pet_index_2: int) -> None:
        """
        Exchange two pets in the board.
            :param pet_index: index of the first pet
            :param pet_index_2: index of the second pet
        """
        self.board_pets[pet_index], self.board_pets[pet_index_2] = (
            self.board_pets[pet_index_2],
            self.board_pets[pet_index],
        )

    def buy_food(
        self, food: Food, shop_slot: int, target_pet: Pet | None = None
    ) -> None:
        """
        Buy a food.
            :param Food food: food to buy
            :param int shop_slot: slot of the food in the shop
            :param Pet | None target_pet: pet of the board to feed if applicable
        """
        # Add pet to player's board
        if self.money >= food.cost:
            # Foods that apply to a pet
            if target_pet:
                Pet.apply_food_effect(target_pet, food)
            else:
                Shop.apply_food_effect(self.shop, food)
            # Foods that apply to the game/board...
            self.money -= food.cost
            # Remove from shop and unfreeze the slot
            self.shop.shop_slots_foods[shop_slot] = None
            self.shop.shop_slots_frozen[shop_slot] = False
        else:
            raise ValueError("Not enough money")

    def __str__(self) -> str:
        board = "|"
        for pet in self.board_pets:
            if not pet:
                board += "x|"
            else:
                board += f"{pet.name}|"
        for food in self.shop.shop_slots_foods:
            if food:
                board += f"{food.food_name}|"
            else:
                board += "x|"
        return f"{self.name} ; {self.money}golds ; {self.hp}/10hp.\n{board}"
