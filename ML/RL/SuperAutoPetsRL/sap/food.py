from __future__ import annotations

import effect
from data import data


class Food:
    def __init__(
        self,
        food_name: str | None = None,
        ability: effect.Effect | None = None,
        cost: int = 3,
    ) -> None:
        self.food_name = food_name
        self.ability = ability
        self.cost = cost

    @property
    def food_name(self) -> str | None:
        """The food_name property."""
        return self._food_name

    @food_name.setter
    def food_name(self, value: str | None):
        self._food_name = value

    @property
    def tier(self) -> int | None:
        """The tier property."""
        return self._tier

    @tier.setter
    def tier(self, value: int | None):
        self._tier = value

    @property
    def ability(self) -> effect.Effect | None:
        """The ability property."""
        return self._ability

    @ability.setter
    def ability(self, value: effect.Effect | None):
        self._ability = value

    @property
    def cost(self) -> int:
        """The cost property."""
        return self._cost

    @cost.setter
    def cost(self, value: int):
        self._cost = value

    @staticmethod
    def create_food(food_name: str):
        """
        Initializes a food with its effect.

            :param str food_name: The name of the food to initialize
            :return: The initialized food
            :rtype: Food
            :raises ValueError: If the food name is not in the data
        """
        if not food_name.lower() in data["foods"]:
            raise ValueError(f"Unknown food name: '{food_name}'")
        food_data = data["foods"][food_name.lower()]
        effect_kind = food_data["ability"]["effect"]["kind"]
        food = Food(
            food_name=food_name.lower(),
            cost=food_data["cost"] if "cost" in food_data else 3,
            ability=effect.Effect(
                trigger=food_data["ability"]["trigger"],
                triggered_by=food_data["ability"]["triggeredBy"],
                kind=effect_kind,
            ),
        )
        if food.ability:
            if effect_kind == "modifyStats":
                food.ability.target = food_data["ability"]["effect"]["target"]
                food.ability.parameters = food_data["ability"]["effect"]["parameters"]
                food.ability.untilEndOfBattle = food_data["ability"]["effect"][
                    "untilEndOfBattle"
                ]
            elif effect_kind == "applyStatus":
                food.ability.to_pet = food_data["ability"]["effect"]["to"]
                food.ability.status = food_data["ability"]["effect"]["status"]
            elif effect_kind == "faint":
                food.ability.target = food_data["ability"]["effect"]["target"]
            elif effect_kind == "gainExperience":
                food.ability.target = food_data["ability"]["effect"]["target"]
                food.ability.parameters = food_data["ability"]["effect"]["parameters"]
        return food

    def __str__(self) -> str:
        return (
            f"Name: {self.food_name} - Cost: {self.cost}\nEffect: {str(self.ability)}"
        )
