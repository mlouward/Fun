from typing import Union


class Food:
    def __init__(self, food_name: Union[str, None] = None) -> None:
        self.food_name = food_name

    @property
    def food_name(self) -> Union[str, None]:
        """The food_name property."""
        return self._food_name

    @food_name.setter
    def food_name(self, value: Union[str, None]):
        self._food_name = value

    @property
    def tier(self) -> Union[int, None]:
        """The tier property."""
        return self._tier

    @tier.setter
    def tier(self, value: Union[int, None]):
        self._tier = value

    @staticmethod
    def create_food(food_name: str):
        return Food(food_name)

    def __str__(self) -> str:
        return str({k: v for k, v in self.__dict__.items() if v is not None})
