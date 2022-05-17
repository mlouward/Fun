from typing import Union


class Food:
    def __init__(self) -> None:
        pass

    @property
    def tier(self):
        """The tier property."""
        return self._tier

    @tier.setter
    def tier(self, value: Union[int, None]):
        self._tier = value
