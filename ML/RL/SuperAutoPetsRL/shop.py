import random as rd
from typing import List

from data import data
from effect import Effect, Trigger
from pet import Pet

# Build dict of tiers to list of available pets
pet_tier_lookup: dict[int, List[str]] = {1: [], 2: [], 3: [], 4: [], 5: [], 6: []}
for name, pet in data["pets"].items():
    pet_tier_lookup[pet["tier"]].append(name)
print(pet_tier_lookup)
del name, pet


class Shop:
    def __init__(
        self,
        shop_size=None,
        shop_slots=[],
        turn=1,
        shop_attack=0,
        shop_health=0,
        tier=1,
    ) -> None:
        self.turn = turn
        self.tier = tier

    @property
    def shop_size(self) -> int:
        """Size of the shop."""
        return self._shop_size

    @shop_size.setter
    def shop_size(self, value: int):
        self._shop_size = value

    @property
    def shop_slots(self) -> List[Pet]:
        """List of pets available in shop."""
        return self._shop_slots

    @shop_slots.setter
    def shop_slots(self, value: List[Pet]):
        self._shop_slots = value

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

    def create_new_shop(self):
        """Create new shop."""
        self.increment_turn()
        self.generate_shop_pets()

    def increment_turn(self):
        """Increment turn by 1, updates shop tier and shop size."""
        self.turn += 1
        self.tier = min(6, (self.turn + 1) // 2)
        self.shop_size = (
            3 if self.turn in range(1, 5) else 4 if self.turn in range(5, 9) else 5
        )

    def get_random_pet(self) -> Pet:
        """Get random pet from shop."""
        # Get random pet from shop tier
        print("tier:", pet_tier_lookup[self.tier])
        pet_name = rd.choice(pet_tier_lookup[self.tier])
        # Create pet
        return Pet.create_pet(pet_name)

    def generate_shop_pet(self):
        """Generate shop pet."""
        # Check if shop is full
        if len(self.shop_slots) == self.shop_size:
            raise OverflowError("Shop is full.")
        # Get pet according to shop tier
        pet = self.get_random_pet()

    def generate_shop_pets(self):
        """Generate shop pets."""
        for _ in range(self.shop_size):
            self.generate_shop_pet()


if __name__ == "__main__":
    shop = Shop()
    shop.turn = 1
    print(shop.get_random_pet())
