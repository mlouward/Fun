from sap.player import Player
from sap.shop import food_tier_lookup, pet_tier_lookup
from sap.pet import Pet
from sap.food import Food


def generate_all_pets():
    for _, pets in pet_tier_lookup.items():
        for pet in pets:
            Pet.create_pet(pet)


def generate_all_foods():
    for _, foods in food_tier_lookup.items():
        for food in foods:
            Food.create_food(food)


if __name__ == "__main__":

    # generate_all_foods()
    # generate_all_pets()

    #################################

    # p1 = Player("p1")
    # for _ in range(12):
    #     print(p1.shop)
    #     p1.shop.update_shop_after_battle()

    #################################

    # pet = Pet.create_pet("mosquito")
    # print(pet.get_effect_values())
    pass
