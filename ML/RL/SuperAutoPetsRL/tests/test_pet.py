import pytest
from ..sap.pet import Pet
from ..sap.shop import food_tier_lookup, pet_tier_lookup

pets_strings = [
    pet_name
    for tier_list_names in pet_tier_lookup.values()
    for pet_name in tier_list_names
]


def test_get_level():
    print(pets_strings)
    """Experience is always between 1 and 6."""
    pet = Pet("test", 1, 1, 1)
    assert pet.get_level() == 1
    pet.experience = 4
    assert pet.get_level() == 2
    pet.experience = 6
    assert pet.get_level() == 3
    with pytest.raises(
        ValueError, match=r"Experience must be between 1 and 6, got \d+$"
    ):
        pet.experience = 8
    with pytest.raises(
        ValueError, match=r"Experience must be between 1 and 6, got \d+$"
    ):
        pet = Pet("test", 1, 1, 1, experience=10)
    with pytest.raises(
        ValueError, match=r"Experience must be between 1 and 6, got -?\d+$"
    ):
        pet.experience = -2398


def test_get_effect_values():
    # For these animals, parameters don't change with level
    # But number of targets do
    for name in (
        "mosquito",
        "elephant",
        "giraffe",
        "turtle",
        "crocodile",
    ):
        print(name)
        pet = Pet.create_pet(name)
        lvl1_params = pet.get_effect_values()
        pet.experience = 4
        assert pet.get_effect_values() == lvl1_params
        assert pet.pet_effect.target.get("n") == 2
        pet.experience = 6
        assert pet.get_effect_values() == lvl1_params
        assert pet.pet_effect.target.get("n") == 3
    # nb of rats spawned proportional to level
    rat = Pet.create_pet("rat")
    assert rat.get_effect_values() == [1, 1]
    assert rat.pet_effect.n == 1
    rat.experience = 4
    assert rat.get_effect_values() == [1, 1]
    assert rat.pet_effect.n == 2
    rat.experience = 6
    assert rat.get_effect_values() == [1, 1]
    assert rat.pet_effect.n == 3
    # cat effect is 1 + level
    cat = Pet.create_pet("cat")
    assert cat.get_effect_values() == [2]
    cat.experience = 4
    assert cat.get_effect_values() == [3]
    cat.experience = 6
    assert cat.get_effect_values() == [4]
    # gorilla effect is max_triggers * level
    gorilla = Pet.create_pet("gorilla")
    assert gorilla.get_effect_values() is None
    assert gorilla.pet_effect.max_triggers == 1
    gorilla.experience = 4
    assert gorilla.get_effect_values() is None
    assert gorilla.pet_effect.max_triggers == 2
    gorilla.experience = 6
    assert gorilla.get_effect_values() is None
    assert gorilla.pet_effect.max_triggers == 3
    # For these animals, parameters change with level
    for pet_name in pets_strings:
        if pet_name not in {
            "mosquito",
            "elephant",
            "giraffe",
            "turtle",
            "crocodile",
            "rat",
            "cat",
            "gorilla",
            "dog",
            "ox",
            "scorpion",
        }:
            print(pet_name)
            pet = Pet.create_pet(pet_name)
            lvl1_params = pet.get_effect_values()
            assert isinstance(lvl1_params, list)
            pet.experience = 4
            assert pet.get_effect_values() == lvl1_params * 2
            pet.experience = 6
            assert pet.get_effect_values() == lvl1_params * 3
        elif pet_name == "scorpion":
            pet = Pet.create_pet(pet_name)
            lvl1_params = pet.get_effect_values()
            assert lvl1_params is None
            pet.experience = 4
            assert pet.get_effect_values() is None
            pet.experience = 6
            assert pet.get_effect_values() is None


def test_create_pet():
    for pet_name in pets_strings:
        pet = Pet.create_pet(pet_name)
        assert pet.name == pet_name
        assert pet.get_level() == 1


def test_apply_food_effect():
    pass
