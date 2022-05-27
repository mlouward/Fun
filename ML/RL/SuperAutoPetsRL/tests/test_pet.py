import pytest
from ..sap.pet import Pet


def test_get_level():
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
