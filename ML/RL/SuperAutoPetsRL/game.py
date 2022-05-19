import pet as pt
from data import data


def create_all_pets() -> dict[str, pt.Pet]:
    all_pets = {}
    for pet in data["pets"]:
        all_pets[pet] = pt.Pet.create_pet(pet)
    return all_pets


if __name__ == "__main__":
    all_pets = create_all_pets()
    for pet in all_pets.values():
        print(pet)
