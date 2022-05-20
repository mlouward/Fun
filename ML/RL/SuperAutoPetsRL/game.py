from player import Player
from pet import Pet

if __name__ == "__main__":
    p1 = Player("p1")
    p1.board = [
        Pet.create_pet("ant"),
        Pet.create_pet("ant"),
        None,
    ]
    p1.shop.shop_slots_pets[0] = Pet.create_pet("ant")
    for pet in p1.shop.shop_slots_pets:
        if pet and p1.board[0]:
            if pet.name == "ant":
                p1.combine_pets(p1.board[0], pet, from_shop=True)
    p1.combine_pets(p1.board[0], p1.board[1], from_shop=False)
    print(p1)
    print(p1.board[0])
