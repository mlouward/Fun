from effect import Effect, Trigger
from food import Food
from pet import Pet

if __name__ == "__main__":
    ant = Pet(
        "Ant",
        1,
        2,
        1,
        Effect(Trigger.Faint, "On faint, gives +2/+1 to a random friend", [2, 1]),
    )
    print(ant)
    print(ant.get_resell_cost())

    pig = Pet(
        "Pig",
        3,
        1,
        1,
        Effect(Trigger.SellPet, "On sell, gives +1gold", [1]),
    )
    pig.experience = 5
    print(pig)
    print(pig.get_resell_cost())
