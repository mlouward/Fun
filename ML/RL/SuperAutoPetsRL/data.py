from triggers import Trigger

data = {
    "pets": {
        "ant": {
            "tier": 1,
            "baseAttack": 2,
            "baseHealth": 1,
            "ability": {
                "description": "Faint: Give a random friend +2/+1.",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [2, 1],
                "target": {"kind": "randomFriend", "n": 1},
                "untilEndOfBattle": False,
            },
        },
        "beaver": {
            "tier": 1,
            "baseAttack": 3,
            "baseHealth": 2,
            "ability": {
                "description": "Sell: Give 2 random friends +1 HP",
                "trigger": Trigger.SellPet,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [0, 1],
                "target": {"kind": "randomFriend", "n": 2},
                "untilEndOfBattle": False,
            },
        },
        "cricket": {
            "tier": 1,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": "Faint: Summon a 1/1 Zombie Cricket",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "summonPet",
                "pet": "zombie_cricket",
                "parameters": [1, 1],
                "n": 1,
                "team": "friendly",
            },
        },
        "duck": {
            "tier": 1,
            "baseAttack": 2,
            "baseHealth": 3,
            "ability": {
                "description": "Sell: Give shop pets +1 HP",
                "trigger": Trigger.SellPet,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [0, 1],
                "target": {"kind": "eachShopAnimal", "includingFuture": False},
                "untilEndOfBattle": False,
            },
        },
        "fish": {
            "tier": 1,
            "baseAttack": 2,
            "baseHealth": 2,
            "ability": {
                "description": "Level-up: Give all friends +1/+1",
                "trigger": Trigger.LevelUp,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 1],
                "target": {"kind": "eachFriend"},
                "untilEndOfBattle": False,
            },
        },
        "horse": {
            "tier": 1,
            "baseAttack": 2,
            "baseHealth": 1,
            "ability": {
                "description": (
                    "Friend summoned: Give it +1 ATK until the end of battle."
                ),
                "trigger": Trigger.FriendSummoned,
                "triggeredBy": "eachFriend",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 0],
                "target": {"kind": "triggeringEntity"},
                "untilEndOfBattle": True,
            },
        },
        "mosquito": {
            "tier": 1,
            "baseAttack": 2,
            "baseHealth": 2,
            "ability": {
                "description": "Start of battle: Deal 1 dmg to 1 random enemies.",
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "dealDamage",
                "parameters": [1],
                "target": {"kind": "randomEnemy", "n": 1},
            },
        },
        "otter": {
            "tier": 1,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": "Buy: Give 1 random friend +1/+1.",
                "trigger": Trigger.Buy,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 1],
                "target": {"kind": "randomFriend", "n": 1},
                "untilEndOfBattle": False,
            },
        },
        "pig": {
            "tier": 1,
            "baseAttack": 4,
            "baseHealth": 1,
            "ability": {
                "description": "Sell: Gain 1 gold.",
                "trigger": Trigger.SellPet,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "gainGold",
                "parameters": [1],
            },
        },
        "crab": {
            "tier": 2,
            "baseAttack": 3,
            "baseHealth": 3,
            "ability": {
                "description": "Buy: Copy the Health of the healthiest friend.",
                "trigger": Trigger.Buy,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "transferStats",
                "parameters": [0, 0.5],  # [atk, health to copy (fraction)]
                "from": {"kind": "highestHealthFriend"},
                "to": {"kind": "self"},
            },
        },
        "dodo": {
            "tier": 2,
            "baseAttack": 2,
            "baseHealth": 3,
            "ability": {
                "description": "Start of battle: Give friend ahead 50% ATK.",
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "transferStats",
                "parameters": [0.5, 0],  # [atk, health to copy (fraction)]
                "from": {"kind": "self"},
                "to": {"kind": "friendAhead", "n": 1},
            },
        },
        "elephant": {
            "tier": 2,
            "baseAttack": 3,
            "baseHealth": 5,
            "ability": {
                "description": "Before Attack: Deal 1 damage to 1 friend behind.",
                "trigger": Trigger.BeforeAttack,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "dealDamage",
                "parameters": [1],  # amount of dmg
                "target": {"kind": "friendBehind", "n": 1},
            },
        },
        "flamingo": {
            "tier": 2,
            "baseAttack": 3,
            "baseHealth": 1,
            "ability": {
                "description": "Faint: Give the two friends behind (+1/+1).",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 1],  # buff amount
                "target": {"kind": "friendBehind", "n": 2},
                "untilEndOfBattle": False,
            },
        },
        "hedgehog": {
            "tier": 2,
            "baseAttack": 3,
            "baseHealth": 2,
            "ability": {
                "description": "Faint: Deal 2 dmg to all.",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "dealDamage",
                "parameters": [2],  # amount of dmg
                "target": {"kind": "all"},
            },
        },
        "peacock": {
            "tier": 2,
            "baseAttack": 3,
            "baseHealth": 5,
            "ability": {
                "description": r"Hurt: Gain 50% of ATK 1 time.",
                "trigger": Trigger.Hurt,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [4, 0],  # [atk, health to gain]
                "target": {"kind": "self"},
                "untilEndOfBattle": False,
            },
        },
        "rat": {
            "tier": 2,
            "baseAttack": 4,
            "baseHealth": 5,
            "ability": {
                "description": (
                    "Faint: Summon 1 1/1 Dirty Rat up front for the opponent."
                ),
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "summonPet",
                "pet": "dirty_rat",
                "n": 1,
                "parameters": [1, 1],
                "team": "enemy",
            },
        },
        "shrimp": {
            "tier": 2,
            "baseAttack": 2,
            "baseHealth": 3,
            "ability": {
                "description": "Friend sold: Give a random friend +1 HP.",
                "trigger": Trigger.FriendSold,
                "triggeredBy": "eachFriend",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [0, 1],
                "target": {"kind": "randomFriend", "n": 1},
                "untilEndOfBattle": False,
            },
        },
        "spider": {
            "tier": 2,
            "baseAttack": 2,
            "baseHealth": 3,
            "ability": {
                "description": "Faint: Summon a level 1 tier 3 animal as a 2/2",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "summonRandomPet",
                "parameters": [1],  # level
                "tier": 3,
                "attack": 2,
                "health": 2,
            },
        },
        "swan": {
            "tier": 2,
            "baseAttack": 1,
            "baseHealth": 3,
            "ability": {
                "description": "Start of turn: Gain 1/2/3 gold.",
                "trigger": Trigger.StartOfTurn,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "gainGold",
                "parameters": [1],  # level
            },
        },
        "badger": {
            "tier": 3,
            "baseAttack": 5,
            "baseHealth": 4,
            "ability": {
                "description": "Faint: Deal 50%/100%/150% ATK to adjacent pets.",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "dealDamage",
                "parameters": [0.5],  # percentage of atk
                "target": {"kind": "adjacentPets"},
            },
        },
        "blowfish": {
            "tier": 3,
            "baseAttack": 5,
            "baseHealth": 4,
            "ability": {
                "description": "Hurt: Deal 2/4/6 damage to a random enemy.",
                "trigger": Trigger.Hurt,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "dealDamage",
                "parameters": [2],
                "target": {"kind": "randomEnemy", "n": 1},
            },
        },
        "camel": {
            "tier": 3,
            "baseAttack": 2,
            "baseHealth": 5,
            "ability": {
                "description": "Hurt: Give friend behind (+1/+2)/(+2/+4)/(+3/+6).",
                "trigger": Trigger.Hurt,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 2],
                "target": {"kind": "friendBehind", "n": 1},
                "untilEndOfBattle": False,
            },
        },
        "dog": {
            "tier": 3,
            "baseAttack": 2,
            "baseHealth": 2,
            "ability": {
                "description": "Friend summoned: Gain +1/+2/+3 ATK or HP.",
                "trigger": Trigger.FriendSummoned,
                "triggeredBy": "eachFriend",
            },
            "effect": {
                "kind": "oneOf",
                "effects": [
                    {
                        "kind": "modifyStats",
                        "untilEndOfBattle": False,
                        "target": {"kind": "self"},
                        "parameters": [1, 0],
                    },
                    {
                        "kind": "modifyStats",
                        "untilEndOfBattle": False,
                        "target": {"kind": "self"},
                        "parameters": [0, 1],
                    },
                ],
            },
        },
        "giraffe": {
            "tier": 3,
            "baseAttack": 2,
            "baseHealth": 5,
            "ability": {
                "description": "End turn: Give 1/2/3 friends ahead +1/+1.",
                "trigger": Trigger.EndOfTurn,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 1],
                "target": {"kind": "friendAhead", "n": 1},
                "untilEndOfBattle": False,
            },
        },
        "kangaroo": {
            "tier": 3,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": "Friend ahead attacks: Gain (+2/+2)/(+4/+4)/(+6/+6).",
                "trigger": Trigger.FriendAheadAttacks,
                "triggeredBy": "friendAhead",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [2, 2],
                "target": {"kind": "self"},
                "untilEndOfBattle": False,
            },
        },
        "ox": {
            "tier": 3,
            "baseAttack": 1,
            "baseHealth": 3,
            "ability": {
                "description": (
                    "Friend ahead faints: Gain Melon Armor and +1/+2/+3 ATK."
                ),
                "trigger": Trigger.FriendAheadFaints,
                "triggeredBy": "friendAhead",
            },
            "effect": {
                "kind": "allOf",
                "effects": [
                    {
                        "kind": "applyStatus",
                        "status": "melon-armor",
                        "target": {"kind": "self"},
                    },
                    {
                        "kind": "modifyStats",
                        "target": {"kind": "self"},
                        "parameters": [1, 0],
                        "untilEndOfBattle": False,
                    },
                ],
            },
        },
        "rabbit": {
            "tier": 3,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": "Friend eats shop food: Give it +1/+2/+3 HP.",
                "trigger": Trigger.FriendEatsFood,
                "triggeredBy": "eachFriend",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [0, 1],
                "target": {"kind": "triggeringPet"},
                "untilEndOfBattle": False,
            },
        },
        "sheep": {
            "tier": 3,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": "Faint: Summon two (2/2)/(4/4)/(6/6) Rams.",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "summonPet",
                "parameters": [2, 2],
                "n": 2,
                "pet": "ram",
                "team": "friendly",
            },
        },
        "snail": {
            "tier": 3,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": (
                    "Buy: If you lost last battle, give all friends"
                    " (+1/+1)/(+2/+2)/(+3/+3)."
                ),
                "trigger": Trigger.Buy,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [1, 1],
                "target": {"kind": "eachFriend"},
                "untilEndOfBattle": False,
            },
        },
        "turtle": {
            "tier": 3,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": "Faint: Give 1/2/3 friends behind Melon Armor.",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "applyStatus",
                "status": "melon-armor",
                "target": {"kind": "friendBehind", "n": 1},
            },
        },
        "bison": {
            "tier": 4,
            "baseAttack": 4,
            "baseHealth": 4,
            "ability": {
                "description": (
                    "End turn: If there's at least one level 3 friend, gain"
                    " (+2/+2)/(+4/+4)/(+6/+6)."
                ),
                "trigger": Trigger.EndOfTurn,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "self"},
                "parameters": [2, 2],
                "untilEndOfBattle": False,
            },
        },
        "deer": {
            "tier": 4,
            "baseAttack": 1,
            "baseHealth": 1,
            "ability": {
                "description": (
                    "Faint: Summon a (5/5)/(10/10)/(15/15) Bus with Splash Attack."
                ),
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "summonPet",
                "pet": "bus",
                "parameters": [5, 5],
                "n": 1,
                "team": "friendly",
            },
        },
        "dolphin": {
            "tier": 4,
            "baseAttack": 4,
            "baseHealth": 6,
            "ability": {
                "description": (
                    "Start of battle: Deal 5/10/15 dmg to the lowest health enemy."
                ),
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "dealDamage",
                "target": {"kind": "lowestHealthEnemy"},
                "parameters": [5],
            },
        },
        "hippo": {
            "tier": 4,
            "baseAttack": 4,
            "baseHealth": 7,
            "ability": {
                "description": "Knockout: Gain (+3/+3)/(+6/+6)/(+9/+9).",
                "trigger": Trigger.Knockout,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "self"},
                "parameters": [3, 3],
                "untilEndOfBattle": False,
            },
        },
        "parrot": {
            "tier": 4,
            "baseAttack": 5,
            "baseHealth": 3,
            "ability": {
                "description": (
                    "End turn: Copy ability from friend ahead as lvl 1/2/3 until"
                    " the end of battle."
                ),
                "trigger": Trigger.EndOfTurn,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "transferAbility",
                "from": {"kind": "friendAhead", "n": 1},
                "to": {"kind": "self"},
                "parameters": [1],
            },
        },
        "penguin": {
            "tier": 4,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": (
                    "End turn: Give three lvl 2 and 3 friends (+1/+1)/(+2/+2)/(+3/+3)."
                ),
                "trigger": Trigger.EndOfTurn,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "level2and3Friends", "n": 3},
                "parameters": [1, 1],
                "untilEndOfBattle": False,
            },
        },
        "rooster": {
            "tier": 4,
            "baseAttack": 5,
            "baseHealth": 3,
            "ability": {
                "description": "Faint: Summon 1/2/3 chicks with 1 HP and half ATK.",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "summonPet",
                "pet": "chick",
                "n": [1, 2, 3],
                "parameters": [0.5, 1],
                "team": "friendly",
            },
        },
        "skunk": {
            "tier": 4,
            "baseAttack": 3,
            "baseHealth": 6,
            "ability": {
                "description": (
                    "Start of battle: Reduce HP of the highest enemy by 33%/66%/99%."
                ),
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "reduceHealth",
                "target": {"kind": "highestHealthEnemy"},
                "parameters": [0.33],
            },
        },
        "squirrel": {
            "tier": 4,
            "baseAttack": 2,
            "baseHealth": 5,
            "ability": {
                "description": "Start of turn: Discount shop food by 1/2/3 gold.",
                "trigger": Trigger.StartOfTurn,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "discountFood",
                "parameters": [1],
            },
        },
        "whale": {
            "tier": 4,
            "baseAttack": 3,
            "baseHealth": 8,
            "ability": {
                "description": (
                    "Start of battle: Swallow friend ahead and release it as lvl"
                    " 1/2/3 after fainting."
                ),
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "swallow",
                "target": {"kind": "friendAhead", "n": 1},
                "parameters": [1],
            },
        },
        "worm": {
            "tier": 4,
            "baseAttack": 3,
            "baseHealth": 3,
            "ability": {
                "description": "Eats shop food: Gain (+1/+1)/(+2/+2)/(+3/+3).",
                "trigger": Trigger.EatFood,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "self"},
                "parameters": [1, 1],
                "untilEndOfBattle": False,
            },
        },
        "cow": {
            "tier": 5,
            "baseAttack": 3,
            "baseHealth": 3,
            "ability": {
                "description": (
                    "Buy: Replace food shop with free milk that gives"
                    " (+1/+2)/(+2/+4)/(+3/+6)."
                ),
                "trigger": Trigger.Buy,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "refillShops",
                "parameters": [1, 2],
                "shop": "food",
                "food": "milk",
            },
        },
        "crocodile": {
            "tier": 5,
            "baseAttack": 8,
            "baseHealth": 4,
            "ability": {
                "description": (
                    "Start of battle: Deal 8 damage to last enemy. Repeat 1/2/3"
                    " time(s)."
                ),
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "dealDamage",
                "damage": 8,
                "target": {"kind": "lastEnemy", "n": 1},
            },
        },
        "monkey": {
            "tier": 5,
            "baseAttack": 1,
            "baseHealth": 2,
            "ability": {
                "description": (
                    "End turn: Give right-most friend (+2/+3)/(+4/+6)/(+6/+9)."
                ),
                "trigger": Trigger.EndOfTurn,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "modifyStats",
                "parameters": [2, 3],
                "target": {"kind": "rightMostFriend"},
                "untilEndOfBattle": False,
            },
        },
        "rhino": {
            "tier": 5,
            "baseAttack": 5,
            "baseHealth": 8,
            "ability": {
                "description": (
                    "Knockout: Deal 4/8/12 dmg to first enemy. Double against Tier"
                    " 1 pets."
                ),
                "trigger": Trigger.Knockout,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "dealDamage",
                "parameters": [4],
                "target": {"kind": "firstEnemy"},
            },
        },
        "scorpion": {
            "tier": 5,
            "baseAttack": 1,
            "baseHealth": 1,
            "ability": {
                "description": "Summoned: Gain Peanuts.",
                "trigger": Trigger.Summoned,
                "triggeredBy": "self",
            },
            "heldFood": "peanuts",
        },
        "seal": {
            "tier": 5,
            "baseAttack": 3,
            "baseHealth": 8,
            "ability": {
                "description": (
                    "Eats shop food: Give 2 random friends (+1/+1)/(+2/+2)/(+3/+3)."
                ),
                "trigger": Trigger.EatFood,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "randomFriend", "n": 2},
                "parameters": [1, 1],
                "untilEndOfBattle": False,
            },
        },
        "shark": {
            "tier": 5,
            "baseAttack": 4,
            "baseHealth": 4,
            "ability": {
                "description": "Friend faints: Gain (+2/+1)/(+4/+2)/(+6/+3).",
                "trigger": Trigger.FriendFaints,
                "triggeredBy": "eachFriend",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "self"},
                "parameters": [2, 1],
                "untilEndOfBattle": False,
            },
        },
        "turkey": {
            "tier": 5,
            "baseAttack": 3,
            "baseHealth": 4,
            "ability": {
                "description": "Friend summoned: Give it (+2/+3)/(+4/+6)/(+6/+9).",
                "trigger": Trigger.FriendSummoned,
                "triggeredBy": "eachFriend",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "triggeringEntity"},
                "parameters": [2, 3],
                "untilEndOfBattle": False,
            },
        },
        "boar": {
            "tier": 6,
            "baseAttack": 8,
            "baseHealth": 6,
            "ability": {
                "description": "Before attack: Gain (+4/+2)/(+8/+4)/(+12/+6).",
                "trigger": Trigger.BeforeAttack,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "self"},
                "parameters": [4, 2],
                "untilEndOfBattle": False,
            },
        },
        "cat": {
            "tier": 6,
            "baseAttack": 4,
            "baseHealth": 5,
            "ability": {
                "description": "Multiplies the HP and ATK effect of food by 2/3/4.",
                "trigger": Trigger.BuyFood,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "foodMultiplier",
                "parameters": [2],
            },
        },
        "dragon": {
            "tier": 6,
            "baseAttack": 6,
            "baseHealth": 8,
            "ability": {
                "description": (
                    "Buy tier 1 pet: Give all pets except itself"
                    " (+1/+1)/(+2/+2)/(+3/+3)."
                ),
                "trigger": Trigger.BuyTier1Pet,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "eachFriend"},
                "parameters": [1, 1],
                "untilEndOfBattle": False,
            },
        },
        "fly": {
            "tier": 6,
            "baseAttack": 5,
            "baseHealth": 5,
            "ability": {
                "description": (
                    "Friend faints: Summon a (5/5)/(10/10)/(15/15) Zombie Fly in"
                    " its place (thrice per battle)."
                ),
                "trigger": Trigger.FriendFaints,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "summonPet",
                "parameters": [5, 5],
                "pet": "zombie_fly",
                "n": 1,
                "team": "friendly",
            },
            "maxTriggers": 3,
        },
        "gorilla": {
            "tier": 6,
            "baseAttack": 6,
            "baseHealth": 9,
            "ability": {
                "description": "Hurt: Gain Coconut Shield (1/2/3 times per battle).",
                "trigger": Trigger.Hurt,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "applyStatus",
                "status": "coconut_shield",
                "target": {"kind": "self"},
            },
            "maxTriggers": 1,
        },
        "leopard": {
            "tier": 6,
            "baseAttack": 10,
            "baseHealth": 4,
            "ability": {
                "description": (
                    "Start of battle: Deal 50% ATK dmg to 1/2/3 random enemies."
                ),
                "trigger": Trigger.StartOfBattle,
                "triggeredBy": "player",
            },
            "effect": {
                "kind": "dealDamage",
                "target": {"kind": "randomEnemy", "n": 1},
                "parameters": [0.5],
            },
        },
        "mammoth": {
            "tier": 6,
            "baseAttack": 3,
            "baseHealth": 10,
            "ability": {
                "description": "Faint: Give all friends (+2/+2)/(+4/+4)/(+6/+6).",
                "trigger": Trigger.Faint,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "modifyStats",
                "target": {"kind": "eachFriend"},
                "parameters": [2, 2],
                "untilEndOfBattle": False,
            },
        },
        "snake": {
            "tier": 6,
            "baseAttack": 6,
            "baseHealth": 6,
            "ability": {
                "description": (
                    "Friend ahead attacks: Deal 5/10/15 damage to a random enemy."
                ),
                "trigger": Trigger.FriendAheadAttacks,
                "triggeredBy": "self",
            },
            "effect": {
                "kind": "dealDamage",
                "target": {"kind": "randomEnemy", "n": 1},
                "parameters": [5],
            },
        },
        "tiger": {
            "tier": 6,
            "baseAttack": 4,
            "baseHealth": 3,
            "ability": {
                "description": (
                    "Friend ahead repeats their ability in battle as if they were"
                    " level 1/2/3."
                ),
                "trigger": Trigger.CastsAbility,
                "triggeredBy": "friendAhead",
            },
            "effect": {
                "kind": "repeatAbility",
                "target": {"kind": "triggeringEntity"},
                "parameters": [1],
            },
        },
    }
}
