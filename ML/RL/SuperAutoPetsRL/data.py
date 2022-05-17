from tkinter import N
from effect import Trigger

data = {
    "pets": [
        {
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
                    "target": {"kind": "RandomFriend", "n": 1},
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
                    "target": {"kind": "RandomFriend", "n": 2},
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
                    "target": {"kind": "EachShopAnimal", "includingFuture": False},
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
                    "target": {"kind": "EachFriend"},
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
                    "target": {"kind": "TriggeringEntity"},
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
                    "target": {"kind": "RandomEnemy", "n": 1},
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
                    "target": {"kind": "RandomFriend", "n": 1},
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
                    "kind": {"summonPet"},
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
                    "target": {"kind": "RandomFriend", "n": 1},
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
        }
    ]
}
