from __future__ import annotations

from argparse import ArgumentParser
from typing import Dict, List, Tuple

from powerups import data


def autoclicks_formula(final_level: int):
    """
    Returns the cost to upgrade to final_level.
    """
    return 1.3 ** (final_level - 1) * data["AutoClick"]["base_cost"]


def powerups_formula(final_level: int, powerup_name: str):
    """
    Returns the cost to upgrade to final_level.
    """
    return 1.4 ** (final_level - 1) * data[powerup_name]["base_cost"]


def one_upgrade(verbose: int = 2) -> Tuple[str, float]:
    """
    Prints the most cost effective upgrade, and returns
    the name of this upgrade.
    Parameters:
    verbose: 0: no output,
             1: print the best upgrade only,
             2: print all upgrades and their value
    """
    best_cost = float("inf")
    best_key = ""
    # When 2 powerups in a row are zero level, we can't access them
    zero_level_tracker = 0
    if verbose > 1:
        print(f"| {'name':20} | {'cps_cost':20} |")
    # Go through all powerups
    for upgrade_name in data.keys():
        if zero_level_tracker == 2:
            # When 2 powerups in a row are zero level, we can't access them
            # Debug mode only (>2)
            if verbose > 2:
                print("You can't afford more powerups.")
            break
        if data[upgrade_name]["levels"] == 0:
            zero_level_tracker += 1
            continue
        # Compute the cost of the upgrade
        upgrade_cost = powerups_formula(data[upgrade_name]["levels"] + 1, upgrade_name)
        # Compute ration between cost and gained CPS
        cps_cost = upgrade_cost / data[upgrade_name]["cps"]
        if min(best_cost, cps_cost) == cps_cost:
            best_cost = cps_cost
            best_key = upgrade_name
        if verbose > 1:
            print(f"| {upgrade_name:20} | {cps_cost:20.0f} |")
    if verbose > 0:
        # Print worthiest upgrade
        print(f"Best cost: {best_key}: {best_cost}")
    return best_key, best_cost


def chain_upgrades(n: int = 20):
    # Contains upgrade name and their value
    upgrades_list: List[Tuple[str, float]] = []
    while n > 0:
        upgrade_key, upgrade_value = one_upgrade(verbose=0)
        data[upgrade_key]["levels"] += 1
        upgrades_list.append((upgrade_key, upgrade_value))
        n -= 1
    print(
        "Upgrades order: \n- {}".format(
            "\n- ".join(
                [
                    f"{upgrade_name} ({upgrade_value:.0f})"
                    for upgrade_name, upgrade_value in upgrades_list
                ]
            )
        )
    )
    return upgrades_list


if __name__ == "__main__":
    # parse arguments: --one-upgrade, --chain-upgrades, --verbose, -n
    parser = ArgumentParser()
    parser.add_argument(
        "--one-upgrade",
        "-o",
        action="store_true",
        help="Prints the most cost effective upgrade",
    )
    parser.add_argument(
        "--chain-upgrades",
        "-c",
        action="store_true",
        help="Prints the most efficient order of upgrades",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        type=int,
        default=2,
        help=(
            "0: no output, 1: print the best upgrade only, 2: print all upgrades and"
            " their value"
        ),
    )
    parser.add_argument(
        "-n",
        type=int,
        default=20,
        help="Number of upgrades to chain",
    )
    args = parser.parse_args()
    if args.one_upgrade:
        one_upgrade(args.verbose)
    elif args.chain_upgrades:
        chain_upgrades(args.n)
    else:
        print("No argument given.")
        parser.print_help()
        exit(1)
