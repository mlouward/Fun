# %%

import argparse
from typing import Literal

import pyperclip

from .movie import Movie
from .neo4j_utils.neo4j_utils import GraphDbConnector
from .person import Person

VERBOSE = 0


class Player:
    """
    A player has a name, a list of 3 bans, and a skip available.
    """

    def __init__(self) -> None:
        # role
        self._role: Literal["player", "opponent", None] = None
        # IDs of banned actors
        self._bans: list[Person] = []
        self._skip_available: bool = True

    @property
    def role(self) -> Literal["player", "opponent", None]:
        return self._role

    @role.setter
    def role(self, role: Literal["player", "opponent", None]) -> None:
        self._role = role

    @property
    def bans(self) -> list[Person]:
        return self._bans

    @bans.setter
    def bans(self, banned_names: list[str]) -> None:
        if len(banned_names) > 3:
            raise ValueError("A player can only ban 3 actors.")
        for ban in banned_names:
            # query the database to get the ID of the actor
            # add it to the bans list
            person_id = GraphDbConnector.get_person_from_name(ban)
            self._bans.append(person_id)

    @property
    def skip_available(self) -> bool:
        return self._skip_available

    def use_skip(self) -> None:
        self._skip_available = False


class Game:
    """
    The game is played against an opponent.
    Starting from a movie, the players take turns naming a movie with a link to the previous one.
    Links can be actor, director, composer or writer.
    Each link can only be used 3 times.
    Each movie can only be used once.
    If there are multiple links between two movies, they all count as one.
    If one of the links has already been used 3 times, it cannot be used again even if the other links have not been used yet.
    Each player has a list of 3 bans, that only apply to the opponent. The opponent's bans are hidden at the start of the game.
    Bans do not apply on each player's first turn.
    The game ends when a player cannot name a movie in less than 20 seconds.
    Each player can skip their turn once. If they both skip on the same movie, the game ends in a draw.
    Strategies:
    - Combine bans, lifelines, and overused links to limit the options your opponent has on their turn.
    - Try to use links that your opponent has already used 2 times, with an unpopular movie (lethal)
    - Prefer playing unpopular movies, so that your opponent has less options.
    - Play movies with unseen links, so that your opponent might use it again.
    """

    def __init__(self) -> None:
        self.connector = GraphDbConnector.get_default_connection()
        self.starting_player: Player = Player()
        self.opponent: Player = Player()
        self.current_movie: Movie  # movie_id
        self.turn: int
        self.movies_played: list[Movie]  # movie_id
        self.links_count: dict[Person, int]  # link_id: count
        self._possible_movies: list[Movie]  # movie_id
        self.game_over: bool

    @property
    def possible_movies(self) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        all_links_from_current_movie = self.connector.get_all_links_from_movie(self.current_movie)
        usable_links = self.get_usable_links(all_links_from_current_movie)
        if VERBOSE:
            print(f"Usable links: {usable_links}")

        if usable_links == []:
            self.game_over = True
            raise ValueError("No possible movies left.")

        return self.connector.get_possible_movies(
            self.current_movie, self.movies_played, usable_links
        )

    def get_possible_movies_strategy_1(self) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        all_links_from_current_movie: list[Person] = self.connector.get_all_links_from_movie(
            self.current_movie
        )
        usable_links: list[Person] = self.get_usable_links_strategy_1(all_links_from_current_movie)
        if VERBOSE:
            print(f"Usable links: {usable_links}")

        possible_movies: list[Movie] = self.connector.get_possible_movies(
            self.current_movie, self.movies_played, usable_links
        )
        if usable_links == [] or possible_movies == []:
            # fallback to default strategy
            return self.possible_movies

        return possible_movies

    def get_usable_links(self, all_links: list[Person]) -> list[Person]:
        """
        Get the usable links for the current player.

        Args:
            all_links (list[Person]): all links from the current movie

        Returns:
            list[Person]: list of usable links
        """
        #  get the links that have not been used 3 times yet
        return [link for link in all_links if self.links_count[link] < 3]

    def get_usable_links_strategy_1(self, current_movie_links: list[Person]) -> list[Person]:
        """
        Only use links never used, or used twice (try to get a killshot).

        Args:
            all_links (list[int]): all links from the current movie

        Returns:
            list[int]: list of usable links
        """
        #  get the links that have not been used 3 times yet
        return [
            link
            for link in current_movie_links
            if link not in self.links_count
            or self.links_count[link] == 0
            or self.links_count[link] == 2
        ]

    def start_game(self, current_movie: Movie, usable_links: list[Person]):
        """
        Start a new game.
        """
        self.starting_player = Player()
        self.opponent = Player()
        self.starting_player.role = "player"
        self.opponent.role = "opponent"
        self.turn = 0
        self.game_over = False
        self.current_movie = current_movie
        self.movies_played = [current_movie]
        self.links_count = dict()

    def set_initial_movie(self, movie: str, year: int) -> None:
        """
        Set the initial movie for the game.
        """
        starting_movie_id = self.connector.get_movie_from_name_and_year(movie, year)
        all_links = self.connector.get_all_links_from_movie(starting_movie_id)
        self.start_game(starting_movie_id, all_links)

    def play_turn(self, movie: Movie, used_links: list[Person]) -> None:
        """
        Play a turn. Should update the game state:
        - turn number
        - used movies
        - used links
        - current movie
        """
        self.turn += 1
        self.movies_played.append(movie)
        self.current_movie = movie
        self.update_used_links(used_links)
        if VERBOSE:
            print(f"Turn: {self.turn}")
            print(f"Movies played: {self.movies_played}")
            print(f"Links count: {self.links_count}")
            print(f"Current movie: {self.current_movie}")

    def update_used_links(self, used_links_ids: list[Person]) -> None:
        for link_id in used_links_ids:
            if link_id in self.links_count:
                self.links_count[link_id] += 1
            else:
                self.links_count[link_id] = 1


# %%
def show_next_choices(movie_choices: list[Movie], choice: int | None = None) -> tuple[str, int]:
    """
    Show the next choices to the player.

    """
    print("Choose the next movie:")
    for i, movie in enumerate(movie_choices):
        print(f"{i+1}. {movie}")

    if not choice:
        choice = int(input("Your choice: "))
    chosen_movie: Movie = movie_choices[choice - 1]
    if VERBOSE:
        print(f"Chosen movie: {chosen_movie}")
    # copy to clipboard for faster input
    pyperclip.copy(f"{chosen_movie}")
    if chosen_movie.name is None or chosen_movie.release_year is None:
        raise ValueError("Movie name or year is None.")
    return chosen_movie.name, chosen_movie.release_year


def auto_play_to_profile():
    game = Game()
    game.set_initial_movie("The Lion King", 1994)
    next_name, next_year = show_next_choices(game.possible_movies, 1)
    next_movie_id = game.connector.get_movie_from_name_and_year(next_name, next_year)
    # Get used links from the database
    used_links_ids = game.connector.get_used_links_from_movies(game.current_movie, next_movie_id)
    if VERBOSE:
        print(f"Chosen movie id: {next_movie_id}")
        print(f"Used links ids this turn: {used_links_ids}")
    # update game state
    game.play_turn(next_movie_id, used_links_ids)


def play_loop_single_player():
    """
    Version used for testing. We control both players, so we have
    to make actions for both of them.
    """

    game = Game()
    # game.set_initial_movie(input("Movie: "), int(input("Year: ")))
    game.set_initial_movie("freaky friday", 2003)

    while True:
        if game.turn % 2 == 0:
            # player turn
            try:
                # next_name, next_year = show_next_choices(game.possible_movies)
                next_name, next_year = show_next_choices(game.get_possible_movies_strategy_1())
            except ValueError:
                print("No possible movies left.")
                break
            next_movie_id = game.connector.get_movie_from_name_and_year(next_name, next_year)
            # Get used links from the database
            used_links_ids = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie_id
            )
            if VERBOSE:
                print(f"Chosen movie id: {next_movie_id}")
                print(f"Used links ids this turn: {used_links_ids}")
            # update game state
            game.play_turn(next_movie_id, used_links_ids)
        else:
            # opponent turn
            try:
                # next_name, next_year = show_next_choices(game.possible_movies, choice=1)
                next_name, next_year = show_next_choices(
                    game.get_possible_movies_strategy_1(), choice=1
                )
            except ValueError:
                print("No possible movies left.")
                break
            next_movie_id = game.connector.get_movie_from_name_and_year(next_name, next_year)
            # Get used links from the database
            used_links_ids = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie_id
            )
            if VERBOSE:
                print(f"Chosen movie id: {next_movie_id}")
                print(f"Used links ids this turn: {used_links_ids}")
            game.play_turn(next_movie_id, used_links_ids)

    print(
        f"Game over after {game.turn} turns. Winner is {game.starting_player.role if game.turn % 2 == 0 else game.opponent.role}"
    )


def play_against_opponent():
    """
    Here, we play as the main player against an opponent.
    We have to choose a movie, then the opponent chooses a movie.
    We enter the opponent's movie name and year manually, then we play the turn as usual
    when it is our turn, we have to choose a movie.
    """
    game = Game()
    starting = input("Are you playing first? (Y/n): ")
    if starting == "y" or "":
        game.starting_player.role = "player"
        game.opponent.role = "opponent"
    else:
        game.starting_player.role = "opponent"
        game.opponent.role = "player"

    game.set_initial_movie(input("initial movie: "), int(input("year: ")))

    while True:
        if game.turn % 2 == int(game.starting_player.role == "player"):
            # player turn
            try:
                next_name, next_year = show_next_choices(game.possible_movies)
            except ValueError:
                print("No possible movies left.")
                break
            next_movie_id = game.connector.get_movie_from_name_and_year(next_name, next_year)
            # Get used links from the database
            used_links_ids = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie_id
            )
            if VERBOSE:
                print(f"Chosen movie id: {next_movie_id}")
                print(f"Used links ids this turn: {used_links_ids}")
            # update game state
            game.play_turn(next_movie_id, used_links_ids)
        else:
            # opponent turn
            next_name = input("opponent's movie: ")
            next_year = int(input("year: "))
            next_movie_id = game.connector.get_movie_from_name_and_year(next_name, next_year)
            # Get used links from the database
            used_links_ids = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie_id
            )
            if VERBOSE:
                print(f"Chosen movie id: {next_movie_id}")
                print(f"Used links ids this turn: {used_links_ids}")
            game.play_turn(next_movie_id, used_links_ids)

    print(
        f"Game over after {game.turn} turns. Winner is {game.starting_player.role if game.turn % 2 == 0 else game.opponent.role}"
    )


# %%

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-v", "--verbose", action="store_true")

    args = parser.parse_args()

    VERBOSE = args.verbose

    play_loop_single_player()
