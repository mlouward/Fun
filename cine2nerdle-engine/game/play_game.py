# %%

from typing import Literal

import pyperclip

from neo4j_utils.neo4j_utils import GraphDbConnector


class Player:
    """
    A player has a name, a list of 3 bans, and a skip available.
    """

    def __init__(self) -> None:
        # role
        self._role: Literal["player", "opponent", None] = None
        # IDs of banned actors
        self._bans: list[int] = []
        self._skip_available: bool = True

    @property
    def role(self) -> Literal["player", "opponent", None]:
        return self._role

    @role.setter
    def role(self, role: Literal["player", "opponent", None]) -> None:
        self._role = role

    @property
    def bans(self) -> list[int]:
        return self._bans

    @bans.setter
    def bans(self, banned_names: list[str]) -> None:
        if len(banned_names) > 3:
            raise ValueError("A player can only ban 3 actors.")
        for ban in banned_names:
            # query the database to get the ID of the actor
            # add it to the bans list
            person_id = GraphDbConnector.get_id_from_person_name(ban)
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
        self.main_player: Player
        self.opponent: Player
        self.current_movie: int  # movie_id
        self.turn: int
        self.movies_played: list[int]  # movie_id
        self.links_count: dict[int, int]  # link_id: count
        self._possible_movies: list[str]  # movie_id
        self.game_over: bool

    @property
    def possible_movies(self) -> list[str]:
        """
        Get the possible moves for the current player.
        """
        usable_links = self.get_usable_links()
        return self.connector.get_possible_movies(
            self.current_movie, self.movies_played, usable_links
        )

    def get_usable_links(self) -> list[int]:
        """
        Get the usable links for the current player.
        return: list of link ids used less than 3 times
        """
        return [link_id for link_id, count in self.links_count.items() if count < 3]

    def start_game(self, current_movie: int, usable_links: list[int]):
        """
        Start a new game.
        """
        self.main_player = Player()
        self.opponent = Player()
        self.main_player.role = "player"
        self.opponent.role = "opponent"
        self.turn = 0
        self.game_over = False
        self.current_movie = current_movie
        self.movies_played = [current_movie]
        self.links_count = {link_id: 0 for link_id in usable_links}

    def set_initial_movie(self, movie: str, year: int) -> None:
        """
        Set the initial movie for the game.
        """
        (
            starting_movie_id,
            usable_links,
        ) = self.connector.get_id_and_links_from_movie_title_and_year(movie, year)
        self.start_game(starting_movie_id, usable_links)

    def play_turn(self, movie_id: int, used_links_ids: list[int]) -> None:
        """
        Play a turn. Should update the game state:
        - turn number
        - used movies
        - used links
        - current movie
        """
        self.turn += 1
        self.movies_played.append(movie_id)
        self.current_movie = movie_id
        self.update_used_links(used_links_ids)

    def update_used_links(self, used_links_ids: list[int]) -> None:
        for link_id in used_links_ids:
            if link_id in self.links_count:
                self.links_count[link_id] += 1
            else:
                self.links_count[link_id] = 1


# %%
def show_next_choices(
    movie_choices: list[str], choice: int | None = None
) -> tuple[str, int]:
    """
    Show the next choices to the player.

    """
    print("Choose the next movie:")
    for i, movie in enumerate(movie_choices):
        print(f"{i+1}. {movie}")

    if not choice:
        choice = int(input("Your choice: "))
    choice_full_name = movie_choices[choice - 1]
    # copy to clipboard for faster input
    pyperclip.copy(choice_full_name)
    choice_name = choice_full_name.split(" (")[0]
    choice_year = int(choice_full_name.split(" (")[1].replace(")", ""))
    return choice_name, choice_year


def auto_play_to_profile():
    game = Game()
    game.set_initial_movie("The Lion King", 1994)
    next_name, next_year = show_next_choices(game.possible_movies, 1)
    (
        movie_id,
        used_links_ids,
    ) = game.connector.get_id_and_links_from_movie_title_and_year(next_name, next_year)
    game.play_turn(movie_id, used_links_ids)
    next_name, next_year = show_next_choices(game.possible_movies, 1)


def manual_play():
    game = Game()
    game.set_initial_movie("The Lion King", 1994)
    next_name, next_year = show_next_choices(game.possible_movies)
    (
        movie_id,
        used_links_ids,
    ) = game.connector.get_id_and_links_from_movie_title_and_year(next_name, next_year)
    game.play_turn(movie_id, used_links_ids)
    next_name, next_year = show_next_choices(game.possible_movies)


def play_loop_single_player():
    """
    Version used for testing. We control both players, so we have
    to make actions for both of them.
    """

    game = Game()
    game.set_initial_movie(input("Movie: "), int(input("Year: ")))

    while True:
        if game.turn % 2 == 0:
            # player turn
            next_name, next_year = show_next_choices(game.possible_movies)
            (
                movie_id,
                used_links_ids,
            ) = game.connector.get_id_and_links_from_movie_title_and_year(
                next_name, next_year
            )
            game.play_turn(movie_id, used_links_ids)
        else:
            # opponent turn
            next_name, next_year = show_next_choices(game.possible_movies, 1)
            (
                movie_id,
                used_links_ids,
            ) = game.connector.get_id_and_links_from_movie_title_and_year(
                next_name, next_year
            )
            game.play_turn(movie_id, used_links_ids)


# %%

if __name__ == "__main__":
    play_loop_single_player()
