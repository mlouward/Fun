# %%

from typing import Literal

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
            person_id = GraphDbConnector.get_id_from_name(ban)
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
        self.used_links: dict[int, int]  # link_id: count
        self._possible_moves: list[int]  # movie_id
        self.game_over: bool

    @property
    def possible_movies(self) -> list[int]:
        """
        Get the possible moves for the current player.
        """
        return self.connector.get_possible_movies(self.current_movie)

    def start_game(self, current_movie) -> None:
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
        self.used_links = dict()

    def play_turn(self, movie_id: int) -> None:
        """
        Play a turn.
        """
        self.turn += 1
        self.movies_played.append(movie_id)
        self.current_movie = movie_id
