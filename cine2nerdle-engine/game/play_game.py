import argparse
import json
import re
from typing import Any, Literal

import pyperclip
import pyshark

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
        self._role: Literal["me", "opponent", None] = None
        # IDs of banned actors
        self._bans: list[Person] = []
        self.username: str = ""
        self._skip_available: bool = True
        self.player_number: int

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Player):
            return False
        return self.username == __value.username

    def __str__(self) -> str:
        return f"Player {self.username}"

    def __repr__(self) -> str:
        return f"Player({self.username}, {self.role}, {self.bans}, {self.player_number=}, {self.skip_available=})"

    @property
    def role(self) -> Literal["me", "opponent", None]:
        return self._role

    @role.setter
    def role(self, role: Literal["me", "opponent", None]) -> None:
        self._role = role

    @property
    def bans(self) -> list[Person]:
        return self._bans

    def set_bans(self, connector: GraphDbConnector, banned_names: list[str]) -> None:
        if len(banned_names) > 3:
            raise ValueError("A player can only ban 3 actors.")
        for ban in banned_names:
            if not ban:
                continue
            # query the database to get the ID of the actor
            # add it to the bans list
            person_id = connector.get_person_from_name(ban)
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
        self.to_play: Player
        self.current_movie: Movie  # movie_id
        self.turn: int
        self.movies_played: list[Movie]  # movie_id
        self.links_count: dict[Person, int]  # link_id: count
        self._possible_movies: list[Movie]  # movie_id
        self.game_over: bool

    def _get_possible_movies(self, strategy=None) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        all_links_from_current_movie = self.connector.get_all_links_from_movie(self.current_movie)
        if strategy == 1:
            usable_links = self.get_usable_links_strategy_1(all_links_from_current_movie)
        else:
            usable_links = self.get_usable_links(all_links_from_current_movie)

        if VERBOSE >= 1:
            print(f"Usable links: {usable_links}")

        if not usable_links:
            self.game_over = True
            raise ValueError("No possible movies left.")

        return self.connector.get_possible_movies(
            self.current_movie, self.movies_played, usable_links
        )

    @property
    def possible_movies(self) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        return self._get_possible_movies()

    def get_possible_movies_strategy_1(self) -> list[Movie]:
        """
        Get the possible movies for the current player. Prefers links with 2 uses, then 0 uses.
        If no such links exist, fallback to links with 1 use.
        """
        return self._get_possible_movies(strategy=1)

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
        #  get the links that have been used 2 times
        killshots = [
            link
            for link in current_movie_links
            if link in self.links_count and self.links_count[link] == 2
        ]

        if killshots:
            possible_movies: list[Movie] = self.connector.get_possible_movies(
                self.current_movie, self.movies_played, killshots
            )
            if possible_movies:
                return killshots

        # fallback to using links with 0 uses
        zero_star_links = [
            link
            for link in current_movie_links
            if link not in self.links_count or self.links_count[link] == 0
        ]
        if zero_star_links:
            possible_movies: list[Movie] = self.connector.get_possible_movies(
                self.current_movie, self.movies_played, zero_star_links
            )
            if possible_movies:
                return zero_star_links

        # fallback to using links with 1 use
        return self.get_usable_links(current_movie_links)

    def _start_game(self, current_movie: Movie):
        """
        Start a new game.
        """
        self.starting_player = Player()
        self.opponent = Player()
        self.starting_player.role = "me"
        self.opponent.role = "opponent"
        self.to_play = self.starting_player
        self.turn = 0
        self.game_over = False
        self.current_movie = current_movie
        self.movies_played = [current_movie]
        self.links_count = dict()

    def set_initial_movie(self, movie: Movie) -> None:
        """
        Set the initial movie for the game.
        """
        all_links = self.connector.get_all_links_from_movie(movie)
        self._start_game(movie)

    def play_turn(self, movie: Movie, used_links: list[Person]) -> None:
        """
        Play a turn. Should update the game state:
        - turn number
        - used movies
        - used links
        - current movie
        """
        if movie in self.movies_played:
            raise ValueError("Movie already played.")
        self.turn += 1
        self.movies_played.append(movie)
        self.current_movie = movie
        self.update_used_links(used_links)
        self.to_play = (
            self.opponent if self.to_play == self.starting_player else self.starting_player
        )
        if VERBOSE >= 1:
            print(f"Turn: {self.turn}")
            print(f"Movies played: {self.movies_played}")
            print(f"Links count: {self.links_count}")
            print(f"Current movie: {self.current_movie}")

    def update_used_links(self, used_links: list[Person]) -> None:
        for link_id in used_links:
            if link_id in self.links_count:
                self.links_count[link_id] += 1
            else:
                self.links_count[link_id] = 1


def show_next_choices(movie_choices: list[Movie], choice: int | None = None) -> Movie:
    """
    Show the next choices to the player.

    """
    print("Choose the next movie:")
    for i, movie in enumerate(movie_choices):
        print(f"{i+1}. {movie}")

    if not choice:
        choice = int(input("Your choice: "))
    chosen_movie: Movie = movie_choices[choice - 1]
    if VERBOSE >= 1:
        print(f"Chosen movie: {chosen_movie}")
    # copy to clipboard for faster input
    pyperclip.copy(f"{chosen_movie}")
    if chosen_movie.title is None or chosen_movie.release_year is None:
        raise ValueError("Movie name or year is None.")
    return chosen_movie


def show_next_choices_no_input(movie_choices: list[Movie]):
    """
    Show the next choices to the player.
    """
    for i, movie in enumerate(movie_choices):
        print(f"{i+1}. {movie}")


def auto_play_to_profile():
    game = Game()
    game.set_initial_movie(game.connector.get_movie_from_title_and_year("The Lion King", 1994))
    next_movie = show_next_choices(game.possible_movies, 1)
    # Get used links from the database
    used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
    if VERBOSE >= 1:
        print(f"Chosen movie id: {next_movie.id}")
        print(f"Used links ids this turn: {used_links}")
    # update game state
    game.play_turn(next_movie, used_links)


def play_loop_single_player():
    """
    Version used for testing. We control both players, so we have
    to make actions for both of them.
    """

    game = Game()
    # game.set_initial_movie(input("Movie: "), int(input("Year: ")))
    game.set_initial_movie(game.connector.get_movie_from_title_and_year("freaky friday", 2003))

    while True:
        if game.turn % 2 == 0:
            # player turn
            try:
                # next_name, next_year = show_next_choices(game.possible_movies)
                next_movie = show_next_choices(game.get_possible_movies_strategy_1())
            except ValueError:
                print("No possible movies left.")
                break
            # Get used links from the database
            used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie.id}")
                print(f"Used links ids this turn: {used_links}")
            # update game state
            game.play_turn(next_movie, used_links)
        else:
            # opponent turn
            try:
                # next_name, next_year = show_next_choices(game.possible_movies, choice=1)
                next_movie = show_next_choices(game.get_possible_movies_strategy_1(), choice=1)
            except ValueError:
                print("No possible movies left.")
                break
            # Get used links from the database
            used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie.id}")
                print(f"Used links ids this turn: {used_links}")
            game.play_turn(next_movie, used_links)

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
        game.starting_player.role = "me"
        game.opponent.role = "opponent"
        game.starting_player.set_bans(
            game.connector,
            [
                "ice cube",
                "jackie chan",
                "jamie lee curtis",
            ],
        )
    else:
        game.starting_player.role = "opponent"
        game.opponent.role = "me"

    game.set_initial_movie(
        game.connector.get_movie_from_title_and_year(input("initial movie: "), int(input("year: ")))
    )

    while True:
        if game.turn % 2 == int(game.starting_player.role == "me"):
            # player turn
            try:
                next_movie = show_next_choices(game.possible_movies)
            except ValueError:
                print("No possible movies left.")
                break
            # Get used links from the database
            used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie.id}")
                print(f"Used links ids this turn: {used_links}")
            # update game state
            game.play_turn(next_movie, used_links)
        else:
            # opponent turn
            next_name = input("opponent's movie: ")
            next_year = int(input("year: "))
            next_movie_id = game.connector.get_movie_from_title_and_year(next_name, next_year)
            # Get used links from the database
            used_links = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie_id
            )
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie_id}")
                print(f"Used links ids this turn: {used_links}")
            game.play_turn(next_movie_id, used_links)

    print(
        f"Game over after {game.turn} turns. Winner is {game.starting_player.role if game.turn % 2 == 0 else game.opponent.role}"
    )


def initialize_game(game: Game, game_data: dict[str, Any]) -> None:
    game.starting_player.role = "me"
    game.starting_player.set_bans(game.connector, game_data["bans"])
    game.starting_player.username = game_data["username"]

    players_data: dict[str, Any] = game_data["playersData"]

    game.opponent.role = "opponent"
    game.opponent.set_bans(game.connector, [])
    # get the key from playersData that is not the starting player's username
    game.opponent.username = (players_data.keys() - {game.starting_player.username}).pop()

    game.starting_player.player_number = players_data[game.starting_player.username]["playerNumber"]
    game.opponent.player_number = players_data[game.opponent.username]["playerNumber"]
    game.to_play = (
        game.starting_player if game.starting_player.player_number == 1 else game.opponent
    )
    if VERBOSE >= 1:
        print(f"Starting player: {repr(game.starting_player)}")
        print(f"Opponent: {repr(game.opponent)}")
        print(f"To play: {game.to_play.role}")
    if game.to_play == game.starting_player:
        # suggest 5 movies to play
        show_next_choices_no_input(game.get_possible_movies_strategy_1())
    else:
        play_client_side(game)


def play_client_side(game: Game):
    # In this case, the other player is player 1 (server side)
    # and we are player 2 (client side)
    # After submitting a movie, we have to enter manually the movie played by the opponent,
    # update the game, and suggest 5 movies to play based on it.
    # get the movie played by the opponent
    title = input("Enter opponent's movie name: ")
    year = int(input("Enter opponent's movie year: "))
    opponent_movie: Movie = game.connector.get_movie_from_title_and_year(title, year)
    if VERBOSE >= 1:
        print(opponent_movie)
    # get the links used by the opponent
    used_links = game.connector.get_used_links_from_movies(game.current_movie, opponent_movie)
    # update game state
    game.play_turn(opponent_movie, used_links)

    # suggest 5 movies to play for us
    next_movie = show_next_choices(game.get_possible_movies_strategy_1())

    # Get used links from the database
    used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
    if VERBOSE >= 2:
        print(f"Chosen movie id: {next_movie.id}")
        print(f"Used links ids this turn: {used_links}")

    # update game state
    game.play_turn(next_movie, used_links)


def play_server_side(game, game_data):
    if game.to_play == game.starting_player:
        if VERBOSE >= 1:
            print("We played:")
        # check if skipped
        if (
            game_data["gameData"]["playersData"][game.starting_player.username]["lifelines"]["skip"]
            is False
        ):
            if VERBOSE >= 1:
                print("We used skip")
            game.starting_player.use_skip()

        # verify that the game says that next player's turn is the opponent
        assert game_data["gameData"]["playerTurn"] == game.opponent.player_number
        # get the movie we just played
        title, year = game_data["newMovie"]["title"].split(" (")
        year = int(year[:-1])
        our_movie: Movie = game.connector.get_movie_from_title_and_year(title, year)
        if VERBOSE >= 1:
            print(our_movie)

        # get the links we used
        used_links = game.connector.get_used_links_from_movies(game.current_movie, our_movie)
        # update game state
        game.play_turn(our_movie, used_links)
    else:
        if VERBOSE >= 1:
            print("Opponent played:")
        # check if opponent skipped
        if game_data["connections"][-1] == "SKIP":
            # opponent used skip
            if VERBOSE >= 1:
                print("Opponent used skip")
            game.opponent.use_skip()
            show_next_choices_no_input(game.get_possible_movies_strategy_1())
            return

        # verify that the game also says next turn is ours
        # assert game_data["gameData"]["playerTurn"] == game.starting_player.player_number
        # get the movie played by the opponent
        title, year = game_data["newMovie"]["title"].split(" (")
        year = int(year[:-1])
        opponent_movie: Movie = game.connector.get_movie_from_title_and_year(title, year)
        if VERBOSE >= 1:
            print(opponent_movie)
        # get the links used by the opponent
        used_links = game.connector.get_used_links_from_movies(game.current_movie, opponent_movie)
        # update game state
        game.play_turn(opponent_movie, used_links)

        # suggest 5 movies to play for us
        show_next_choices_no_input(game.get_possible_movies_strategy_1())
        # once we select a movie, the game will send a new update-game event


def analyze_game_packet(packet, game: Game):
    # Get the data from the websocket payload
    rawtext = packet["websocket"].get_field_value("websocket.payload.text")
    # parse the json in the payload
    if not rawtext:
        return
    data = re.search(r"^\d+(\[.*\])", rawtext)
    parsed_data = json.loads(data.group(1)) if data else None
    if not parsed_data:
        print(rawtext)
        return
    if VERBOSE >= 2:
        print(parsed_data)
        print("-" * 20)
    # 1st element is a string, 2nd is a dict
    action: str
    game_data: dict
    action, game_data = parsed_data[0], parsed_data[1]
    # appears after clicking on "find game"
    # Should set the starting movie
    if action == "find-match":
        print("found match")
        starting_movie_input: str = game_data["startingMovieInput"]
        if not starting_movie_input:
            raise ValueError("No starting movie input.")
        name, year = starting_movie_input.split(" (")
        year = int(year[:-1])
        # get initial movie information and set it for the current game
        starting_movie: Movie = game.connector.get_movie_from_title_and_year(name, year)
        game.set_initial_movie(starting_movie)
        if VERBOSE >= 1:
            print(f"Starting movie: {starting_movie}")

    elif action == "ready-up":
        print("ready")
        # set the player's bans and who plays first according to pdata["playerNumber"]
        initialize_game(game, game_data)

    elif action == "submit-movie":
        # In this case, the other player is player 1 (server side)
        # and we are player 2 (client side)
        # After submitting a movie, we have to enter manually the movie played by the opponent,
        # update the game, and suggest 5 movies to play based on it.
        # TODO: handle skip when they are the server
        # update game state with movie i just played
        movie = game.connector.get_movie_from_title_and_year(
            game_data["currentMovieTitle"], game_data["currentMovieYear"]
        )
        used_links = game.connector.get_used_links_from_movies(game.current_movie, movie)
        game.play_turn(movie, used_links)

        play_client_side(game)

    elif action == "add-time":
        if VERBOSE >= 1:
            print("lifeline used")
        # Ask if skip was used by opponent
        # if yes, update game state
        skip_used = input("Did the opponent use skip? (Y/n): ")
        if skip_used == "y" or "":
            game.opponent.use_skip()
            # suggest 5 movies to play
            show_next_choices_no_input(game.get_possible_movies_strategy_1())

    elif action in ("update-game", "add-time"):
        # In this case, the other player is player 2 (client side)
        # and we are player 1 (server side)
        # We can access all the game data, so we can update the game state
        # automatically, and suggest 5 movies to play.
        print("update", game.to_play)

        # triggered after submitting a movie, or after opponent submits a movie
        # get who just played a movie to make the distinction: do we need to play a movie
        # or did we just play and we need to update the game state ?
        play_server_side(game, game_data)

    elif action == "game-over":
        print("game over")
        # game over
        # get the winner
        winner: str = game_data["gameData"]["winner"]  # 1 or 2
        if VERBOSE >= 1:
            print(f"{winner=}")
        if winner == game.starting_player.player_number:
            print("We won!")
        elif winner == game.opponent.player_number:
            print("We lost!")
        return


def online_play():
    game = Game()

    # capture websocket traffic on the Ethernet interface
    # where game info is sent
    capture = pyshark.LiveCapture(
        interface="Ethernet", display_filter='_ws.col.protocol == "WebSocket"'
    )
    capture.apply_on_packets(lambda packet: analyze_game_packet(packet, game))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    # -v is verbose = 1, -vv is verbose = 2
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("-vv", "--very-verbose", action="store_true")

    args = parser.parse_args()

    VERBOSE = 1 if args.verbose else 2 if args.very_verbose else 0

    online_play()
