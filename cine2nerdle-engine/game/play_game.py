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
        self._player_number: int = 0

    def __eq__(self, __value: object) -> bool:
        if not isinstance(__value, Player):
            return False
        if self.username == "" and __value.username == "":
            return self.role == __value.role
        return self.username == __value.username

    def __str__(self) -> str:
        return f"Player {self.username or self.role}"

    def __repr__(self) -> str:
        return f"Player({self.username}, {self.role}, {self.bans}, {self.player_number=}, {self.skip_available=})"

    @property
    def player_number(self) -> int:
        return self._player_number

    @player_number.setter
    def player_number(self, player_number: int) -> None:
        self._player_number = player_number

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
        self.main_player: Player = Player()
        self.opponent: Player = Player()
        self.main_player.role = "me"
        self.opponent.role = "opponent"
        self.to_play: Player
        self.turn: int = 0
        self.current_movie: Movie = Movie(-1)
        self.movies_played: set[Movie]
        self.links_count: dict[Person, int]
        self._possible_movies: set[Movie]
        self.game_over: bool = False

    def __str__(self) -> str:
        return (
            f"Game state:\n\t- {self.main_player} ({self.main_player.player_number}) VS"
            f" {self.opponent} ({self.opponent.player_number})\n\t- {self.to_play} to play"
            f"\n\t- Turn {self.turn}\n\t- {self.current_movie} is the current movie"
            f"\n\t- {self.movies_played}\n\t- {self.links_count}"
        )

    @property
    def possible_movies(self) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        return self._get_possible_movies()

    def get_usable_links(self, all_links: set[Person]) -> set[Person]:
        """
        Get the usable links for the current player.

        Args:
            all_links (set[Person]): all links from the current movie

        Returns:
            set[Person]: set of usable links
        """
        #  get the links that have not been used 3 times yet
        return {
            link for link in all_links if link not in self.links_count or self.links_count[link] < 3
        }

    def _get_possible_movies(self) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        # get all links from the current movie
        current_movie_links: set[Person] = self.connector.get_all_links_from_movie(
            self.current_movie
        )
        # get the usable links
        usable_links: set[Person] = self.get_usable_links(current_movie_links)
        # get the possible movies using the usable links
        return self.connector.get_possible_movies_using_links(
            self.current_movie, self.movies_played, usable_links
        )

    def update_possible_movies(
        self, links_to_try: set[Person], possible_links: set[Person], possible_movies: list[Movie]
    ):
        """
        Update the possible movies and links to try.

        Args:
            links_to_try (set[Person]): links to try
            possible_links (set[Person]): possible links
            possible_movies (list[Movie]): possible movies

        Returns:
            None
        """
        if links_to_try:
            possible_links |= links_to_try
            possible_movies += [
                m
                for m in self.connector.get_possible_movies_using_links(
                    self.current_movie, self.movies_played, possible_links
                )
                if m not in possible_movies
            ]

    def get_possible_movies_strategy_1(self) -> list[Movie]:
        """
        Returns a set of playable movies in the current game state.
        In this order, prioritizes:
        - movies that have links that have been used twice (lethal)
        - movies that have links that we have banned
        - movies that have links that have not been used yet
        - movies that have links that have been used once

        Returns:
            set[Movie]: possible movies to play
        """
        current_movie_links = self.connector.get_all_links_from_movie(self.current_movie)

        possible_movies: list[Movie] = []
        possible_links: set[Person] = set()

        steps: list[set[Person]] = [
            #  get the links that have been used once AND that we banned
            {
                link
                for link in current_movie_links
                if link in self.links_count
                and self.links_count[link] == 1
                and link in self.main_player.bans
            },
            #  get the links that have been used twice
            {
                link
                for link in current_movie_links
                if link in self.links_count and self.links_count[link] == 2
            },
            # else, try to use links that we banned and have not been used yet
            {
                link
                for link in current_movie_links
                if link in self.main_player.bans and link not in self.links_count
            },
            # else, try to use links that have not been used yet
            {link for link in current_movie_links if link not in self.links_count},
            # else, get any link used less than 3 times and are banned
            {
                link
                for link in current_movie_links
                if link in self.links_count
                and self.links_count[link] < 3
                and link in self.main_player.bans
            },
            self.get_usable_links(current_movie_links),
        ]
        for step in steps:
            self.update_possible_movies(step, possible_links, possible_movies)
            # if we have at least 5 possible movies, return them
            # else, try the next step
            if len(possible_movies) >= 5:
                return possible_movies

        print("No possible movies left.")
        return possible_movies

    def _start_game(self, current_movie: Movie):
        """
        Start a new game.
        """
        self.to_play = self.main_player if self.main_player.player_number == 1 else self.opponent
        self.current_movie = current_movie
        self.movies_played = {current_movie}
        self.links_count = dict()

    def set_initial_movie(self, movie: Movie) -> None:
        """
        Set the initial movie for the game.
        """
        self._start_game(movie)

    def play_turn(self, movie: Movie, used_links: set[Person]) -> None:
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
        self.movies_played.add(movie)
        self.current_movie = movie
        self.update_used_links(used_links)
        self.to_play = self.opponent if self.to_play == self.main_player else self.main_player
        if VERBOSE >= 1:
            print(f"Turn: {self.turn}")
            print(f"Movies played: {self.movies_played}")
            print(f"Links count: {self.links_count}")
            print(f"Current movie: {self.current_movie}")

    def update_used_links(self, used_links: set[Person]) -> None:
        for link_id in used_links:
            if link_id in self.links_count:
                self.links_count[link_id] += 1
            else:
                self.links_count[link_id] = 1

    def use_skip(self, player: Player) -> None:
        """
        Use skip for a player => it is not their turn anymore.
        Update game state.
        """
        player.use_skip()
        self.to_play = self.opponent if self.to_play == self.main_player else self.main_player
        self.turn += 1
        if VERBOSE >= 1:
            print(f"{player} used skip. Turn: {self.turn}")


def show_next_choices(
    game: Game, movie_choices: list[Movie], choice: int | None = None
) -> Movie | None:
    """
    Show the next movie choices to the player, as well as some actions:
    - skip turn (next player plays)
    - play other movie (enter manually)

    Args:
        :param game: the current game
        :param movie_choices: the movies that can be played next
        :param choice: the choice made by the player (if any)

    Returns:
        Movie, optional: the movie chosen by the player, or None if they skipped
    """
    print("Choose the next action:")
    for i, movie in enumerate(movie_choices):
        print(f"{i+1}. {movie}")
    print(f"{len(movie_choices)+1}. Skip turn")
    print(f"{len(movie_choices)+2}. Play other movie")
    print(f"{len(movie_choices)+3}. Not my turn to play")

    if not choice:
        choice = int(input("Your choice: "))

    if choice == len(movie_choices) + 1:
        game.use_skip(game.to_play)
        return None
    elif choice == len(movie_choices) + 2:
        movie_name = input("Movie name: ")
        movie_year = int(input("Movie year: "))
        chosen_movie = game.connector.get_movie_from_title_and_year(movie_name, movie_year)
    elif choice == len(movie_choices) + 3:
        # update game state
        game.to_play = game.opponent if game.to_play == game.main_player else game.main_player
        return None
    else:
        chosen_movie: Movie = movie_choices[choice - 1]

    if VERBOSE >= 1:
        print(f"Chosen movie: {chosen_movie}")
    # copy to clipboard for faster input
    pyperclip.copy(f"{chosen_movie}")
    return chosen_movie


def show_next_choices_no_input(movie_choices: list[Movie]):
    """
    Show the next choices to the player.
    """
    for i, movie in enumerate(movie_choices):
        print(f"{i+1}. {movie}")


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
                next_movie = show_next_choices(game, game.get_possible_movies_strategy_1())
                if not next_movie:
                    continue
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
                next_movie = show_next_choices(
                    game, game.get_possible_movies_strategy_1(), choice=1
                )
                if not next_movie:
                    continue
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
        f"Game over after {game.turn} turns. Winner is {game.main_player.role if game.turn % 2 == 0 else game.opponent.role}"
    )


def play_against_opponent():
    """
    Here, we play as the main player against an opponent.
    We have to choose a movie, then the opponent chooses a movie.
    We enter the opponent's movie name and year manually, then we play the turn as usual
    when it is our turn, we have to choose a movie.
    """
    game = Game()
    starting = input("Are you playing first? (Y/n): ").strip()
    if starting in ("y", ""):
        game.main_player.set_bans(
            game.connector,
            [
                "matthew broderick",
                "rowan atkinson",
                "anya taylor-joy",
            ],
        )
        game.main_player.player_number = 1
        game.opponent.player_number = 2
    else:
        game.main_player.player_number = 2
        game.opponent.player_number = 1

    while game.current_movie.id == -1:
        try:
            game.set_initial_movie(
                game.connector.get_movie_from_title_and_year(
                    input("initial movie: "), int(input("year: "))
                )
            )
        except ValueError as e:
            print(e)

    while True:
        if VERBOSE >= 2:
            print(game)

        if game.to_play == game.main_player:
            # player turn
            try:
                next_movie = show_next_choices(game, game.get_possible_movies_strategy_1())
                if not next_movie:  # we skipped
                    continue

            except ValueError as e:
                print(e)
                continue
            # Get used links from the database
            used_links: set[Person] = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie
            )
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie.id}")
                print(f"Used links ids this turn: {used_links}")
            # update game state
            game.play_turn(next_movie, used_links)
        else:
            # opponent turn
            next_name = input("opponent's movie: ")
            next_year = input("year: ")
            if not next_name and not next_year:
                # opponent skipped
                game.use_skip(game.opponent)
                continue

            try:
                next_movie: Movie | None = game.connector.get_movie_from_title_and_year(
                    next_name, int(next_year)
                )
            except ValueError:
                continue  # if no movie found, try again
            # Get used links from the database
            used_links: set[Person] = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie
            )
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie}")
                print(f"Used links ids this turn: {used_links}")
            game.play_turn(next_movie, used_links)


def initialize_game(game: Game, game_data: dict[str, Any]) -> None:
    game.main_player.role = "me"
    game.main_player.set_bans(game.connector, game_data["bans"])
    game.main_player.username = game_data["username"]

    players_data: dict[str, Any] = game_data["playersData"]

    game.opponent.role = "opponent"
    game.opponent.set_bans(game.connector, [])
    # get the key from playersData that is not the starting player's username
    game.opponent.username = (players_data.keys() - {game.main_player.username}).pop()

    game.main_player.player_number = players_data[game.main_player.username]["playerNumber"]
    game.opponent.player_number = players_data[game.opponent.username]["playerNumber"]
    game.to_play = game.main_player if game.main_player.player_number == 1 else game.opponent
    if VERBOSE >= 1:
        print(f"Starting player: {repr(game.main_player)}")
        print(f"Opponent: {repr(game.opponent)}")
        print(f"To play: {game.to_play.role}")
    if game.to_play == game.main_player:
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
    try:
        title = input("Enter opponent's movie name: ")
        year = int(input("Enter opponent's movie year: "))
        opponent_movie: Movie = game.connector.get_movie_from_title_and_year(title, year)
        if VERBOSE >= 1:
            print(opponent_movie)
        # get the links used by the opponent
        used_links = game.connector.get_used_links_from_movies(game.current_movie, opponent_movie)
        # update game state
        game.play_turn(opponent_movie, used_links)

    except ValueError:
        # opponent lost
        print("Opponent skipped?")
        skipped = input("Did the opponent skip? (y/N): ")
        if skipped == "y":
            game.opponent.use_skip()
        else:
            return

    # suggest 5 movies to play for us
    next_movie = show_next_choices(game, game.get_possible_movies_strategy_1())
    if not next_movie:
        return

    # Get used links from the database
    used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
    if VERBOSE >= 2:
        print(f"Chosen movie id: {next_movie.id}")
        print(f"Used links ids this turn: {used_links}")

    # update game state
    try:
        game.play_turn(next_movie, used_links)
    except ValueError:
        print("old packet")
        return


def play_server_side(game: Game, game_data: dict[str, Any], action: str):
    if action == "add-time":
        if VERBOSE >= 1:
            print("lifeline used")
        # Ask if skip was used by opponent
        # if yes, update game state
        skip_used = input("Did the opponent use skip? (y/N): ")
        if skip_used == "y":
            game.opponent.use_skip()
            # suggest 5 movies to play
            show_next_choices_no_input(game.get_possible_movies_strategy_1())
        # else:
        #     # invert player's numbers
        #     game.starting_player.player_number, game.opponent.player_number = (
        #         game.opponent.player_number,
        #         game.starting_player.player_number,
        #     )

    else:
        if game.to_play == game.main_player:
            if VERBOSE >= 1:
                print("We played:")
            # check if skipped
            if (
                game_data["gameData"]["playersData"][game.main_player.username]["lifelines"]["skip"]
                is False
            ):
                if VERBOSE >= 1:
                    print("We used skip")
                game.use_skip(game.main_player)

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
                game.use_skip(game.opponent)
                # our turn to play
                movie = show_next_choices(game, game.get_possible_movies_strategy_1())
                if not movie:
                    return  # we used skip, game is a draw
                # Get used links from the database
                used_links = game.connector.get_used_links_from_movies(game.current_movie, movie)
                if VERBOSE >= 2:
                    print(f"Chosen movie id: {movie.id}")
                    print(f"Used links ids this turn: {used_links}")
                # update game state
                try:
                    game.play_turn(movie, used_links)
                except ValueError:
                    print("old packet")

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
            used_links = game.connector.get_used_links_from_movies(
                game.current_movie, opponent_movie
            )
            # update game state
            try:
                game.play_turn(opponent_movie, used_links)
            except ValueError:
                print("old packet")
                return
            # suggest 5 movies to play for us
            show_next_choices_no_input(game.get_possible_movies_strategy_1())
            # once we select a movie, the game will send a new update-game event


def analyze_game_packet(packet: dict, game: Game):
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
    if action in ("find-match", "rematch-request"):
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
        if VERBOSE >= 2:
            print(game)
        # In this case, the other player is player 1 (server side)
        # and we are player 2 (client side)
        # After submitting a movie, we have to enter manually the movie played by the opponent,
        # update the game, and suggest 5 movies to play based on it.
        # TODO: handle skip when they are the server

        # If we are server side and we both submit & update game, ignore
        if game.main_player.player_number == 1 and game.current_movie in game.movies_played:
            return

        # Else, we are client side
        if game_data["input"] == "SKIP":
            # we used skip
            if VERBOSE >= 1:
                print("We used skip")
            game.main_player.use_skip()
            # suggest 5 movies to play
        elif not game_data["input"]:
            # we lost
            print("We lost?")
            return
        # else:
        # update game state with movie i just played
        # title, year = game_data["input"].split(" (")
        # year = int(year[:-1])
        # movie = game.connector.get_movie_from_title_and_year(title, year)
        # used_links = game.connector.get_used_links_from_movies(game.current_movie, movie)
        # game.play_turn(movie, used_links)

        play_client_side(game)

    elif action in ("update-game", "add-time"):
        if VERBOSE >= 2:
            print(game)
        # In this case, the other player is player 2 (client side)
        # and we are player 1 (server side)
        # We can access all the game data, so we can update the game state
        # automatically, and suggest 5 movies to play.
        print("update", game.to_play)

        # triggered after submitting a movie, or after opponent submits a movie
        # get who just played a movie to make the distinction: do we need to play a movie
        # or did we just play and we need to update the game state ?
        play_server_side(game, game_data, action)

    elif action == "game-over":
        print("game over")
        # game over
        # get the winner
        winner: str = game_data["gameData"]["winner"]  # 1 or 2
        if VERBOSE >= 1:
            print(f"{winner=}")
        if winner == game.main_player.player_number:
            print("We won!")
        elif winner == game.opponent.player_number:
            print("We lost!")
        return


def analyze_game_packet_manual(packet: dict, game: Game):
    """
    Uses game packets to handle game start, but prompts the user for input
    """
    # Get the data from the websocket payload
    rawtext = packet["websocket"].get_field_value("websocket.payload.text")
    if not rawtext:
        return
    data = re.search(r"^\d+(\[.*\])", rawtext)
    # parse the json in the payload
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
    if action in ("find-match", "rematch-request"):
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

    elif action == "game-over":
        print("game over")
        # game over
        # get the winner
        winner: str = game_data["gameData"]["winner"]  # 1 or 2
        if VERBOSE >= 1:
            print(f"{winner=}")
        if winner == game.main_player.player_number:
            print("We won!")
        elif winner == game.opponent.player_number:
            print("We lost!")
        return

    elif action in ("error", "add-time", "opponent-disconnected"):
        # do nothing
        return

    else:
        if VERBOSE >= 2:
            print(game)
        # resort to manual input
        # check whose turn it is to play
        if game.to_play == game.main_player:
            # player turn
            try:
                next_movie = show_next_choices(game, game.get_possible_movies_strategy_1())
                if not next_movie:
                    return  # we used skip or not our turn
            except ValueError:
                print("No possible movies left.")
                return
            # Get used links from the database
            used_links: set[Person] = game.connector.get_used_links_from_movies(
                game.current_movie, next_movie
            )
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie.id}")
                print(f"Used links ids this turn: {used_links}")
            # update game state
            game.play_turn(next_movie, used_links)
        else:
            # opponent turn
            next_movie: Movie | None = None
            while not next_movie:
                next_name = input("opponent's movie: ")
                if next_name == "skip":
                    # opponent skipped
                    game.use_skip(game.opponent)
                    return
                next_year = input("year: ")
                if not next_name and not next_year:
                    # not his turn
                    return
                try:
                    next_movie = game.connector.get_movie_from_title_and_year(
                        next_name, int(next_year)
                    )
                except ValueError as e:
                    print(e)
                    continue  # if no movie found, try again
            # Get used links from the database
            used_links = game.connector.get_used_links_from_movies(game.current_movie, next_movie)
            if VERBOSE >= 1:
                print(f"Chosen movie id: {next_movie}")
                print(f"Used links ids this turn: {used_links}")
            game.play_turn(next_movie, used_links)


def online_play():
    game = Game()

    # capture websocket traffic on the Ethernet interface
    # where game info is sent
    capture = pyshark.LiveCapture(
        interface="Ethernet", display_filter='_ws.col.protocol == "WebSocket"'
    )
    capture.apply_on_packets(lambda packet: analyze_game_packet(packet, game))


def online_play_manual():
    game = Game()

    capture = pyshark.LiveCapture(
        interface="Ethernet", display_filter='_ws.col.protocol == "WebSocket"'
    )
    capture.apply_on_packets(lambda packet: analyze_game_packet_manual(packet, game))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    # -v is verbose = 1, -vv is verbose = 2
    parser.add_argument("-v", "--verbose", action="store_true")
    parser.add_argument("-vv", "--very-verbose", action="store_true")

    args = parser.parse_args()

    VERBOSE = 1 if args.verbose else 2 if args.very_verbose else 0

    online_play_manual()
