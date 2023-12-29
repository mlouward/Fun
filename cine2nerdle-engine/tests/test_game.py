import pytest

from game.movie import Movie
from game.neo4j_utils.neo4j_utils import GraphDbConnector
from game.person import Person
from game.play_game import Game, Player


@pytest.fixture
def db_connector():
    connector: GraphDbConnector = GraphDbConnector.get_default_connection()
    yield connector  # provide the fixture value
    # if you need any cleanup after the test, do it here


class TestGame:
    def test_ban_setter_normal(self):
        player = Player()
        player.bans = ["Tom Cruise", "Tom Hanks", "Tom Hardy"]
        assert player.bans == [500, 31, 2524]

    def test_ban_setter_too_many(self):
        player = Player()
        with pytest.raises(ValueError):
            player.bans = [
                "Tom Cruise",
                "Tom Hanks",
                "Tom Hardy",
                "Tom Holland",
            ]

    def test_start_game(self):
        game = Game()
        game.start_game(Movie(1), [Person(2), Person(5)])
        assert game.current_movie == Movie(1)
        assert game.links_count == {}
        assert game.movies_played == [Movie(1)]
        assert game.turn == 0
        assert game.game_over is False

    def test_set_initial_movie(self, db_connector: GraphDbConnector):
        game = Game()
        game.set_initial_movie("The Matrix", 1999)
        movie_object = db_connector.get_movie_from_name_and_year("The Matrix", 1999)
        assert game.current_movie == movie_object
        assert game.links_count == {}
        assert game.movies_played == [movie_object]
        assert game.turn == 0
        assert game.game_over is False

    def test_set_initial_movie_nonexistant(self):
        game = Game()
        with pytest.raises(ValueError):
            game.set_initial_movie("zzzzzzzzzzzzzz", 1999)

    def test_set_initial_movie_multiple_movies(self):
        game = Game()
        with pytest.warns(UserWarning):
            game.set_initial_movie("the", 1999)
