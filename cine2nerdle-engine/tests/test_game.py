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
    def test_ban_setter_normal(self, db_connector):
        player = Player()
        player.set_bans(db_connector, ["Tom Cruise", "Tom Hanks", "Tom Hardy"])
        for player in player.bans:
            assert isinstance(player, Person)
            assert player.id in [500, 31, 2524]

    def test_ban_setter_too_many(self, db_connector):
        player = Player()
        with pytest.raises(ValueError):
            player.set_bans(
                db_connector,
                [
                    "Tom Cruise",
                    "Tom Hanks",
                    "Tom Hardy",
                    "Tom Holland",
                ],
            )

    def test_start_game(self):
        game = Game()
        game._start_game(Movie(1))
        assert game.current_movie == Movie(1)
        assert game.links_count == {}
        assert game.movies_played == [Movie(1)]
        assert game.turn == 0
        assert game.game_over is False

    def test_set_initial_movie(self, db_connector: GraphDbConnector):
        game = Game()
        game.set_initial_movie(Movie(id=603, title="The Matrix", release_year=1999))
        movie_object = db_connector.get_movie_from_title_and_year("The Matrix", 1999)
        assert game.current_movie == movie_object
        assert game.links_count == {}
        assert game.movies_played == [movie_object]
        assert game.turn == 0
        assert game.game_over is False

    def test_set_initial_movie_nonexistant(self):
        game = Game()
        with pytest.raises(ValueError):
            game.set_initial_movie(Movie(id=0, title="zezzzzzzzz", release_year=1999))

    def test_play_alread_played_raises_error(self, db_connector: GraphDbConnector):
        game = Game()
        game.set_initial_movie(Movie(id=603, title="The Matrix", release_year=1999))
        movie = db_connector.get_movie_from_title_and_year("The Matrix", 1999)
        used_links = db_connector.get_all_links_from_movie(movie)
        with pytest.raises(ValueError):
            game.play_turn(movie, used_links)
