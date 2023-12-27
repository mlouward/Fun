import pytest
from neo4j import Driver

from game.play_game import Game, Player
from neo4j_utils.neo4j_utils import GraphDbConnector


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
        game.start_game(1, [1, 2, 3])
        assert game.current_movie == 1
        assert game.links_count == {1: 0, 2: 0, 3: 0}
        assert game.movies_played == [1]
        assert game.turn == 0
        assert game.game_over is False

    def test_set_initial_movie(self, db_connector: GraphDbConnector):
        game = Game()
        game.set_initial_movie("The Matrix", 1999)
        movie, links = db_connector.get_id_and_links_from_movie_title_and_year(
            "The Matrix", 1999
        )
        assert game.current_movie == movie
        assert game.links_count == {link: 0 for link in links}
        assert game.movies_played == [movie]
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


class TestQueries:
    def test_get_id_from_name(self):
        assert GraphDbConnector.get_id_from_person_name("Tom Cruise") == 500

    def test_get_id_from_name_multiple_results(self):
        with pytest.warns(UserWarning):
            # expect most popular to be returned (Tom Hanks)
            assert GraphDbConnector.get_id_from_person_name("Tom") == 31
