import pytest

from game.play_game import Player
from neo4j_utils.neo4j_utils import GraphDbConnector


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


class TestQueries:
    def test_get_id_from_name(self):
        assert GraphDbConnector.get_id_from_name("Tom Cruise") == 500

    def test_get_id_from_name_multiple_results(self):
        with pytest.warns(UserWarning):
            # expect most popular to be returned (Tom Hanks)
            assert GraphDbConnector.get_id_from_name("Tom") == 31
