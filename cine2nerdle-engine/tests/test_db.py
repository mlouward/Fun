import pytest

from game.movie import Movie
from game.neo4j_utils.neo4j_utils import GraphDbConnector
from game.person import Person


@pytest.fixture
def db_connector():
    connector: GraphDbConnector = GraphDbConnector.get_default_connection()
    yield connector  # provide the fixture value


@pytest.fixture
def tx(db_connector):
    with db_connector.driver.session() as session:
        yield session.begin_transaction()


class TestGraphDbConnector:
    def test_get_person_from_name(self):
        person = GraphDbConnector.get_person_from_name("Tom Cruise")
        assert person == Person(500, "Tom Cruise", 31)

    def test_get_person_from_name_nonexistant(self):
        with pytest.raises(ValueError):
            GraphDbConnector.get_person_from_name("zzzzzzzzzzzzzz")

    def test_get_person_from_name_multiple_people(self):
        with pytest.warns(UserWarning):
            GraphDbConnector.get_person_from_name("Tom")

    def test_get_movie_from_name_and_year(self, tx):
        movie = GraphDbConnector._get_movie_from_name_and_year(tx, "The Matrix", 1999)
        assert movie.id == 603
        assert movie.name == "The Matrix"
        assert movie.release_year == 1999

    def test_get_movie_from_name_and_year_nonexistant(self, tx):
        with pytest.raises(ValueError):
            GraphDbConnector._get_movie_from_name_and_year(tx, "zzzzzzzzzzzzzz", 1999)

    def test_get_all_links_from_movie(self, tx):
        links = GraphDbConnector._get_all_links_from_movie(tx, Movie(603, "The Matrix", 8.7, 1999))
        assert len(links) == 13
