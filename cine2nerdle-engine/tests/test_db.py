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
    def test_get_person_from_name(self, db_connector):
        person = db_connector.get_person_from_name("Tom Cruise")
        assert person == Person(500, "Tom Cruise", 31)

    def test_get_person_from_name_nonexistant(self, db_connector):
        with pytest.raises(ValueError):
            db_connector.get_person_from_name("zzzzzzzzzzzzzz")

    def test_get_person_from_name_multiple_people(self, db_connector):
        with pytest.warns(UserWarning):
            db_connector.get_person_from_name("Tom")

    def test_get_movie_from_title_and_year(self, tx, db_connector):
        movie = db_connector._get_movie_from_title_and_year(tx, "The Matrix", 1999)
        assert movie.id == 603
        assert movie.title == "The Matrix"
        assert movie.release_year == 1999

    def test_get_movie_from_title_and_year_nonexistant(self, tx, db_connector):
        with pytest.raises(ValueError):
            db_connector._get_movie_from_title_and_year(tx, "zzzzzzzzzzzzzz", 1999)

    def test_get_all_links_from_movie(self, tx):
        links = GraphDbConnector._get_all_links_from_movie(tx, Movie(603, "The Matrix", 8.7, 1999))
        assert len(links) == 13
