import warnings

from neo4j import Driver, GraphDatabase

from game.movie import Movie
from game.person import Person


class GraphDbConnector:
    def __init__(self, uri, database, user, password):
        self.driver: Driver = GraphDatabase.driver(uri, auth=(user, password), database=database)
        self.driver.verify_connectivity()
        print("Connected!")

    @staticmethod
    def get_default_connection() -> "GraphDbConnector":
        return GraphDbConnector(
            uri="bolt://localhost:7687",
            user="neo4j",
            password="password",
            database="neo4j",
        )

    def close(self):
        self.driver.close()

    def initialize_database(self) -> None:
        """
        Creates constraints on movie and person ids.
        """
        with self.driver.session() as session:
            session.write_transaction(
                self._initialize_database,
            )

    @staticmethod
    def _initialize_database(tx) -> None:
        """
        Create constraints on movie and person ids
        Args:
            tx: neo4j transaction
        """
        tx.run(
            """
            CREATE CONSTRAINT movie_id_constraint FOR (m:Movie) REQUIRE m.id IS UNIQUE
            """
        )
        tx.run(
            """
            CREATE CONSTRAINT person_id_constraint FOR (p:Person) REQUIRE p.id IS UNIQUE
            """
        )

    def insert_person(self, person: Person) -> None:
        with self.driver.session() as session:
            session.write_transaction(self._insert_person, person)

    @staticmethod
    def _insert_person(tx, person: Person) -> None:
        """Create a person node in the graph database.
        Args:
            tx: neo4j transaction
            person: Person object
        """
        tx.run(
            "MERGE (p:Person {id: $person_id, name: $person_name, popularity: $popularity})",
            person_id=person.id,
            person_name=person.name,
            popularity=person.popularity,
        )

    def insert_movie(self, movie: Movie) -> None:
        with self.driver.session() as session:
            session.write_transaction(self._insert_movie, movie)

    @staticmethod
    def _insert_movie(tx, movie: Movie) -> None:
        """Create a movie node in the graph database.
        Args:
            tx: neo4j transaction
            movie: Movie object
        """
        tx.run(
            "MERGE (m:Movie {id: $movie_id, title: $title, popularity: $popularity, release_year: $release_year, name: $title})",
            movie_id=movie.id,
            title=movie.title,
            popularity=movie.popularity,
            release_year=movie.release_year,
        )

    def insert_person_to_movie(self, person: Person, movie: Movie, role: str, job: str) -> None:
        """
        Create a relationship between a person and a movie.
        Args:
            person_id: person id
            movie_id: movie id
        """
        with self.driver.session() as session:
            session.write_transaction(self._insert_person_to_movie, person, movie, role, job)

    @staticmethod
    def _insert_person_to_movie(tx, person: Person, movie: Movie, role: str, job: str) -> None:
        """Create a relationship between a person and a movie.
        Args:
            tx: neo4j transaction
            person_id: person id
            movie_id: movie id
            role: known_for_department
            job: job description for crew, "actor" for cast
        """
        tx.run(
            """
            MATCH (p:Person {id: $person_id})
            MATCH (m:Movie {id: $movie_id})
            MERGE (p)-[r:APPEARED_IN {role: $role, job: $job}]->(m)
            """,
            person_id=person.id,
            movie_id=movie.id,
            role=role,
            job=job,
        )

    def get_person_from_name(self, name: str) -> Person:
        """
        Get the id of a person from their name.
        Args:
            name: person name
        Returns:
            person id
        """
        with self.driver.session() as session:
            return session.execute_read(self._get_person_from_name, name)

    @staticmethod
    def _get_person_from_name(tx, name: str) -> Person:
        """Get the id of a person from their name, using a case-insensitive partial match.
        If more than one person matches the name, return the first one and show a warning.
        Args:
            tx: neo4j transaction
            name: person name
        Returns:
            person id
        """
        result = tx.run(
            """
            MATCH (p:Person)
            WHERE toLower(p.name) CONTAINS toLower($name)
            RETURN p ORDER BY p.popularity DESC;
            """,
            name=name,
        )
        results = result.data()
        if not results:
            raise ValueError(f"No person found with name {name}")

        if len(results) > 1:
            warnings.warn(f"Multiple people found with name {name}. Using the first one.")
        return Person(**results[0]["p"])

    def get_movie_from_title_and_year(self, movie_title: str, release_date: int) -> Movie:
        """
        Gets a movie from its title and release year.

        If more than one movie matches the title, return the first one and show a warning.

        Args:
            title: movie title
            year: movie release year
        Returns:
            movie
        """
        with self.driver.session() as session:
            return session.execute_read(
                self._get_movie_from_title_and_year, movie_title, release_date
            )

    @staticmethod
    def _get_movie_from_title_and_year(tx, movie_title: str, release_date: int) -> Movie:
        """
        Gets a movie from its title and release year.

        If more than one movie matches the title, return the first one and show a warning.

        Args:
            tx: neo4j transaction
            title: movie title
            year: movie release year
        Returns:
            movie
        """
        movie_result = tx.run(
            """
            MATCH (m:Movie)
            WHERE toLower(m.title) CONTAINS toLower($title)
            AND m.release_year = $year
            RETURN m ORDER BY m.popularity DESC;
            """,
            title=movie_title,
            year=release_date,
        )
        results: list = movie_result.data()
        if not results:
            raise ValueError(f"No movie found with title {movie_title} and year {release_date}.")

        if len(results) > 1:
            warnings.warn(
                f"Multiple movies found with title {movie_title} and year {release_date}. Using the first one."
            )
        return Movie(**results[0]["m"])

    def get_all_links_from_movie(self, movie: Movie) -> list[Person]:
        """
        Get all the person links from a movie.
        Args:
            movie: movie

        Returns:
            list of link persons
        """
        with self.driver.session() as session:
            return session.execute_read(self._get_all_links_from_movie, movie)

    @staticmethod
    def _get_all_links_from_movie(tx, movie: Movie) -> list[Person]:
        """
        Get all the person links from a movie.
        Args:
            tx: neo4j transaction
            movie: movie

        Returns:
            list of link persons
        """
        result = tx.run(
            """
            MATCH (m:Movie {id: $movie_id})-[r:APPEARED_IN]-(p:Person)-[r2:APPEARED_IN]-(m2:Movie)
            WHERE m2.id <> $movie_id
            RETURN DISTINCT p;
            """,
            movie_id=movie.id,
        )

        result = [Person(**record["p"]) for record in result.data()]
        if not result:
            raise ValueError(f"No links found for movie {movie}")

        return result

    def get_used_links_from_movies(self, current_movie: Movie, next_movie: Movie) -> list[Person]:
        """
        Get the ids of links that have been used between two movies.
        Args:
            current_movie_id: movie id
            next_movie_id: movie id
        Returns:
            list of link ids
        """
        with self.driver.session() as session:
            return session.execute_read(self._get_used_links_from_movies, current_movie, next_movie)

    @staticmethod
    def _get_used_links_from_movies(tx, current_movie: Movie, next_movie: Movie) -> list[Person]:
        """
        Get the ids of links that have been used between two movies.
        Args:
            tx: neo4j transaction
            current_movie_id: movie id
            next_movie_id: movie id
        Returns:
            list of link ids
        """
        result = tx.run(
            """
            MATCH (:Movie {id: $current_movie_id})-[:APPEARED_IN]-(p:Person)-[:APPEARED_IN]-(:Movie {id: $next_movie_id})
            RETURN DISTINCT p;
            """,
            current_movie_id=current_movie.id,
            next_movie_id=next_movie.id,
        )

        return [Person(**record["p"]) for record in result.data()]

    def get_possible_movies(
        self, current_movie: Movie, movies_played: list[Movie], usable_links: list[Person]
    ) -> list[Movie]:
        """
        Get the possible movies for the current player.
        Possible movies have a link to the current movie through the APPEARED_IN relationship.
        They cannot be the current movie, or any movie that has already been played.
        A movie is possible if the link has been used less than 3 times.
        Args:
            current_movie: current movie
            movies_played: movies that have already been played
            usable_links: links that have been used less than 3 times
        Returns:
            list of movie ids
        """
        with self.driver.session() as session:
            return session.execute_read(
                self._get_possible_movies, current_movie, movies_played, usable_links
            )

    @staticmethod
    def _get_possible_movies(
        tx, current_movie: Movie, movies_played: list[Movie], usable_links: list[Person]
    ) -> list[Movie]:
        """
        Get the possible movies for the current player.
        """
        result = tx.run(
            """
            MATCH (m:Movie {id: $movie_id})-[:APPEARED_IN]-(p:Person)-[:APPEARED_IN]-(m2:Movie)
            WHERE NOT m2.id IN $movies_played
            AND p.id IN $usable_links
            RETURN DISTINCT m2 ORDER BY m2.popularity ASC LIMIT 5;
            """,
            movie_id=current_movie.id,
            movies_played=[movie.id for movie in movies_played],
            usable_links=[person.id for person in usable_links],
        )

        return [Movie(**record["m2"]) for record in result.data()]
