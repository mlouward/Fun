import warnings

from neo4j import GraphDatabase


class GraphDbConnector:
    def __init__(self, uri, user, password):
        self.driver = GraphDatabase.driver(uri, auth=(user, password))

    @staticmethod
    def get_default_connection():
        return GraphDbConnector(
            uri="bolt://localhost:7687", user="neo4j", password="password"
        )

    def close(self):
        self.driver.close()

    def initialize_database(self) -> None:
        """
        Creates constraints on movie and person ids.
        Creates indexes on movie title and person name.

        """
        with self.driver.session() as session:
            session.write_transaction(
                self._initialize_database,
            )

    @staticmethod
    def _initialize_database(tx) -> None:
        """
        Create constraints on movie and person ids.
        Create indexes on APPEARED_IN role and job.
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
        tx.run(
            """
            CREATE INDEX appeared_in_role_index FOR ()-[r:APPEARED_IN]-() ON (r.role)
            """
        )
        tx.run(
            """
            CREATE INDEX appeared_in_job_index FOR ()-[r:APPEARED_IN]-() ON (r.job)
            """
        )

    def insert_person(self, person_id: int, person_name: str, popularity: int) -> None:
        with self.driver.session() as session:
            session.write_transaction(
                self._insert_person, person_id, person_name, popularity
            )

    @staticmethod
    def _insert_person(tx, person_id: int, person_name: str, popularity: int) -> None:
        """Create a person node in the graph database.
        Args:
            tx: neo4j transaction
            person_id: person id
            person_name: person name
        """
        tx.run(
            "MERGE (p:Person {id: $person_id, name: $person_name, popularity: $popularity})",
            person_id=person_id,
            person_name=person_name,
            popularity=popularity,
        )

    def insert_movie(
        self, movie_id: int, title: str, popularity: int, release_year: int
    ) -> None:
        with self.driver.session() as session:
            session.write_transaction(
                self._insert_movie, movie_id, title, popularity, release_year
            )

    @staticmethod
    def _insert_movie(
        tx, movie_id: int, title: str, popularity: int, release_year: int
    ) -> None:
        """Create a movie node in the graph database.
        Args:
            tx: neo4j transaction
            movie_id: movie id
            title: movie title
        """
        tx.run(
            "MERGE (m:Movie {id: $movie_id, name: $title, popularity: $popularity, release_year: $release_year})",
            movie_id=movie_id,
            title=title,
            popularity=popularity,
            release_year=release_year,
        )

    def insert_person_to_movie(
        self, person_id: int, movie_id: int, role: str, job: str
    ) -> None:
        """
        Create a relationship between a person and a movie.
        Args:
            person_id: person id
            movie_id: movie id
        """
        with self.driver.session() as session:
            session.write_transaction(
                self._insert_person_to_movie, person_id, movie_id, role, job
            )

    @staticmethod
    def _insert_person_to_movie(
        tx, person_id: int, movie_id: int, role: str, job: str
    ) -> None:
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
            person_id=person_id,
            movie_id=movie_id,
            role=role,
            job=job,
        )

    @staticmethod
    def get_id_from_name(name: str) -> int:
        """
        Get the id of a person from their name.
        Args:
            name: person name
        Returns:
            person id
        """
        with GraphDbConnector.get_default_connection().driver.session() as session:
            return session.execute_read(GraphDbConnector._get_id_from_name, name)

    @staticmethod
    def _get_id_from_name(tx, name: str) -> int:
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
            RETURN p.id AS id, p.popularity AS pop ORDER BY pop DESC;
            """,
            name=name,
        )
        results = result.data()
        if not results:
            raise ValueError(f"No person found with name {name}")

        if len(results) > 1:
            warnings.warn(
                f"Multiple people found with name {name}. Using the first one."
            )
        return results[0]["id"]

    def get_possible_movies(self, movie_id: int) -> list[int]:
        """
        Get the possible movies for the current player.
        Args:
            movie_id: movie id
        Returns:
            list of movie ids
        """
        with self.driver.session() as session:
            return session.execute_read(self._get_possible_movies, movie_id)

    @staticmethod
    def _get_possible_movies(tx, movie_id: int) -> list[int]:
        """Get the possible movies for the current player.
        Args:
            tx: neo4j transaction
            movie_id: movie id
        Returns:
            list of movie ids
        """
        result = tx.run(
            """
            MATCH (m:Movie {id: $movie_id})-[r:APPEARED_IN]-(p:Person)
            RETURN m.id AS id
            """,
            movie_id=movie_id,
        )
        return [record["id"] for record in result.data()]
