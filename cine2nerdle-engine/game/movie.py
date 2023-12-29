class Movie:
    """
    Defines a basic movie, with a name, an id, a popularity score and a release_year
    """

    def __init__(
        self,
        id: int,
        name: str | None = None,
        popularity: float | None = None,
        release_year: int | None = None,
    ):
        self.id = id
        self.name = name
        self.popularity = float(popularity) if popularity is not None else None
        self.release_year = int(release_year) if release_year is not None else None

    def __str__(self):
        if self.name is None or self.release_year is None:
            return str(self.id)
        return self.name + " (" + str(self.release_year) + ")"

    def __repr__(self):
        return f"Movie({self.name}, {self.id}, {self.popularity}, {self.release_year})"

    def __eq__(self, other):
        return self.id == other.id
