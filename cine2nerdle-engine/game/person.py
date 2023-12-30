class Person:
    """
    Defines a basic person, with a name, an id, a popularity score
    """

    def __init__(self, id: int, name: str | None = None, popularity: float | None = None):
        self.id = id
        self.name = name
        self.popularity = popularity

    def __str__(self):
        return self.name

    def __repr__(self):
        return str(self)

    def __eq__(self, other):
        return self.id == other.id

    def __hash__(self):
        return hash(self.id)
